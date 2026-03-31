package lila.puzzle

import chess.format.Fen
import chess.Speed

import lila.db.dsl.{ *, given }
import lila.tree.Advice

/** Picks puzzles whose themes align with judgements from [[SmartPuzzleRecentAnalysis]]. */
final class SmartPuzzleFinder(
    colls: PuzzleColls,
    smartRepo: SmartPuzzleRecentAnalysisRepo,
    gameRepo: lila.core.game.GameRepo
)(using Executor):

  import Advice.Judgement as J
  import BsonHandlers.given

  private case class BoardTarget(weight: Double, pieces: Int, nonPawns: Int)

  /** Slightly favour the two newest games among the last five in the chosen speed bucket. */
  private val recentGameWeight = 1.28
  private val olderGameWeight = 1.0

  private def gameWeight(gameIndex: Int): Double =
    if gameIndex < 2 then recentGameWeight else olderGameWeight

  /** Uses all analysed games in the bucket (including a single game). At most [[SmartPuzzleRecentAnalysis.maxGames]] are stored per speed. HTTP handlers redirect when the bucket is empty so users pick a time control on the setup page first. */
  def next(session: PuzzleSession, exclude: Set[PuzzleId] = Set.empty)(using me: Me, perf: Perf): Fu[Option[Puzzle]] =
    val center = perf.intRating.value
    val matchBoard = session.settings.smartMatchBoard
    val speed = session.settings.smartSpeed | Speed.Blitz

    smartRepo.byUser(me.userId).flatMap: dataOpt =>
      val games = dataOpt.fold(List.empty[SmartPuzzleGameRow])(_.gamesFor(speed))
      val themeKeys = themesFromGames(games)
      val targets = boardTargetsFrom(games)
      tryRatingFlex(center, themeKeys, exclude, matchBoard, targets, flex = 150)

  private def tryRatingFlex(
      center: Int,
      themeKeys: List[PuzzleTheme.Key],
      exclude: Set[PuzzleId],
      matchBoard: Boolean,
      targets: List[BoardTarget],
      flex: Int
  )(using Me): Fu[Option[Puzzle]] =
    val rMin = (center - flex).atLeast(400)
    val rMax = (center + flex).atMost(3500)
    tryPick(themeKeys, rMin, rMax, exclude, matchBoard, targets, 0, none).flatMap:
      case None if flex < 400 =>
        tryRatingFlex(center, themeKeys, exclude, matchBoard, targets, flex + 65)
      case res => fuccess(res)

  private def tryPick(
      themeKeys: List[PuzzleTheme.Key],
      rMin: Int,
      rMax: Int,
      exclude: Set[PuzzleId],
      matchBoard: Boolean,
      targets: List[BoardTarget],
      attempt: Int,
      best: Option[(Puzzle, Double)]
  )(using Me): Fu[Option[Puzzle]] =
    val maxAttempts = if matchBoard && targets.nonEmpty then 26 else 16
    if attempt > maxAttempts then fuccess(best.map(_._1))
    else
      sampleOne(themeKeys, rMin, rMax, exclude).flatMap:
        case None => tryPick(themeKeys, rMin, rMax, exclude, matchBoard, targets, attempt + 1, best)
        case Some(p) =>
          val score =
            if matchBoard && targets.nonEmpty then boardMatchScore(p, targets)
            else 1.0
          val merged = best.fold((p, score).some): b =>
            if score > b._2 then (p, score).some else b.some
          val goodEnough = !matchBoard || targets.isEmpty || score >= 0.54
          if goodEnough then fuccess(p.some)
          else tryPick(themeKeys, rMin, rMax, exclude, matchBoard, targets, attempt + 1, merged)

  private def sampleOne(
      themeKeys: List[PuzzleTheme.Key],
      rMin: Int,
      rMax: Int,
      exclude: Set[PuzzleId]
  )(using Me): Fu[Option[Puzzle]] =
    colls.puzzle:
      _.aggregateOne(_.pri): framework =>
        import framework.*
        val baseMatch =
          $doc(
            "themes".$in(themeKeys),
            "glicko.r".$inRange(rMin -> rMax)
          ) ++ exclude.nonEmpty.so($doc("_id".$nin(exclude)))
        // Source game must exist: [[GameJson]] needs game5 for PGN / tree (avoids 500 on thin DBs).
        Match(baseMatch) -> List(
          PipelineOperator($lookup.simple(gameRepo.coll, as = "preGame", local = "gameId", foreign = "_id")),
          Match("preGame.0".$exists(true)),
          Sample(1)
        )
    .flatMap:
      case Some(doc) =>
        doc.asOpt[Puzzle] match
          case Some(p) => fuccess(p.some)
          case None => fuccess(none)
      case None => fuccess(none)

  private def puzzleBoardSig(p: Puzzle): Option[(Int, Int)] =
    Fen.read(p.fen).map: sit =>
      SmartPuzzlePosition.boardPieceStats(sit.board)

  private def boardMatchScore(p: Puzzle, targets: List[BoardTarget]): Double =
    puzzleBoardSig(p) match
      case None => 0.0
      case Some((pp, pnp)) =>
        val tw = targets.map(_.weight).sum
        if tw <= 0 then 0.0
        else
          targets.foldLeft(0.0): (acc, t) =>
            acc + t.weight * grade(pp, t.pieces) * grade(pnp, t.nonPawns)
          / tw

  private def grade(a: Int, b: Int): Double =
    math.exp(-math.abs(a - b) / 3.6)

  private def defaultThemes: List[PuzzleTheme.Key] =
    List(
      PuzzleTheme.fork,
      PuzzleTheme.middlegame,
      PuzzleTheme.pin,
      PuzzleTheme.mateIn2
    ).map(_.key)

  private def themesFromGames(games: List[SmartPuzzleGameRow]): List[PuzzleTheme.Key] =
    var blunders = 0.0
    var mistakes = 0.0
    var inaccuracies = 0.0
    games.zipWithIndex.foreach: (g, gi) =>
      val gw = gameWeight(gi)
      g.moves.foreach: row =>
        row.judgement.foreach:
          case J.Blunder => blunders += gw
          case J.Mistake => mistakes += gw
          case J.Inaccuracy => inaccuracies += gw
    val total = blunders + mistakes + inaccuracies
    if total == 0 then defaultThemes
    else
      val blunderThemes =
        List(PuzzleTheme.fork, PuzzleTheme.hangingPiece, PuzzleTheme.discoveredAttack, PuzzleTheme.skewer)
      val mistakeThemes =
        List(PuzzleTheme.pin, PuzzleTheme.deflection, PuzzleTheme.trappedPiece, PuzzleTheme.interference)
      val inaccuracyThemes =
        List(PuzzleTheme.quietMove, PuzzleTheme.defensiveMove, PuzzleTheme.endgame, PuzzleTheme.middlegame)
      val scored = List(
        (blunders / total, blunderThemes),
        (mistakes / total, mistakeThemes),
        (inaccuracies / total, inaccuracyThemes)
      ).sortBy(-_._1)
      val primary = scored.head._2.map(_.key)
      val secondary = scored.lift(1).filter(_._1 > 0.15).so(_._2.map(_.key))
      (primary ++ secondary).distinct.take(8)

  private def boardTargetsFrom(games: List[SmartPuzzleGameRow]): List[BoardTarget] =
    games.zipWithIndex.toList.flatMap: (g, gi) =>
      val gw = gameWeight(gi)
      g.moves.toList.flatMap: m =>
        if m.judgement.isEmpty then Nil
        else
          (m.pieceCount, m.nonPawnCount) match
            case (Some(pc), Some(np)) => List(BoardTarget(gw, pc, np))
            case _ => Nil
