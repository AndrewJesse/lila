package lila.puzzle

import lila.db.dsl.{ *, given }

final class PuzzleSelector(
    colls: PuzzleColls,
    pathApi: PuzzlePathApi,
    sessionApi: PuzzleSessionApi,
    anon: PuzzleAnon,
    smartFinder: SmartPuzzleFinder
)(using Executor):

  import BsonHandlers.given

  private enum NextPuzzleResult(val name: String):
    case PathMissing extends NextPuzzleResult("pathMissing")
    case PathEnded extends NextPuzzleResult("pathEnded")
    case WrongColor(puzzle: Puzzle) extends NextPuzzleResult("wrongColor")
    case PuzzleMissing(id: PuzzleId) extends NextPuzzleResult("puzzleMissing")
    case PuzzleAlreadyPlayed(puzzle: Puzzle) extends NextPuzzleResult("puzzlePlayed")
    case PuzzleFound(puzzle: Puzzle) extends NextPuzzleResult("puzzleFound")

  def nextPuzzleFor(
      angle: PuzzleAngle,
      color: Option[Option[Color]],
      difficulty: Option[PuzzleDifficulty],
      smartMatchBoard: Option[Boolean] = none
  )(using me: Option[Me], perf: Perf): Fu[Option[Puzzle]] =
    me match
      case Some(me) =>
        given Me = me
        for
          _ <- difficulty.so(sessionApi.setDifficulty)
          _ <- smartMatchBoard.so(sessionApi.setSmartMatchBoard)
          _ <- color.so(sessionApi.setAngleAndColor(angle, _))
          puz <- nextPuzzleFor(angle)
        yield puz
      case None =>
        val forAnon = if angle == PuzzleAngle.Smart then PuzzleAngle.mix else angle
        anon.getOneFor(forAnon, difficulty | PuzzleDifficulty.Normal, ~color)

  def nextPuzzleFor(angle: PuzzleAngle)(using Me, Perf): Fu[Option[Puzzle]] =
    findNextPuzzleFor(angle, 0)
      .fold(
        err =>
          logger.warn(s"PuzzleSelector.nextPuzzleFor ${err.getMessage}")
          none
        ,
        some
      )
      .mon(_.puzzle.selector.user.time(angle.categ))

  private def pathAngleFor(a: PuzzleAngle) = a match
    case PuzzleAngle.Smart => PuzzleAngle.mix
    case x => x

  private def findNextPuzzleFor(angle: PuzzleAngle, retries: Int)(using me: Me, perf: Perf): Fu[Puzzle] =
    sessionApi
      .continueOrCreateSessionFor(angle, canFlush = retries == 0)
      .flatMap: session =>
        import NextPuzzleResult.*

        def switchPath(reason: String)(withRetries: Int)(tier: PuzzleTier) =
          pathApi
            .nextFor(s"switchPath.$reason")(
              pathAngleFor(angle),
              tier,
              session.settings.difficulty,
              session.previousPaths
            )
            .orFail(s"No puzzle path for selection ${me.username} $angle $tier")
            .flatMap: pathId =>
              val newSession = session.switchTo(pathId)
              sessionApi.set(newSession)
              findNextPuzzleFor(angle, retries = withRetries + 1)

        def serveAndMonitor(puzzle: Puzzle) =
          val mon = lila.mon.puzzle.selector.user
          mon.retries(angle.categ).record(retries)
          mon.vote.record(100 + math.round(puzzle.vote * 100))
          mon.tier(session.path.tier.key, angle.categ, session.settings.difficulty.key).increment()
          puzzle

        def handle(result: NextPuzzleResult): Fu[Puzzle] =
          result match
            case PathMissing if retries < 10 => switchPath("missing")(retries)(session.path.tier)
            case PathMissing => fufail(s"Puzzle path missing for ${me.username} $session")
            case PathEnded if retries < 10 => switchPath("ended")(retries)(session.path.tier)
            case PathEnded => fufail(s"Puzzle path ended for ${me.username} $session")
            case PuzzleMissing(id) =>
              logger.warn(s"Puzzle missing: $id")
              sessionApi.set(session.next)
              findNextPuzzleFor(angle, retries + 1)
            case PuzzleAlreadyPlayed(_) if retries < 5 =>
              sessionApi.set(session.next)
              findNextPuzzleFor(angle, retries = retries + 1)
            case PuzzleAlreadyPlayed(puzzle) =>
              session.path.tier.stepDown.fold(fuccess(serveAndMonitor(puzzle)))(switchPath("played")(retries))
            case WrongColor(_) if retries < 10 =>
              sessionApi.set(session.next)
              findNextPuzzleFor(angle, retries = retries + 1)
            case WrongColor(puzzle) =>
              session.path.tier.stepDown
                .fold(fuccess(serveAndMonitor(puzzle)))(switchPath("wrongColor")(retries - 5))
            case PuzzleFound(puzzle) => fuccess(serveAndMonitor(puzzle))

        if session.playAngle.contains(PuzzleAngle.Smart) then
          nextSmartPuzzleResult(session).flatMap:
            case PathMissing if retries < 8 =>
              sessionApi.set(session.copy(playAngle = None))
              findNextPuzzleFor(PuzzleAngle.mix, retries + 1)
            case other => handle(other)
        else nextPuzzleResult(session).flatMap(handle)

  private def nextSmartPuzzleResult(session: PuzzleSession)(using me: Me, perf: Perf): Fu[NextPuzzleResult] =
    import NextPuzzleResult.*
    smartFinder.next(session, Set.empty).flatMap:
      case None => fuccess(PathMissing)
      case Some(puzzle) =>
        colls.round(_.exists($id(PuzzleRound.Id(me.userId, puzzle.id).toString))).map: played =>
          if session.settings.color.exists(puzzle.color !=) then WrongColor(puzzle)
          else if played then PuzzleAlreadyPlayed(puzzle)
          else PuzzleFound(puzzle)

  /** For mobile batch: successive Smart puzzles without repeating the same id in the batch. */
  def nextSmartPuzzleExcluding(exclude: Set[PuzzleId])(using me: Me, perf: Perf): Fu[Option[Puzzle]] =
    sessionApi.continueOrCreateSessionFor(PuzzleAngle.Smart, false).flatMap: session =>
      if !session.playAngle.contains(PuzzleAngle.Smart) then fuccess(none)
      else
        smartFinder.next(session, exclude).flatMap:
          case None => fuccess(none)
          case Some(puzzle) =>
            colls.round(_.exists($id(PuzzleRound.Id(me.userId, puzzle.id).toString))).map: played =>
              if session.settings.color.exists(puzzle.color !=) then none
              else if played then none
              else puzzle.some

  private def nextPuzzleResult(session: PuzzleSession)(using me: Me): Fu[NextPuzzleResult] =
    colls
      .path:
        _.aggregateOne(): framework =>
          import framework.*
          Match($id(session.path)) -> List(
            // get the puzzle ID from session position
            Project($doc("puzzleId" -> $doc("$arrayElemAt" -> $arr("$ids", session.positionInPath)))),
            Project:
              $doc(
                "puzzleId" -> true,
                "roundId" -> $doc("$concat" -> $arr(s"${me.userId}${PuzzleRound.idSep}", "$puzzleId"))
              )
            ,
            // fetch the puzzle
            PipelineOperator:
              $doc:
                "$lookup" -> $doc(
                  "from" -> colls.puzzle.name.value,
                  "localField" -> "puzzleId",
                  "foreignField" -> "_id",
                  "as" -> "puzzle"
                )
            ,
            // look for existing round
            PipelineOperator:
              $doc:
                "$lookup" -> $doc(
                  "from" -> colls.round.name.value,
                  "localField" -> "roundId",
                  "foreignField" -> "_id",
                  "as" -> "round"
                )
          )
      .map: docOpt =>
        import NextPuzzleResult.*
        docOpt.fold[NextPuzzleResult](PathMissing): doc =>
          doc
            .getAsOpt[PuzzleId]("puzzleId")
            .fold[NextPuzzleResult](PathEnded): puzzleId =>
              doc
                .getAsOpt[List[Puzzle]]("puzzle")
                .flatMap(_.headOption)
                .fold[NextPuzzleResult](PuzzleMissing(puzzleId)): puzzle =>
                  if session.settings.color.exists(puzzle.color !=) then WrongColor(puzzle)
                  else if doc.getAsOpt[List[Bdoc]]("round").exists(_.nonEmpty) then
                    PuzzleAlreadyPlayed(puzzle)
                  else PuzzleFound(puzzle)
      .monValue: result =>
        _.puzzle.selector.nextPuzzleResult(result.name)
