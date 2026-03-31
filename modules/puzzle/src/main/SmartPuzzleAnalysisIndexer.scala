package lila.puzzle

import chess.format.Uci
import chess.{ Color, Ply, Speed }

import lila.core.game.{ Game, GameRepo }
import lila.tree.{ Analysis, Advice }

/** On fishnet completion, stores per-move judgements and evals for each human player (last 5 games). */
final class SmartPuzzleAnalysisIndexer(
    repo: SmartPuzzleRecentAnalysisRepo,
    uciMemo: lila.core.game.UciMemo,
    gameRepo: GameRepo
)(using Executor):

  private val logger = lila.log("smartPuzzleAnalysis")

  /** Use `_root_.lila` so this resolves even if `lila` is shadowed in nested scopes. */
  _root_.lila.common.Bus.sub[_root_.lila.analyse.actorApi.AnalysisReady]:
    case _root_.lila.analyse.actorApi.AnalysisReady(game, analysis) =>
      process(game, analysis).recover { case e: Exception =>
        logger.warn(s"game ${game.id} ${e.getMessage}", e)
      }

  def process(game: Game, analysis: Analysis): Funit =
    if !analysis.valid || game.sans.isEmpty then funit
    else
      uciMemo.get(game).flatMap: uciStrs =>
        if uciStrs.size != game.sans.size then
          logger.warn(s"game ${game.id} uci/san size mismatch ${uciStrs.size} vs ${game.sans.size}")
          funit
        else
          parseUcis(uciStrs) match
            case None =>
              logger.warn(s"game ${game.id} invalid UCI in smart puzzle indexer")
              funit
            case Some(ucis) =>
              gameRepo.initialFen(game).flatMap: initialFen =>
                val advices = analysis.advices.mapBy(_.info.ply)
                val infoByPly = analysis.infos.mapBy(_.ply)

                inline def moverAt(index: Int): Color =
                  if index % 2 == 0 then game.startColor else !game.startColor

                inline def plyAfter(index: Int): Ply =
                  game.startedAtPly + (index + 1)

                def rowsFor(color: Color): List[SmartPuzzleMoveRow] =
                  ucis.indices.iterator
                    .filter(moverAt(_) == color)
                    .map: i =>
                      val ply = plyAfter(i)
                      val judgement: Option[Advice.Judgement] =
                        advices
                          .get(ply)
                          .filter(_.color == color)
                          .map(_.judgment)
                      val centipawns =
                        infoByPly
                          .get(ply)
                          .flatMap(_.cp)
                          .map(_.ceiled.centipawns)
                      val sig = SmartPuzzlePosition.boardStatsAt(game, ucis, i, initialFen)
                      SmartPuzzleMoveRow(
                        ply,
                        ucis(i),
                        judgement,
                        centipawns,
                        pieceCount = sig.map(_._1),
                        nonPawnCount = sig.map(_._2)
                      )
                    .toList

                val speed = game.speed
                val jobs = Color.all.toList.flatMap: c =>
                  game.player(c).userId.map: uid =>
                    val row =
                      SmartPuzzleGameRow(
                        game.id,
                        nowInstant,
                        game.movedAt,
                        c,
                        rowsFor(c)
                      )
                    merge(uid, speed, row)

                jobs.sequentiallyVoid(identity)

  private def parseUcis(strs: Vector[String]): Option[Vector[Uci]] =
    strs.foldLeft(Option(Vector.empty[Uci])) { case (acc, s) =>
      acc.flatMap(vec => Uci(s).map(vec :+ _))
    }

  private def merge(userId: UserId, speed: Speed, newGame: SmartPuzzleGameRow): Funit =
    repo.byUser(userId).flatMap: prevOpt =>
      val prev = prevOpt | SmartPuzzleRecentAnalysis.empty(userId)
      val bucket = prev.gamesFor(speed).filterNot(_.gameId == newGame.gameId)
      val merged = (newGame :: bucket).take(SmartPuzzleRecentAnalysis.maxGames)
      val nextBySpeed = prev.bySpeed.updated(speed, merged)
      repo.upsert(prev.copy(bySpeed = nextBySpeed, updatedAt = nowInstant))
