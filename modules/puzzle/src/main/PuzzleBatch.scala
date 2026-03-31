package lila.puzzle

import lila.db.dsl.*

// mobile app
final class PuzzleBatch(
    colls: PuzzleColls,
    anonApi: PuzzleAnon,
    pathApi: PuzzlePathApi,
    selector: PuzzleSelector,
    sessionApi: PuzzleSessionApi
)(using Executor):

  import BsonHandlers.given

  def nextForMe(difficulty: PuzzleDifficulty, nb: Int)(using Option[Me], Perf): Fu[Vector[Puzzle]] =
    nextForMe(PuzzleAngle.mix, PuzzleSettings.default.copy(difficulty = difficulty, color = none), nb)

  def nextForMe(
      angle: PuzzleAngle,
      settings: PuzzleSettings,
      nb: Int
  )(using me: Option[Me], perf: Perf): Fu[Vector[Puzzle]] =
    import settings.*
    if nb < 1 then fuccess(Vector.empty)
    else if nb == 1 then selector.nextPuzzleFor(angle, color.map(some), difficulty.some).map(_.toVector)
    else if angle == PuzzleAngle.Smart then
      me match
        case None => anonApi.getBatchFor(PuzzleAngle.mix, difficulty, nb)
        case Some(m) =>
          given Me = m
          sessionApi.setDifficulty(settings.difficulty).flatMap: _ =>
            def step(ex: Set[PuzzleId], left: Int, acc: Vector[Puzzle], guard: Int): Fu[Vector[Puzzle]] =
              if left == 0 || guard > 100 then fuccess(acc)
              else
                selector.nextSmartPuzzleExcluding(ex).flatMap:
                  case None => fuccess(acc)
                  case Some(p) =>
                    if ex.contains(p.id) then step(ex, left, acc, guard + 1)
                    else step(ex + p.id, left - 1, acc :+ p, guard + 1)
            step(Set.empty, nb, Vector.empty, 0)
    else // todo honor color for batch, maybe
      me.foldUse(anonApi.getBatchFor(angle, difficulty, nb)): me ?=>
        val tier =
          if perf.nb > 5000 then PuzzleTier.good
          else if angle.opening.isDefined then PuzzleTier.good
          else if PuzzleDifficulty.isExtreme(difficulty) then PuzzleTier.good
          else PuzzleTier.top
        pathApi
          .nextFor("batch")(angle, tier, difficulty, Set.empty)
          .orFail(s"No puzzle path for batch ${me.username} $angle $tier")
          .flatMap: pathId =>
            colls.path:
              _.aggregateList(nb): framework =>
                import framework.*
                Match($id(pathId)) -> List(
                  Project($doc("puzzleId" -> "$ids", "_id" -> false)),
                  Unwind("puzzleId"),
                  Sample(nb),
                  PipelineOperator:
                    $lookup.simple(
                      from = colls.puzzle.name,
                      local = "puzzleId",
                      foreign = "_id",
                      as = "puzzle",
                      pipe = Nil
                    )
                  ,
                  PipelineOperator:
                    $doc("$replaceWith" -> $doc("$arrayElemAt" -> $arr("$puzzle", 0)))
                )
              .map:
                _.view.flatMap(puzzleReader.readOpt).toVector
          .mon(_.puzzle.selector.user.batch(nb = nb))
