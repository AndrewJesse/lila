package lila.puzzle

import lila.db.BSON
import lila.db.dsl.{ *, given }

/** Persists per-user rolling analysis of the last [[SmartPuzzleRecentAnalysis.maxGames]] games (Smart Puzzles). */
final class SmartPuzzleRecentAnalysisRepo(colls: PuzzleColls)(using Executor):

  import BsonHandlers.given

  lila.common.Bus.sub[lila.core.user.UserDelete]: del =>
    coll(_.delete.one($id(del.id))).void

  private def coll = colls.smartRecent

  def byUser(id: UserId): Fu[Option[SmartPuzzleRecentAnalysis]] =
    coll(_.byId[SmartPuzzleRecentAnalysis](id))

  def upsert(data: SmartPuzzleRecentAnalysis): Funit =
    val normalized = data.normalized
    val doc = summon[BSON[SmartPuzzleRecentAnalysis]].write(normalized)
    coll(_.update.one($id(normalized.userId), doc, upsert = true)).void
