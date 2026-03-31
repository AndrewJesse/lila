package lila.puzzle

import chess.format.Uci
import chess.{ Color, Ply, Speed }

import lila.tree.Advice

/** One half-move row derived from server analysis (fishnet), for Smart Puzzles. */
case class SmartPuzzleMoveRow(
    ply: Ply,
    uci: Uci,
    judgement: Option[Advice.Judgement],
    /** Engine centipawn eval after this move (none if mate / unknown). */
    centipawns: Option[Int],
    /** Board stats before this move was played (for Smart puzzle matching). */
    pieceCount: Option[Int] = Option.empty,
    nonPawnCount: Option[Int] = Option.empty
)

/** Move-by-move analysis snapshot for one finished game. */
case class SmartPuzzleGameRow(
    gameId: GameId,
    /** When this snapshot was written (e.g. when analysis completed). */
    analyzedAt: Instant,
    /** When the game last moved (for “last played” in the UI). */
    playedAt: Instant,
    /** Side the user played in this game. */
    userColor: Color,
    moves: List[SmartPuzzleMoveRow]
)

/** Last [[SmartPuzzleRecentAnalysis.maxGames]] analysed games per [[Speed]], newest first in each bucket. */
case class SmartPuzzleRecentAnalysis(
    userId: UserId,
    bySpeed: Map[Speed, List[SmartPuzzleGameRow]],
    updatedAt: Instant
):
  def normalized: SmartPuzzleRecentAnalysis =
    copy(bySpeed = bySpeed.view.mapValues(_.take(SmartPuzzleRecentAnalysis.maxGames)).toMap)

  def gamesFor(speed: Speed): List[SmartPuzzleGameRow] =
    bySpeed.getOrElse(speed, Nil)

object SmartPuzzleRecentAnalysis:

  val maxGames = 5

  def empty(userId: UserId, at: Instant = nowInstant): SmartPuzzleRecentAnalysis =
    SmartPuzzleRecentAnalysis(userId, Map.empty, at)

  case class BucketSummary(speed: Speed, lastPlayed: Option[Instant], nb: Int)

  /** One row per standard speed, for setup UI (even empty buckets). */
  def bucketSummaries(data: SmartPuzzleRecentAnalysis): List[BucketSummary] =
    Speed.all.map: sp =>
      val games = data.gamesFor(sp)
      BucketSummary(sp, games.headOption.map(_.playedAt), games.size)

  object BSONFields:
    val id = "_id"
    val gamesLegacy = "g"
    val bySpeed = "bs"
    val updatedAt = "ua"

  object GameFields:
    val gameId = "gi"
    val analyzedAt = "a"
    val playedAt = "pt"
    val userWhite = "uw"
    val moves = "m"

  object MoveFields:
    val ply = "p"
    val uci = "u"
    val judgement = "j"
    val centipawns = "c"
    val pieceCount = "pc"
    val nonPawnCount = "np"
