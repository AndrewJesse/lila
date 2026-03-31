package lila.puzzle

import chess.format.{ Fen, Uci }
import chess.variant.Chess960
import chess.{ Color, Game as ChessGame, Ply, Pawn }
import lila.core.game.Game

/** Replay helpers to snapshot the board before a given full-move UCI index. */
private[puzzle] object SmartPuzzlePosition:

  def boardPieceStats(b: chess.Board): (Int, Int) =
    val pawnNb = b.count(Color.White, Pawn) + b.count(Color.Black, Pawn)
    (b.nbPieces, b.nbPieces - pawnNb)


  /** @return `(nbPieces, nbNonPawns)` on the board before `ucis(moveIndex)` is played. */
  def boardStatsAt(
      game: Game,
      ucis: Vector[Uci],
      moveIndex: Int,
      initialFen: Option[Fen.Full]
  ): Option[(Int, Int)] =
    if moveIndex < 0 || moveIndex > ucis.size then Option.empty
    else
      ucis.take(moveIndex).foldLeft(Option(initialChessGame(game, initialFen))): (og, u) =>
        og.flatMap: g =>
          g.apply(u).toOption.map(_._1)
      .map: cg =>
        boardPieceStats(cg.position.board)

  private def initialChessGame(game: Game, initialFen: Option[Fen.Full]): ChessGame =
    import chess.Color as ChessColor
    import ChessColor.White

    val variant = game.variant
    val prevPosition = initialFen.flatMap(Fen.readWithMoveNumber(variant, _))
    val newPosition = variant match
      case Chess960 => prevPosition.fold(Chess960.initialPosition)(_.position)
      case v => prevPosition.fold(v.initialPosition)(_.position)
    val ply = prevPosition.fold(Ply.initial)(_.ply)
    val color = prevPosition.fold[ChessColor](White)(_.position.color)
    ChessGame(
      position = newPosition.withColor(color),
      sans = Vector.empty,
      clock = Option.empty,
      ply = ply,
      startedAtPly = ply
    )
