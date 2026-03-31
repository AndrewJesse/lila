package lila.puzzle

import chess.format.{ Fen, Uci }
import chess.rating.glicko.Glicko
import chess.{ Color, Ply, Speed }
import reactivemongo.api.bson.*
import scala.util.{ Success, Try }

import lila.db.BSON
import lila.db.dsl.{ *, given }
import lila.tree.Advice

private object BsonHandlers:

  import Puzzle.BSONFields.*
  import lila.rating.Glicko.glickoHandler

  private[puzzle] given puzzleReader: BSONDocumentReader[Puzzle] with
    def readDocument(r: BSONDocument) = for
      id <- r.getAsTry[PuzzleId](id)
      gameId <- r.getAsTry[GameId](gameId)
      fen <- r.getAsTry[Fen.Full](fen)
      lineStr <- r.getAsTry[String](line)
      line <- lineStr.split(' ').toList.flatMap(Uci.Move.apply).toNel.toTry("Empty move list?!")
      glicko <- r.getAsTry[Glicko](glicko)
      plays <- r.getAsTry[Int](plays)
      vote <- r.getAsTry[Float](vote)
      themes <- r.getAsTry[Set[PuzzleTheme.Key]](themes)
    yield Puzzle(
      id = id,
      gameId = gameId,
      fen = fen,
      line = line,
      glicko = glicko,
      plays = plays,
      vote = vote,
      themes = themes.diff(PuzzleTheme.hiddenThemesKey)
    )

  private[puzzle] given roundIdHandler: BSONHandler[PuzzleRound.Id] = tryHandler[PuzzleRound.Id](
    { case BSONString(v) =>
      v.split(PuzzleRound.idSep) match
        case Array(userId, puzzleId) => Success(PuzzleRound.Id(UserId(userId), PuzzleId(puzzleId)))
        case _ => handlerBadValue(s"Invalid puzzle round id $v")
    },
    id => BSONString(id.toString)
  )

  private[puzzle] given BSONHandler[PuzzleRound.Theme] = tryHandler[PuzzleRound.Theme](
    { case BSONString(v) =>
      PuzzleTheme
        .findAny(v.tail)
        .fold[Try[PuzzleRound.Theme]](handlerBadValue(s"Invalid puzzle round theme $v")) { theme =>
          Success(PuzzleRound.Theme(theme.key, v.head == '+'))
        }
    },
    rt => BSONString(s"${if rt.vote then "+" else "-"}${rt.theme}")
  )

  given roundHandler: BSON[PuzzleRound] with
    import PuzzleRound.BSONFields.*
    def reads(r: BSON.Reader) = PuzzleRound(
      id = r.get[PuzzleRound.Id](id),
      win = r.get[PuzzleWin](win),
      fixedAt = r.dateO(fixedAt),
      date = r.date(date),
      vote = r.intO(vote),
      themes = r.getsD[PuzzleRound.Theme](themes)
    )
    def writes(w: BSON.Writer, r: PuzzleRound) =
      $doc(
        id -> r.id,
        win -> r.win,
        fixedAt -> r.fixedAt,
        date -> r.date,
        vote -> r.vote,
        themes -> w.listO(r.themes)
      )

  import PuzzlePath.given
  private[puzzle] given pathIdHandler: BSONHandler[PuzzlePath.Id] = stringIsoHandler

  import PuzzleAngle.given
  private[puzzle] given BSONHandler[PuzzleAngle] = stringIsoHandler

  private given BSONHandler[Ply] = BSONIntegerHandler.as[Ply](Ply.apply, _.value)

  private given BSONHandler[Uci] = tryHandler[Uci](
    { case BSONString(v) => Uci(v).toTry(s"Bad UCI: $v") },
    x => BSONString(x.uci)
  )

  private given BSONHandler[Advice.Judgement] = tryHandler[Advice.Judgement](
    { case BSONString(v) => Try(Advice.Judgement.valueOf(v)) },
    j => BSONString(j.toString)
  )

  private given BSON[SmartPuzzleMoveRow] with
    import SmartPuzzleRecentAnalysis.MoveFields as F
    def reads(r: BSON.Reader) = SmartPuzzleMoveRow(
      ply = r.get[Ply](F.ply),
      uci = r.get[Uci](F.uci),
      judgement = r.getO[Advice.Judgement](F.judgement),
      centipawns = r.intO(F.centipawns),
      pieceCount = r.intO(F.pieceCount),
      nonPawnCount = r.intO(F.nonPawnCount)
    )
    def writes(w: BSON.Writer, m: SmartPuzzleMoveRow) =
      $doc(
        F.ply -> m.ply,
        F.uci -> m.uci,
        F.judgement -> m.judgement,
        F.centipawns -> m.centipawns,
        F.pieceCount -> m.pieceCount,
        F.nonPawnCount -> m.nonPawnCount
      )

  private given BSON[SmartPuzzleGameRow] with
    import SmartPuzzleRecentAnalysis.GameFields as F
    def reads(r: BSON.Reader) = SmartPuzzleGameRow(
      gameId = r.get[GameId](F.gameId),
      analyzedAt = r.date(F.analyzedAt),
      playedAt = r.dateO(F.playedAt) | r.date(F.analyzedAt),
      userColor = Color.fromWhite(r.bool(F.userWhite)),
      moves = r.getsD[SmartPuzzleMoveRow](F.moves)
    )
    def writes(w: BSON.Writer, g: SmartPuzzleGameRow) =
      $doc(
        F.gameId -> g.gameId,
        F.analyzedAt -> g.analyzedAt,
        F.playedAt -> g.playedAt,
        F.userWhite -> g.userColor.white,
        F.moves -> w.listO(g.moves)
      )

  given smartPuzzleRecentAnalysisHandler: BSON[SmartPuzzleRecentAnalysis] with
    import SmartPuzzleRecentAnalysis.BSONFields as F
    def reads(r: BSON.Reader) =
      val legacyList = r.getsD[SmartPuzzleGameRow](F.gamesLegacy)
      val legacy = Option.when(legacyList.nonEmpty)(legacyList)
      val bySpeed = r.getO[Bdoc](F.bySpeed) match
        case Some(doc) =>
          Speed.all.flatMap: sp =>
            doc.getAsOpt[List[SmartPuzzleGameRow]](sp.key.value).map(sp -> _)
          .toMap
        case None =>
          legacy.fold(Map.empty[Speed, List[SmartPuzzleGameRow]])(g => Map(Speed.Blitz -> g))
      SmartPuzzleRecentAnalysis(
        userId = r.get[UserId](F.id),
        bySpeed = bySpeed,
        updatedAt = r.date(F.updatedAt)
      )
    def writes(w: BSON.Writer, s: SmartPuzzleRecentAnalysis) =
      $doc(
        F.id -> s.userId,
        F.bySpeed -> $doc:
          s.bySpeed.toList.flatMap: (sp, rows) =>
            w.listO(rows).map(sp.key.value -> _).toList,
        F.updatedAt -> s.updatedAt
      )
