package lila.puzzle

import chess.{ IntRating, Speed }

import lila.memo.CacheApi

private case class PuzzleSession(
    settings: PuzzleSettings,
    path: PuzzlePath.Id,
    positionInPath: Int,
    rating: IntRating,
    previousPaths: Set[PuzzlePath.Id] = Set.empty,
    /** When set, puzzles are chosen outside the path (e.g. [[PuzzleAngle.Smart]]). */
    playAngle: Option[PuzzleAngle] = None
):
  def effectiveAngle: PuzzleAngle = playAngle | path.angle

  def switchTo(pathId: PuzzlePath.Id) = copy(
    path = pathId,
    previousPaths = previousPaths + pathId,
    positionInPath = 0
  )
  def forward(nb: Int) = copy(positionInPath = positionInPath + nb)
  def next = forward(1)

  def brandNew = positionInPath == 0

  def similarTo(other: PuzzleSession) =
    effectiveAngle == other.effectiveAngle &&
      settings.difficulty == other.settings.difficulty &&
      settings.color == other.settings.color &&
      settings.smartMatchBoard == other.settings.smartMatchBoard &&
      settings.smartSpeed == other.settings.smartSpeed

  override def toString = s"$path:$positionInPath($settings)"

case class PuzzleSettings(
    difficulty: PuzzleDifficulty,
    color: Option[Color],
    /** When true (default), Smart puzzles prefer similar material to your mistake positions. */
    smartMatchBoard: Boolean = true,
    smartSpeed: Option[Speed] = none
)
object PuzzleSettings:
  val default = PuzzleSettings(PuzzleDifficulty.default, none, smartMatchBoard = true, smartSpeed = none)
  def default(color: Option[Color]) =
    PuzzleSettings(PuzzleDifficulty.default, color, smartMatchBoard = true, smartSpeed = none)

final class PuzzleSessionApi(pathApi: PuzzlePathApi, cacheApi: CacheApi)(using Executor):

  def onComplete(userId: UserId, angle: PuzzleAngle, nb: Int = 1): Funit =
    sessions
      .getIfPresent(userId)
      .so:
        _.map: session =>
          // yes, even if the completed puzzle was not the current session puzzle
          // in that case we just skip a puzzle on the path, which doesn't matter
          if session.effectiveAngle == angle then sessions.put(userId, fuccess(session.forward(nb)))

  def getSettings(user: User): Fu[PuzzleSettings] =
    sessions
      .getIfPresent(user.id)
      .fold[Fu[PuzzleSettings]](fuccess(PuzzleSettings.default))(_.dmap(_.settings))

  def setDifficulty(difficulty: PuzzleDifficulty)(using Me, Perf): Funit =
    updateSession: prev =>
      prev
        .forall(_.settings.difficulty != difficulty)
        .option(
          createSessionFor("difficulty")(
            prev.map(_.effectiveAngle) | PuzzleAngle.mix,
            PuzzleSettings(
              difficulty,
              prev.flatMap(_.settings.color),
              prev.fold(PuzzleSettings.default.smartMatchBoard)(_.settings.smartMatchBoard),
              prev.flatMap(_.settings.smartSpeed)
            )
          )
        )

  private[puzzle] def setAngleAndColor(angle: PuzzleAngle, color: Option[Color])(using Me, Perf): Funit =
    updateSession: prev =>
      prev
        .forall(p => p.settings.color != color || p.effectiveAngle != angle)
        .option(
          createSessionFor("angle")(
            angle,
            PuzzleSettings(
              prev.fold(PuzzleDifficulty.default)(_.settings.difficulty),
              color,
              prev.fold(PuzzleSettings.default.smartMatchBoard)(_.settings.smartMatchBoard),
              prev.flatMap(_.settings.smartSpeed)
            )
          )
        )

  def setSmartSpeed(speed: Speed)(using me: Me, perf: Perf): Funit =
    updateSession: prev =>
      if prev.exists(_.settings.smartSpeed.contains(speed)) then none
      else
        createSessionFor("smartSpeed")(
          prev.map(_.effectiveAngle) | PuzzleAngle.mix,
          PuzzleSettings(
            prev.fold(PuzzleDifficulty.default)(_.settings.difficulty),
            prev.flatMap(_.settings.color),
            prev.fold(PuzzleSettings.default.smartMatchBoard)(_.settings.smartMatchBoard),
            speed.some
          )
        ).some

  def setSmartMatchBoard(enabled: Boolean)(using me: Me, perf: Perf): Funit =
    updateSession: prev =>
      if prev.exists(_.settings.smartMatchBoard == enabled) then none
      else
        createSessionFor("smartMatchBoard")(
          prev.map(_.effectiveAngle) | PuzzleAngle.mix,
          PuzzleSettings(
            prev.fold(PuzzleDifficulty.default)(_.settings.difficulty),
            prev.flatMap(_.settings.color),
            enabled,
            prev.flatMap(_.settings.smartSpeed)
          )
        ).some

  private[puzzle] def set(session: PuzzleSession)(using me: Me) = sessions.put(me.userId, fuccess(session))

  private def updateSession(f: Option[PuzzleSession] => Option[Fu[PuzzleSession]])(using me: Me): Funit =
    sessions
      .getIfPresent(me.userId)
      .fold(fuccess(none[PuzzleSession]))(_.dmap(some))
      .flatMap: prev =>
        f(prev).so:
          _.map: next =>
            (!prev.exists(next.similarTo)).so(sessions.put(me.userId, fuccess(next)))

  private val sessions = cacheApi.notLoading[UserId, PuzzleSession](16_384, "puzzle.session"):
    _.expireAfterWrite(1.hour).buildAsync()

  private[puzzle] def continueOrCreateSessionFor(
      angle: PuzzleAngle,
      canFlush: Boolean
  )(using me: Me, perf: Perf): Fu[PuzzleSession] =
    sessions
      .getFuture(me.userId, _ => createSessionFor("miss")(angle, PuzzleSettings.default))
      .flatMap: current =>
        val reCreateReason =
          if current.effectiveAngle != angle then "wrongAngle".some
          else if canFlush && shouldFlushSession(current) then "flush".some
          else none
        reCreateReason match
          case Some(reason) =>
            createSessionFor(reason)(angle, current.settings).tap { sessions.put(me.userId, _) }
          case None => fuccess(current)

  // renew the session often for provisional players
  private def shouldFlushSession(session: PuzzleSession)(using perf: Perf) = !session.brandNew && {
    Math.abs((perf.intRating - session.rating).value) > 100
  }

  private def createSessionFor(reason: String)(angle: PuzzleAngle, settings: PuzzleSettings)(using
      me: Me,
      perf: Perf
  ): Fu[PuzzleSession] =
    val validSettings =
      if angle.opening.isDefined then settings
      else settings.copy(color = none) // only opening sessions can have a color choice
    val pathAngle = angle match
      case PuzzleAngle.Smart => PuzzleAngle.mix
      case a => a
    val playAngle = Option.when(angle == PuzzleAngle.Smart)(PuzzleAngle.Smart)
    pathApi
      .nextFor(s"session.$reason")(pathAngle, PuzzleTier.top, validSettings.difficulty, Set.empty)
      .orFail(s"No puzzle path found for ${me.username}, angle: $angle")
      .map: pathId =>
        PuzzleSession(validSettings, pathId, 0, perf.intRating, playAngle = playAngle)
