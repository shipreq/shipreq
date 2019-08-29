package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq._
import java.time.{Duration, Instant}
import scala.util.{Failure, Success}
import scalaz.syntax.monad._
import scalaz.{-\/, BindRec, Monad, \/, \/-, ~>}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.data.{Obfuscated, Project, ProjectId, ProjectMetaData}
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.event.EventOrd.Implicits._
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.EventResult
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.user.{User, Username}

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def initPage(projectId: ProjectId, username: Username): F[ProjectSpaEntryPoint.InitData]

  def onConnect(cookies  : Cookie.LookupFn,
                projectId: ProjectId.Public): F[ConnectRejection \/ (WebSocketStatic, WebSocketState[F])]

  def onOpen(static: WebSocketStatic,
             state : WebSocketState[F],
             push  : BinaryData => F[Unit]): F[WebSocketState[F]]

  def onMessage(static : WebSocketStatic,
                state  : WebSocketState[F],
                msg    : BinaryData,
                respond: BinaryData => F[Throwable \/ Unit],
                push   : BinaryData => F[Unit],
                onError: MsgError => F[Unit]): F[Option[WebSocketState[F]]]

  // Option is used because this is called after onConnect rejection
  // (in which case valid values are never created for the session)
  def onClose(static: Option[WebSocketStatic],
              state : Option[WebSocketState[F]]): F[Unit]
}

object ProjectSpaLogic extends StrictLogging {

  final case class Config(eventSnapshotCacheRatio: Int,
                          writeSnapshots: Boolean,
                          writeEvents: Boolean) {
    assert(writeSnapshots || writeEvents, "Can't disable both snapshot & event writing.")
  }

  object Config {
    import japgolly.clearconfig._
    import scalaz.syntax.applicative._

    def default = Config(100, true, true) // see calculations.ods

    def defn: ConfigDef[Config] =
      ( ConfigDef.getOrUse("eventSnapshotCacheRatio", default.eventSnapshotCacheRatio) |@|
        ConfigDef.getOrUse("writeSnapshots", default.writeSnapshots) |@|
        ConfigDef.getOrUse("writeEvents", default.writeEvents)
      )(apply)
  }

  final case class WebSocketStatic(user       : User,
                                   projectId  : ProjectId,
                                   span       : Any,
                                   connectedAt: Instant)

  final case class WebSocketState[F[_]](sub: Option[Redis.Subscription[F]])
  object WebSocketState {
    def empty[F[_]] = apply[F](None)
  }

  sealed trait ConnectRejection
  object ConnectRejection {
    case object NoSession        extends ConnectRejection
    case object AnonymousSession extends ConnectRejection
    case object InvalidProjectId extends ConnectRejection
    case object ProjectNotFound  extends ConnectRejection
    case object AccessDenied     extends ConnectRejection
    implicit def univEq: UnivEq[ConnectRejection] = UnivEq.derive
  }

  sealed trait MsgError
  object MsgError {
    case object DecodingFailure extends MsgError
    final case class RespondError(err: Throwable) extends MsgError
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[D[_], F[_]](config: Config)
                       (implicit
                        D       : Monad[D],
                        F       : Monad[F] with BindRec[F],
                        apEvent : ApplyEventLogic[F],
                        db      : DB.ForProjectSpa[D],
                        metrics : MetricsLogic[F],
                        redis   : Redis.ProjectAlgebra[F],
                        runDB   : D ~> F,
                        security: Security.Algebra[F],
                        svr     : Server.Time[F],
                        trace   : Trace.Algebra[F]): ProjectSpaLogic[F] = {
    val webSocketHelper = WebSocketServerHelper(ProjectSpaProtocols.WebSocket(Obfuscated(null)))

    val OnConnect  = Monads.FDisj[F, ConnectRejection]
    val OnMsgError = Monads.FDisj[F, MsgError]

    val fUnit = F.pure(())

    import trace.Span

    def getSpan(static: WebSocketStatic): Span =
      static.span.asInstanceOf[Span]

    new ProjectSpaLogic[F] { self =>

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def initPage(pid: ProjectId, username: Username): F[ProjectSpaEntryPoint.InitData] =
        for {
          name <- runDB(db.projectSpaInitPage(pid))
        } yield {
          val pidPub = Obfuscators.projectId.obfuscate(pid)
          ProjectSpaEntryPoint.InitData(username, pidPub, name)
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def onConnect(cookies  : Cookie.LookupFn,
                             projectId: ProjectId.Public) = {
        val C = OnConnect
        import ConnectRejection._

        def main(span: Span): C.Result[(WebSocketStatic, WebSocketState[F])] =
          for {
            pid     <- C.lift(Obfuscators.projectId.deobfuscate(projectId).leftMap(_ => InvalidProjectId))
            session <- C.optionF(security.sessionRestore(cookies), NoSession)
            user    <- C.option(session.authenticatedUser, AnonymousSession)
            _       <- C.rightF(trace.addAttrs(Trace.Attr.ShipReqUserId(user.id.value) ::
                                               Trace.Attr.ShipReqProjectId(pid.value) :: Nil)(span))
            owner   <- C.optionF(security.db.getProjectOwner(pid), ProjectNotFound)
            _       <- C.ensure(user.id ==* owner, AccessDenied)
            now     <- C.rightF(svr.now)
          } yield {
            val static = WebSocketStatic(user, pid, span, now)
            val state  = WebSocketState.empty[F]
            (static, state)
          }

        trace.newSpan("WebSocket")(span =>
          trace.newSubSpan("onConnect", span)(_ =>
            security.protect(
              for {
                (r, dur) ← svr.measureDuration(main(span).value)
                mresult  = r match {
                             case \/-(_)                => "ok"
                             case -\/(NoSession       ) => "NoSession"
                             case -\/(AnonymousSession) => "AnonymousSession"
                             case -\/(InvalidProjectId) => "InvalidProjectId"
                             case -\/(ProjectNotFound ) => "ProjectNotFound"
                             case -\/(AccessDenied    ) => "AccessDenied"
                           }
                _        ← metrics.projectSpaWebSocketConnected(dur, mresult)
              } yield r
            )))
      }

      override def onOpen(static: WebSocketStatic,
                          state: WebSocketState[F],
                          push : BinaryData => F[Unit]) = {
        val span = getSpan(static)
        def main: F[WebSocketState[F]] =
          state.sub match {
            case None =>
              for {
                sub <- redis.subscribe(static.projectId, pushEvent(span, push, _))
              } yield WebSocketState(Some(sub))
            case Some(_) =>
              F.pure(state)
          }
        trace.newSubSpan("onOpen", span)(_ =>
          for {
            (r, dur) <- svr.measureDuration(main)
            _        <- metrics.projectSpaWebSocketOpened(dur)
          } yield r
        )
      }

      override def onClose(staticO: Option[WebSocketStatic],
                           stateO : Option[WebSocketState[F]]) = {

        val main: F[Unit] =
          stateO.flatMap(_.sub).fold(fUnit)(_.unsubscribe)

        staticO match {
          case Some(static) =>
            trace.newSubSpan("onClose", getSpan(static)) { _ =>
              for {
                dur        ← svr.measureDuration_(main)
                now        ← svr.now
                sessionDur = Duration.between(static.connectedAt, now)
                _          ← metrics.projectSpaWebSocketClosed(dur, sessionDur)
              } yield logger.info(s"WebSocket closed after ${sessionDur.conciseDesc}")
            }
          case None =>
            main
        }
      }

      private def pushEvent(span: Span, push: BinaryData => F[Unit], e: VerifiedEvent): F[Unit] =
        pushEvents(span, push, VerifiedEvent.NonEmptySeq.one(e))

      private def pushEvents(span: Span, push: BinaryData => F[Unit], es: VerifiedEvent.NonEmptySeq): F[Unit] =
        trace.newSubSpan("push", span) { _ =>
          for {
            msgBin <- F point BinaryJvm.encode(webSocketHelper.protocolSC)(-\/(es))
            _      <- metrics.projectSpaWebSocketPush(msgBin.length)
            _      <- push(msgBin)
          } yield ()
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // Message responding

      type OptionState = Option[WebSocketState[F]]

      override def onMessage(static : WebSocketStatic,
                             state  : WebSocketState[F],
                             msg    : BinaryData,
                             respond: BinaryData => F[Throwable \/ Unit],
                             push   : BinaryData => F[Unit],
                             onError: MsgError => F[Unit]): F[OptionState] = {
        val M = OnMsgError

        val span = getSpan(static)

        def body(implicit span: Span): F[MsgResult[F]] = {

          def handleError(wsReqRes: FreeOption[WsReqRes], err: MsgError): F[MsgResult[F]] =
            onError(err) >> F.point {
              err match {
                case MsgError.DecodingFailure => logger.warn(s"Failed to decode message: ${msg.describe(1000)}")
                case MsgError.RespondError(e) => logger.error(s"Error sending response.", e)
              }
              new MsgResult(wsReqRes, -1L, None)
            }

          parseMsg(msg) match {
            case \/-((reqId, req)) =>
              for {
                _              ← trace.rename("onMessage: " + req.reqRes.name)
                msgFnIn        = MsgFnIn(req.req, static, state, push)
                msgFnOut       ← msgFold(req.reqRes)(msgFnIn): F[MsgFnOut[F, req.reqRes.ResponseType]]
                protocolAndRes = req.reqRes.protocolRes.andValue(msgFnOut.output)
                fullRes        = \/-((reqId, protocolAndRes))
                resBin         = BinaryJvm.encode(webSocketHelper.protocolSC)(fullRes)
                wsReqRes       = FreeOption(req.reqRes)
                result         ← respond(resBin).flatMap {
                                   case \/-(_) => F.point(new MsgResult(wsReqRes, resBin.length, msgFnOut.newState))
                                   case -\/(e) => handleError(wsReqRes, MsgError.RespondError(e))
                                 }
              } yield result

            case -\/(e) =>
              handleError(FreeOption.empty, e)
          }
        }

        for {
          (r, dur) ← svr.measureDuration(trace.newSubSpan("onMessage", span)(body(_)))
          pid      = static.projectId.value
          _        ← metrics.projectSpaWebSocketMsg(r.msgType, msg.length, r.bytesOut, dur, r.ok)
          _        ← F.point(logger.info(s"WebSocket for project #$pid processed request in ${dur.conciseDesc}"))
        } yield r.newState
      }

      private def parseMsg(msg: BinaryData) = {
        BinaryJvm.decode(msg, webSocketHelper.protocolCS) match {
          case Success(r) => \/-(r)
          case Failure(_) => -\/(MsgError.DecodingFailure)
        }
      }

      private type MsgFn     [I, O]          = MsgFnIn[F, I] => F[MsgFnOut[F, O]]
      private type MsgFoldIn [R <: WsReqRes] = MsgFnIn[F, R#RequestType]
      private type MsgFoldOut[R <: WsReqRes] = F[MsgFnOut[F, R#ResponseType]]

      private val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
        onInitApp               = onInitApp,
        onReconnect             = onReconnect,
        onSync                  = onSync,
        onUpdateConfig          = updateProject (MakeEvent.updateConfig),
        onCreateContent         = updateProject (MakeEvent.createContent),
        onUpdateContent         = updateProject (MakeEvent.updateContent),
        onProjectNameSet        = updateProjectI(MakeEvent.projectNameSetFn),
        onUpdateSavedViews      = updateProject (MakeEvent.updateSavedViews),
        onUpdateManualIssues    = updateProject (MakeEvent.updateManualIssues),
        onFieldMandatorinessMod = updateProjectI(MakeEvent.fieldMandatorinessMod),
        onReqTypeImplicationMod = updateProjectI(MakeEvent.reqTypeImplicationMod),
      )

      private val writeSnapshotInsteadOfEvents: Int => Boolean =
        (config.writeSnapshots, config.writeEvents) match {
          case (true , false) => _ => true
          case (false, true ) => _ => false
          case _ =>
            val eventSnapshotCacheRatio = config.eventSnapshotCacheRatio
            _ % eventSnapshotCacheRatio == 0
        }

      private val projectUpdater = new ProjectUpdater[D, F](writeSnapshotInsteadOfEvents)

      private def updateProject[I](mkEvent: (I, Project) => MakeEvent.Result): MsgFn[I, EventResult] =
        in => projectUpdater(in.static.projectId, mkEvent(in.input, _)).map(MsgFnOut(_, None))

      private def updateProjectI[I](mkEvent: I => MakeEvent.Result): MsgFn[I, EventResult] =
        updateProject((i, _) => mkEvent(i))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onInitApp: MsgFn[Unit, ErrorMsg \/ InitAppData] = in => {
        val pid = in.static.projectId

        type Result = ErrorMsg \/ InitAppData

        def projectNotFound = -\/(ErrorMsg("Project not found."))

        def step[A](name: String)(f: F[A]): F[A] =
          trace.newSpan(name)(_ => metrics.projectSpaWebSocketStep("load", name)(f))

        def ignoreCache(c: Redis.ProjectCache): F[Result] = {

          def readDb(p: ProjectAndOrd) =
            step("readDb")(
              for {
                (es, mdo) <- runDB(
                               db.inDbTransaction(for {
                                 md <- db.getProjectMetaData(pid)
                                 es <- db.getProjectEvents(pid, DB.EventFilter.given(p.ord))
                               } yield (es, md))
                             )
                buildResult <- apEvent.append(pid, p, es)
              } yield mdo match {
                case Some(md) => buildResult.map(InitAppData(_, md))
                case None     => projectNotFound
              }
            )

          def writeRedis(i: InitAppData): F[Boolean] =
            step("writeRedis")(
              // Maybe write events instead of snapshot here...
              // But really it's going to be such low % that writing events in (TLA+) Load would be worth the logic.
              // The snapshot/event writing decision is much more relevant in (TLA+) Update.
              redis.writeSnapshot(pid, i.project, VerifiedEvent.Seq.empty)
            )

          for {
            cacheb <- c.buildNonEmpty(pid)
            result <- readDb(cacheb getOrElse ProjectAndOrd.empty)
            _      <- result.fold[F[_]](_ => fUnit, writeRedis)
          } yield result
        }

        def useCache(c: Redis.ProjectCache, md: ProjectMetaData): F[Result] =
          step("useCache") {
            for {
              r <- c.build(pid)
            } yield r.map(InitAppData(_, md))
          }

        for {
          cache <- redis.read(pid)
          mdOpt <- runDB(db.getProjectMetaData(pid))
          r     <- mdOpt match {
                     case Some(md) => if (cache.isCompleteTo(md.latestOrd)) useCache(cache, md) else ignoreCache(cache)
                     case None     => F pure projectNotFound
                   }
        } yield MsgFnOut(r, None)
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onReconnect: MsgFn[Option[EventOrd.Latest], VerifiedEvent.Seq] = in => {
        val pid     = in.static.projectId
        val userOrd = in.input
        val span    = getSpan(in.static)

        def step[A](name: String)(f: F[A]): F[A] =
          trace.newSpan(name)(_ => metrics.projectSpaWebSocketStep("reconnect", name)(f))

        val redisSubscribe: F[OptionState] =
          in.state.sub match {
            case None    => redis.subscribe(pid, pushEvent(span, in.push, _)).map(s => Some(WebSocketState(Some(s))))
            case Some(_) => F.pure(None)
          }

        def loadEventsFromRedis: F[VerifiedEvent.Seq] =
          step("readRedis")(redis.readEvents(pid, userOrd))

        def loadEventsFromDb(o: Option[EventOrd.Latest]): F[VerifiedEvent.Seq] =
          step("readDb")(runDB(db.getProjectEvents(pid, DB.EventFilter.given(o))))

        def loadEvents(dbLatest: Option[EventOrd.Latest]): F[VerifiedEvent.Seq] =
          loadEventsFromRedis.flatMap { cachedEvents =>
            val cachedEventsAreUsable = cachedEvents.nonEmpty && cachedEvents.head.ord.immediatelyFollowsLatest(userOrd)
            if (cachedEventsAreUsable) {
              val newLatest = Some(cachedEvents.last.ord.asLatest)
              if (dbLatest > newLatest)
                for {
                  dbEvents <- loadEventsFromDb(newLatest)
                  // Not writing back to Redis for now
                  // * Snapshot could be 1000, this could send events 500-1001 for no reason
                  // * Normal processing (covered by TLA) keeps the cache up-to-date - a reconnection seems orthogonal
                  // _ <- redis.writeEvents(pid, dbEvents, VerifiedEvent.Seq.empty)
                } yield cachedEvents ++ dbEvents
              else
                F.pure(cachedEvents)
            } else
              loadEventsFromDb(userOrd)
          }

        for {
          newState ← redisSubscribe
          mdOpt    ← runDB(db.getProjectMetaData(pid))
          md       = mdOpt.get // This will fail during connection usage after project deleted
          dbLatest = md.latestOrd
          events   ← if (dbLatest > userOrd) loadEvents(dbLatest) else F.pure(VerifiedEvent.Seq.empty)
        } yield MsgFnOut(events, newState)
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onSync: MsgFn[NonEmptySet[EventOrd], Unit] = in => {
        val pid  = in.static.projectId
        val ords = in.input

        for {
          events <- runDB(db.getProjectEvents(pid, DB.EventFilter.Set(ords)))
          _      <- redis.writeEvents(pid, VerifiedEvent.Seq.empty, events)
        } yield MsgFnOut((), None)
      }


    } // new ProjectSpaLogic
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final case class MsgFnIn[F[_], I](input : I,
                                            static: WebSocketStatic,
                                            state : WebSocketState[F],
                                            push  : BinaryData => F[Unit])

  private final case class MsgFnOut[F[_], O](output: O, newState: Option[WebSocketState[F]])

  private final class MsgResult[F[_]](reqType: FreeOption[WsReqRes], len: Long, val newState: Option[WebSocketState[F]]) {
    def msgType : String  = reqType.fold("unknown", _.name)
    def ok      : Boolean = len >= 0
    def bytesOut: Long    = if (ok) len else 0
  }

  private final class ProjectUpdater[D[_], F[_]](writeSnapshot: Int => Boolean)
                                                (implicit
                                                 D       : Monad[D],
                                                 F       : Monad[F] with BindRec[F],
                                                 apEvent : ApplyEventLogic[F],
                                                 db      : DB.ForProjectSpa[D],
                                                 metrics : MetricsLogic[F],
                                                 redis   : Redis.ProjectAlgebra[F],
                                                 runDB   : D ~> F,
                                                 security: Security.Algebra[F],
                                                 trace   : Trace.Algebra[F]) {
    import ProjectUpdater._

    type Result = ErrorMsg \/ VerifiedEvent.Seq

    def apply(pid: ProjectId, mkEvent: Project => MakeEvent.Result): F[Result] = {
      var gas = 200

      def loop(s: State): F[State \/ Result] = {
        import Status._

        if (gas > 0) gas -= 1 else throw new IllegalStateException(s"Infinite loop! state=$s")

        val main: F[State \/ Result] = s.status match {

          case ReadRedis =>
            for {
              r     ← redis.read(pid)
              built ← r.build(pid)
            } yield built match {
              case \/-(p) => -\/(s.copy(local = p, redis = r, status = if (r.ord > s.local.ord) WriteDb else ReadDb))
              case -\/(e) => \/-(-\/(e))
            }

          case ReadDb =>
            for {
              cacheBuilt ← s.redis.buildNonEmpty(pid)
              p1         = s.local max cacheBuilt
              newEvents  ← runDB(db.getProjectEvents(pid, DB.EventFilter.given(p1.ord)))
              built      ← apEvent.append(pid, p1, newEvents)
            } yield built match {
              case \/-(p2) => -\/(s.copy(local = p2, status = WriteRedis1(newEvents)))
              case -\/(e)  => \/-(-\/(e))
            }

          case WriteRedis1(newEvents) =>
            val writeRedis: F[Boolean] =
              if (writeSnapshot(s.local.ordAsInt))
                redis.writeSnapshot(pid, s.local, VerifiedEvent.Seq.empty)
              else
                redis.writeEvents(pid, newEvents, VerifiedEvent.Seq.empty)
            for {
              ok <- writeRedis
            } yield -\/(s.copy(status = if (ok) WriteDb else ReadRedis))

          case WriteDb =>
            mkEvent(s.local.project).flatMap(ApplyNewEvent(_, s.local.project)) match {
              case PotentialChange.Success(updated) =>
                val saveCmd = DB.SaveProjectEventCmd(s.local.nextOrd, updated.event)
                runDB(db.saveProjectEvent(pid, saveCmd)) map {
                  case \/-(ve) =>
                    val nextStatus = WriteRedis2(updated.project, ve)
                    -\/(s.copy(status = nextStatus))
                  case -\/(_) =>
                    -\/(s.copy(status = ReadRedis))
                }

              case PotentialChange.Unchanged =>
                F pure \/-(\/-(VerifiedEvent.Seq.empty))

              case PotentialChange.Failure(e) =>
                F pure \/-(-\/(ErrorMsg(e)))
            }

          case WriteRedis2(newProject, newEvent) =>
            val newEvents = VerifiedEvent.Seq.one(newEvent)
            val writeRedis: F[Boolean] =
              if (writeSnapshot(newEvent.ord.value)) {
                val ss = Redis.ProjectSnapshot(newProject, newEvent.ord.asLatest)
                redis.writeSnapshot(pid, ss, newEvents)
              } else
                redis.writeEvents(pid, VerifiedEvent.Seq.empty, newEvents)
            for {
              _ <- writeRedis
            } yield \/-(\/-(newEvents))
        }

        trace.newSpan(s.status.name)(_ =>
          metrics.projectSpaWebSocketStep("update", s.status.name)(
            main))
      }

      F.tailrecM(loop)(initialState)
    }
  }

  private object ProjectUpdater {

    final case class State(local : ProjectAndOrd,
                           redis : Redis.ProjectCache,
                           status: Status)

    val initialState = State(
      local  = ProjectAndOrd.empty,
      redis  = Redis.ProjectCache.empty,
      status = Status.ReadRedis)

    sealed abstract class Status(final val name: String)
    object Status {
      case object ReadRedis                                                      extends Status("ReadRedis")
      case object ReadDb                                                         extends Status("ReadDb")
      case object WriteDb                                                        extends Status("WriteDb")
      final case class WriteRedis1(newEvents: VerifiedEvent.Seq)                 extends Status("WriteRedis1")
      final case class WriteRedis2(newProject: Project, newEvent: VerifiedEvent) extends Status("WriteRedis2")
    }
  }
}
