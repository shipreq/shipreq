package shipreq.webapp.server.logic.logic

import cats.effect.Sync
import cats.syntax.all.{catsSyntaxEither => _, _}
import cats.{Monad, ~>}
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.taskman.api.{Task, TaskmanApi, UserId => TaskmanUserId}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.EventOrd.Implicits._
import shipreq.webapp.member.project.event.{ApplyEvent, Event, EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WsReqRes.EventResult
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.{InitAppData, StateUpdate, Supplimentary, WsReqRes}
import shipreq.webapp.member.project.protocol.websocket._
import shipreq.webapp.member.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.server.logic.algebra.{Crypto, DB, MetricsAlgebra, Redis, Security, Server}
import shipreq.webapp.server.logic.config.ScalaJsManifest
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.event.{ApplyEventAlgebra, ApplyNewEvent, MakeEvent}
import shipreq.webapp.server.logic.util.Obfuscators

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def initPage(projectId: ProjectId,
               userId   : UserId,
               username : Username,
               am       : AssetManifest): F[Option[ProjectSpaEntryPoint.InitData]]

  def onConnect(cookies  : Cookie.LookupFn,
                projectId: ProjectId.Public,
                creator  : ProjectCreator): F[ConnectRejection \/ (WebSocketStatic, WebSocketState[F])]

  def onOpen(static : WebSocketStatic,
             state  : WebSocketState[F],
             push   : BinaryData => F[Unit],
             onError: ListenerError => F[Unit]): F[WebSocketState[F]]

  def onMessage(static         : WebSocketStatic,
                state          : WebSocketState[F],
                msg            : BinaryData,
                respond        : BinaryData => F[Throwable \/ Unit],
                push           : BinaryData => F[Unit],
                onListenerError: ListenerError => F[Unit],
                onError        : MsgError => F[Unit]): F[Option[WebSocketState[F]]]

  def onKeepAlive(static : WebSocketStatic,
                  onError: MsgError => F[Unit]): F[Unit]

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
    import cats.syntax.apply._

    def default = Config(100, true, true) // see calculations.ods

    def defn: ConfigDef[Config] =
      ( ConfigDef.getOrUse("eventSnapshotCacheRatio", default.eventSnapshotCacheRatio),
        ConfigDef.getOrUse("writeSnapshots", default.writeSnapshots),
        ConfigDef.getOrUse("writeEvents", default.writeEvents),
      ).mapN(apply)
  }

  val userFacingErrorMsgWhenDataPropFails =
    ErrorMsg("Invalid input. The fact the we can't be more specific is a bug. Our staff have been notified and we'll endeavour to fix this ASAP.")

  @inline def userFacingErrorMsgCantRemoveAdmin =
    MakeEvent.userFacingErrorMsgCantRemoveAdmin

  final case class WebSocketStatic(user       : User,
                                   projectId  : ProjectId,
                                   creator    : ProjectCreator,
                                   sessionId  : Security.SessionId,
                                   span       : Any,
                                   connectedAt: Instant,
                                   expiresAt  : Instant) {

    def userIdPublic: UserId.Public =
      Obfuscators.userId.obfuscate(user.id)
  }

  final case class WebSocketState[F[_]](sub: Option[Redis.Subscription[F]])
  object WebSocketState {
    def empty[F[_]] = apply[F](None)
  }

  sealed trait ConnectRejection
  object ConnectRejection {
    case object NoSession        extends ConnectRejection
    case object AnonymousSession extends ConnectRejection
    case object ExpiredSession   extends ConnectRejection
    case object InvalidProjectId extends ConnectRejection
    case object AccessDenied     extends ConnectRejection
    implicit def univEq: UnivEq[ConnectRejection] = UnivEq.derive
  }

  sealed trait ListenerError
  object ListenerError {
    final case class RedisDecodingFailure(err: SafePickler.DecodingFailure) extends ListenerError
    final case class RedisLibraryException(err: Throwable) extends ListenerError
    case object SessionExpired extends ListenerError
  }

  sealed trait MsgError
  object MsgError {
    final case class ClientMsgDecodingFailure(err: SafePickler.DecodingFailure) extends MsgError
    final case class ServerBehindClient(err: SafePickler.DecodingFailure) extends MsgError
    final case class ServerBehindDatabase(err: DB.ReadProjectEventError) extends MsgError
    final case class ServerBehindRedis(err: SafePickler.DecodingFailure) extends MsgError
    final case class RespondError(err: Throwable) extends MsgError
    final case class FunctionNoLongerSupported(devDesc: String) extends MsgError
    case object SessionExpired extends MsgError
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[D[_], F[_]](config: Config)
                       (implicit
                        D       : Monad[D],
                        F       : Sync[F],
                        apEvent : ApplyEventAlgebra[F],
                        crypto  : Crypto[F],
                        db      : DB.ForProjectSpa[D],
                        metrics : MetricsAlgebra[F],
                        redis   : Redis.ProjectAlgebra[F],
                        runDB   : D ~> F,
                        security: Security.Algebra[F],
                        sjsUrls : ScalaJsManifest[String],
                        svr     : Server.Time[F],
                        taskman : TaskmanApi[F],
                        trace   : Trace.Algebra[F]): ProjectSpaLogic[F] = {

    val webSocketHelper = {
      val p = Obfuscated(null): ProjectId.Public
      val c = ProjectCreator(Obfuscated(null))
      WebSocketServerHelper(ProjectSpaProtocols.WebSocket(p, c))
    }

    val OnConnect = Monads.FDisj[F, ConnectRejection]

    import trace.Span

    def getSpan(static: WebSocketStatic): Span =
      static.span.asInstanceOf[Span]

    def fRight[E, A, B](f: E \/ A)(g: A => F[B]): F[E \/ B] =
      f match {
        case \/-(a) => F.map(g(a))(\/-(_))
        case -\/(e) => F.pure(-\/(e))
      }

    new ProjectSpaLogic[F] { self =>

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def initPage(pid: ProjectId, uid: UserId, username: Username, am: AssetManifest): F[Option[ProjectSpaEntryPoint.InitData]] =
        for {
          o <- runDB(db.projectSpaInitPage(pid, uid))
        } yield o.map { i =>
          val userId    = Obfuscators.userId.obfuscate(uid)
          val creatorId = if (i.creatorId ==* uid) userId else Obfuscators.userId.obfuscate(i.creatorId)
          ProjectSpaEntryPoint.InitData(
            username         = username,
            userId           = Obfuscators.userId.obfuscate(uid),
            projectId        = Obfuscators.projectId.obfuscate(pid),
            creator          = ProjectCreator(creatorId),
            projectName      = i.name,
            assetManifest    = am,
            webWorkerJsUrl   = sjsUrls.webWorker,
            encryptionKey    = crypto.clientSideProjectEncryptionKey(i.userKey, i.projectKey),
          )
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def onConnect(cookies  : Cookie.LookupFn,
                             projectId: ProjectId.Public,
                             creator  : ProjectCreator) = {
        val C = OnConnect
        import ConnectRejection._

        val parseSessionResult: Security.SessionRestoreResult[Instant] => C.Result[Security.SessionToken[Instant]] = {
          case Security.SessionRestoreResult.Success(t) => C.pure(t)
          case Security.SessionRestoreResult.Expired(_) => C.fail(ExpiredSession)
          case Security.SessionRestoreResult.None       => C.fail(NoSession)
        }

        def main(span: Span): C.Result[(WebSocketStatic, WebSocketState[F])] =
          for {
            pid     <- C.lift(Obfuscators.projectId.deobfuscate(projectId).leftMap(_ => InvalidProjectId))
            ssr     <- C.rightF(security.sessionRestore(cookies))
            session <- parseSessionResult(ssr)
            user    <- C.option(session.authenticatedUser, AnonymousSession)
            _       <- C.rightF(trace.addAttrs(Trace.Attr.ShipReqUserId(user.id.value) ::
                                               Trace.Attr.ShipReqProjectId(pid.value) :: Nil)(span))
            _       <- C.optionF(security.db.getProjectAccess(pid, user.id), AccessDenied)
            now     <- C.rightF(svr.now)
          } yield {
            val static = WebSocketStatic(
                           user        = user,
                           projectId   = pid,
                           creator     = creator,
                           sessionId   = session.sessionId,
                           span        = span,
                           connectedAt = now,
                           expiresAt   = session.expiry)
            val state  = WebSocketState.empty[F]
            (static, state)
          }

        trace.newSpan("WebSocket")(span =>
          trace.newSubSpan("onConnect", span)(_ =>
            security.protect(
              for {
                (r, dur) <- svr.measureDuration(main(span).value)
                mresult  = r match {
                             case \/-(_)                => "ok"
                             case -\/(NoSession       ) => "NoSession"
                             case -\/(AnonymousSession) => "AnonymousSession"
                             case -\/(ExpiredSession  ) => "ExpiredSession"
                             case -\/(InvalidProjectId) => "InvalidProjectId"
                             case -\/(AccessDenied    ) => "AccessDenied"
                           }
                _        <- metrics.projectSpaWebSocketConnected(dur, mresult)
              } yield r
            )))
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def onOpen(static : WebSocketStatic,
                          state  : WebSocketState[F],
                          push   : BinaryData => F[Unit],
                          onError: ListenerError => F[Unit]) = {
        val span = getSpan(static)
        def main: F[WebSocketState[F]] =
          state.sub match {
            case None =>
              for {
                sub <- redis.subscribe(static.projectId, listener(static, span, push, onError))
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

      private def listener(static : WebSocketStatic,
                           span   : Span,
                           push   : BinaryData => F[Unit],
                           onError: ListenerError => F[Unit]): Redis.Listener[F] = {
        case \/-(event) =>
          hasExpired(static).flatMap {
            case false => pushEvent(span, push, event)
            case true  => onError(ListenerError.SessionExpired)
          }

        case -\/(Redis.ListenerError.DecodingFailure(err)) =>
          for {
            _ <- F.delay(logger.warn("Failed to parse pub/sub event from Redis: ", err))
            _ <- onError(ListenerError.RedisDecodingFailure(err))
          } yield ()

        case -\/(Redis.ListenerError.RedisLibraryException(err)) =>
          for {
            _ <- F.delay(logger.warn("Failed to parse pub/sub event from Redis: ", err))
            _ <- onError(ListenerError.RedisLibraryException(err))
          } yield ()
      }

      private def pushEvent(span: Span, push: BinaryData => F[Unit], e: VerifiedEvent): F[Unit] =
        pushEvents(span, push, VerifiedEvent.NonEmptySeq.one(e))

      private def pushEvents(span: Span, push: BinaryData => F[Unit], es: VerifiedEvent.NonEmptySeq): F[Unit] =
        trace.newSubSpan("push", span) { _ =>

          for {
            msg    <- webSocketPushDataForEvents(es)
            msgBin <- F pure webSocketHelper.protocolSC.codec.encode(-\/(msg))
            _      <- metrics.projectSpaWebSocketPush(msgBin.length)
            _      <- push(msgBin)
          } yield ()
        }

      private def webSocketPushDataForEvents(es: VerifiedEvent.NonEmptySeq): F[Push] = {
        val events = es.values
        for (supp <- supplimentaryDataForEvents(events))
        yield StateUpdate(events, supp)
      }

      private def hasExpired(static: WebSocketStatic): F[Boolean] =
        svr.now.map(_.isAfter(static.expiresAt))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def onClose(staticO: Option[WebSocketStatic],
                           stateO : Option[WebSocketState[F]]) = {

        val main: F[Unit] =
          stateO.flatMap(_.sub).fold(F.unit)(_.unsubscribe)

        staticO match {
          case Some(static) =>
            trace.newSubSpan("onClose", getSpan(static)) { _ =>
              for {
                dur        <- svr.measureDuration_(main)
                now        <- svr.now
                sessionDur = Duration.between(static.connectedAt, now)
                _          <- metrics.projectSpaWebSocketClosed(dur, sessionDur)
              } yield logger.info(s"WebSocket closed after ${sessionDur.conciseDesc}")
            }
          case None =>
            main
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // Message responding

      type OptionState = Option[WebSocketState[F]]

      private def logError(err: MsgError, descMsg: () => String): F[Unit] =
        F.delay {
          err match {
            case MsgError.SessionExpired =>
              logger.info("Session expired.")

            case MsgError.FunctionNoLongerSupported(desc) =>
              logger.warn(s"FunctionNoLongerSupported: $desc")

            case MsgError.ClientMsgDecodingFailure(e) =>
              logger.warn(s"Failed to parse message from client: ${descMsg()}", e)

            case MsgError.ServerBehindDatabase(e) =>
              logger.warn(s"Server older than DB. Aborting. ClientMsg:[${descMsg()}]", e)

            case MsgError.ServerBehindClient(e) =>
              logger.warn(s"Server older than client. Aborting. ClientMsg:[${descMsg()}]", e)

            case MsgError.ServerBehindRedis(e) =>
              logger.warn(s"Server older than cache. Aborting. CacheData:[${descMsg()}]", e)

            case MsgError.RespondError(e) =>
              logger.error(s"Error sending response.", e)
          }
        }

      override def onKeepAlive(static: WebSocketStatic, onError: MsgError => F[Unit]): F[Unit] =
        hasExpired(static).flatMap { expired =>
          if (expired) {
            val err = MsgError.SessionExpired
            logError(err, () => "") >> onError(err)
          } else
            F.unit
        }

      override def onMessage(static         : WebSocketStatic,
                             state          : WebSocketState[F],
                             msg            : BinaryData,
                             respond        : BinaryData => F[Throwable \/ Unit],
                             push           : BinaryData => F[Unit],
                             onListenerError: ListenerError => F[Unit],
                             onError        : MsgError => F[Unit]): F[OptionState] = {

        val span = getSpan(static)

        def body(implicit span: Span): F[MsgResult[F]] = {

          def handleError(wsReqRes: FreeOption[WsReqRes], err: MsgError): F[MsgResult[F]] =
            onError(err) >> logError(err, () => msg.describe(1000)) as new MsgResult(wsReqRes, -1L, None)

          val mainResponseLogic: F[MsgResult[F]] =
            parseClientMsg(msg) match {
              case \/-((reqId, req)) =>
                logger.debug(s"Received $reqId: $req")
                // GenerateUnitTest.req(webSocketHelper, msg)(reqId, req)
                def respondWith(msgFnOut: MsgFnOut[F, req.reqRes.ResponseType]): F[MsgResult[F]] = {
                  val protocolAndRes = req.reqRes.protocolRes.andValue(msgFnOut.output)
                  val fullRes        = \/-((reqId, protocolAndRes))
                  val resBin         = webSocketHelper.protocolSC.codec.encode(fullRes)
                  val wsReqRes       = FreeOption(req.reqRes)
                  logger.debug(s"Responding to $reqId with $fullRes -- $resBin")
                  // GenerateUnitTest.resp(webSocketHelper, req, fullRes)(resBin)
                  respond(resBin).flatMap {
                    case \/-(_) => F.delay(new MsgResult(wsReqRes, resBin.length, msgFnOut.newState))
                    case -\/(e) => handleError(wsReqRes, MsgError.RespondError(e))
                  }
                }
                for {
                  _           <- trace.rename("onMessage: " + req.reqRes.name)
                  msgFnIn     = MsgFnIn(req.req, static, state, push, onListenerError)
                  msgErrOrOut <- msgFold(req.reqRes)(msgFnIn): F[MsgError \/ MsgFnOut[F, req.reqRes.ResponseType]]
                  result      <- msgErrOrOut match {
                                   case \/-(msgFnOut) => respondWith(msgFnOut)
                                   case -\/(e)        => handleError(FreeOption.empty, e)
                                 }
                } yield result

              case -\/(e) =>
                handleError(FreeOption.empty, e)
            }

          hasExpired(static).flatMap { expired =>
            if (expired)
              handleError(FreeOption.empty, MsgError.SessionExpired)
            else
              mainResponseLogic
          }
        }

        for {
          (r, dur) <- svr.measureDuration(trace.newSubSpan("onMessage", span)(body(_)))
          pid      = static.projectId.value
          _        <- metrics.projectSpaWebSocketMsg(r.msgType, msg.length, r.bytesOut, dur, r.ok)
          _        <- F.delay(logger.info(s"WebSocket for project #$pid processed request in ${dur.conciseDesc}"))
        } yield r.newState
      }

      private def parseClientMsg(msg: BinaryData) = {
        webSocketHelper.protocolCS.codec.decode(msg).leftMap[MsgError] { e =>
          if (e.isLocalKnownToBeOutOfDate)
            MsgError.ServerBehindClient(e)
          else
            MsgError.ClientMsgDecodingFailure(e)
        }
      }

      private type MsgFn     [I, O]          = MsgFnIn[F, I] => F[MsgError \/ MsgFnOut[F, O]]
      private type MsgFoldIn [R <: WsReqRes] = MsgFnIn[F, R#RequestType]
      private type MsgFoldOut[R <: WsReqRes] = F[MsgError \/ MsgFnOut[F, R#ResponseType]]

      // This logic is duplicated in TestGlobal
      private val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
        onInitApp               = onInitApp,
        onReconnect             = onReconnect,
        onSync                  = onSync,
        onUpdateConfig          = updateProject (MakeEvent.updateConfig, ProjectRole.Collaborator),
        onCreateContent         = updateProject (MakeEvent.createContent, ProjectRole.Collaborator),
        onUpdateContent         = updateProject (MakeEvent.updateContent, ProjectRole.Collaborator),
        onProjectNameSet        = updateProjectI(MakeEvent.projectNameSetFn, ProjectRole.Admin),
        onUpdateSavedViews      = updateProject (MakeEvent.updateSavedViews, ProjectRole.Collaborator),
        onUpdateManualIssues    = updateProject (MakeEvent.updateManualIssues, ProjectRole.Collaborator),
        onFieldMandatorinessMod = _ => F.pure(-\/(MsgError.FunctionNoLongerSupported("fieldMandatorinessMod"))),
        onReqTypeImplicationMod = updateProjectI(MakeEvent.reqTypeImplicationMod, ProjectRole.Collaborator),
        onUpdateAccess          = onUpdateAccess,
      )

      private val writeSnapshotInsteadOfEvents: Int => Boolean =
        (config.writeSnapshots, config.writeEvents) match {
          case (true , false) => _ => true
          case (false, true ) => _ => false
          case _ =>
            val eventSnapshotCacheRatio = config.eventSnapshotCacheRatio
            _ % eventSnapshotCacheRatio == 0
        }

      private val submitDataPropError: (UserId, ErrorMsg) => F[Unit] = {
        var submitted = false
        (userId, err) =>
          F.delay {
            if (submitted)
              F.unit
            else {
              val nameKey = "error.name"
              val msgKey  = "error.message"
              val task =
                Task.ReportServerError(
                  userId     = Some(TaskmanUserId(userId.value)),
                  nameKey    = nameKey,
                  messageKey = msgKey,
                  data       = Map(nameKey -> "DataProp failure", msgKey -> err.value)
                )
              for {
                _ <- taskman.submit(task)
                _ <- F.delay { submitted = true }
              } yield ()
            }
          }.flatMap(identity)
      }

      private def updateProject[I](mkEvent: (I, Project) => MakeEvent.Result, requiredRole: ProjectRole): MsgFn[I, EventResult] =
        in => projectUpdater(in.static.projectId, in.static.creator, in.static.user.id, mkEvent(in.input, _), requiredRole).map {
          case ProjectUpdater.Result.Ok(upd)                 => \/-(MsgFnOut(\/-(upd), None))
          case ProjectUpdater.Result.Reject(e)               => \/-(MsgFnOut(-\/(e), None))
          case ProjectUpdater.Result.ServerBehindDatabase(e) => -\/(MsgError.ServerBehindDatabase(e))
          case ProjectUpdater.Result.ServerBehindRedis(e)    => -\/(MsgError.ServerBehindRedis(e))
        }

      private def updateProjectI[I](mkEvent: I => MakeEvent.Result, requiredRole: ProjectRole): MsgFn[I, EventResult] =
        updateProject((i, _) => mkEvent(i), requiredRole)

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onInitApp: MsgFn[Option[EventOrd.Latest], ErrorMsg \/ InitAppData] = in => {
        val creator = in.static.creator
        val pid     = in.static.projectId
        val uid     = in.static.user.id
        val userOrd = in.input

        type Result = ErrorMsg \/ InitAppData

        def projectNotFound = -\/(ErrorMsg("Project not found."))

        def step[A](name: String)(f: F[A]): F[A] =
          trace.newSpan(name)(_ => metrics.projectSpaWebSocketStep("load", name)(f))

        def projectData(p: Project): Project \/ VerifiedEvent.Seq = {
          // Now that:
          //   1) projects are cached on the client-side and we only send updates back
          //   2) projects contain all their history
          // the only reason to send a Project instead of events is to save users time to apply all the events and
          // increase the startup time. The size of events is always going to be less than the size of the project.
          val eventsBeingSent = p.ordAsInt - userOrd.fold(0)(_.value)
          if (eventsBeingSent < 1000)
            \/-(p.history.events.takeRight(eventsBeingSent))
          else
            -\/(p)
        }

        def getSupplimentaryData: F[Supplimentary] =
          for {
            rolodex <- runDB(db.getProjectRolodex(pid))
          } yield Supplimentary(rolodex)

        def ignoreCache(c: Redis.ProjectCache): F[MsgError \/ Result] = {

          def readDb(p: Project): F[MsgError \/ (ErrorMsg \/ (Project, InitAppData))] =
            step("readDb")(
              for {
                (mdo, read) <- runDB(
                                 for {
                                   a <- db.getProjectMetaData(pid, uid)
                                   b <- db.getProjectEvents(pid, DB.EventFilter.after(p.ord))
                                 } yield (a, b)
                               )
                result      <- read.traverse(apEvent.append(pid, p, _))
                supp        <- getSupplimentaryData
              } yield
                result match {
                  case \/-(buildResult) =>
                    mdo match {
                      case Some(md) => \/-(buildResult.map(p => p -> InitAppData(projectData(p), md, supp)))
                      case None     => \/-(projectNotFound)
                    }
                  case -\/(e) =>
                    -\/(MsgError.ServerBehindDatabase(e))
                }

            )

          def writeRedis(p: Project): F[Boolean] =
            step("writeRedis")(
              // Maybe write events instead of snapshot here...
              // But really it's going to be such low % that writing events in (TLA+) Load would be worth the logic.
              // The snapshot/event writing decision is much more relevant in (TLA+) Update.
              redis.writeSnapshot(pid, p, VerifiedEvent.Seq.empty)
            )

          for {
            cache  <- c.buildNonEmpty(pid, creator)
            result <- readDb(cache getOrElse Project.init(creator))
            _      <- result match {
                        case \/-(\/-((p, _))) => writeRedis(p)
                        case _                => F.unit
                      }
          } yield result.map(_.map(_._2))
        } // ignoreCache

        def useCache(c: Redis.ProjectCache, md: ProjectMetaData): F[\/-[Result]] =
          step("useCache") {
            for {
              r    <- c.build(pid, creator)
              supp <- getSupplimentaryData
            } yield \/-(r.map(p => InitAppData(projectData(p), md, supp)))
          }

        for {
          cacheRes <- redis.read(pid)
          mdOpt    <- runDB(db.getProjectMetaData(pid, uid))
          r        <- (cacheRes, mdOpt) match {
                     case (\/-(cache), Some(md)) =>
                       if (cache.isCompleteTo(md.latestOrd))
                         useCache(cache, md)
                       else
                         ignoreCache(cache)
                     case (-\/(e), Some(_)) =>
                       if (e.isLocalKnownToBeOutOfDate)
                         F pure -\/(MsgError.ServerBehindRedis(e))
                       else
                         ignoreCache(Redis.ProjectCache.empty)
                     case (_, None) =>
                       F pure \/-(projectNotFound)
                   }
        } yield r.map(MsgFnOut(_, None))
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onReconnect: MsgFn[Option[EventOrd.Latest], StateUpdate] = in => {
        val pid     = in.static.projectId
        val uid     = in.static.user.id
        val userOrd = in.input
        val span    = getSpan(in.static)

        def step[A](name: String)(f: F[A]): F[A] =
          trace.newSpan(name)(_ => metrics.projectSpaWebSocketStep("reconnect", name)(f))

        val redisSubscribe: F[OptionState] =
          in.state.sub match {
            case None =>
              redis.subscribe(pid, listener(in.static, span, in.push, in.onListenerError))
                .map(s => Some(WebSocketState(Some(s))))

            case Some(_) =>
              F.pure(None)
          }

        def loadEventsFromRedis: F[SafePickler.Result[VerifiedEvent.Seq]] =
          step("readRedis")(redis.readEvents(pid, userOrd))

        def loadEventsFromDb(o: Option[EventOrd.Latest]): F[MsgError \/ VerifiedEvent.Seq] =
          step("readDb")(
            runDB(db.getProjectEvents(pid, DB.EventFilter.after(o)))
              .map(_.leftMap(MsgError.ServerBehindDatabase)))

        def loadEvents(dbLatest: Option[EventOrd.Latest]): F[MsgError \/ VerifiedEvent.Seq] =
          loadEventsFromRedis.flatMap {
            case \/-(cachedEvents) =>
              val cachedEventsAreUsable = cachedEvents.nonEmpty && cachedEvents.head.ord.immediatelyFollowsLatest(userOrd)
              if (cachedEventsAreUsable) {
                val newLatest = Some(cachedEvents.last.ord.asLatest)
                if (dbLatest > newLatest)
                  for {
                    dbEventsOrErr <- loadEventsFromDb(newLatest)
                    // Not writing back to Redis for now
                    // * Snapshot could be 1000, this could send events 500-1001 for no reason
                    // * Normal processing (covered by TLA) keeps the cache up-to-date - a reconnection seems orthogonal
                    // _ <- redis.writeEvents(pid, dbEvents, VerifiedEvent.Seq.empty)
                  } yield dbEventsOrErr.map(cachedEvents ++ _)
                else
                  F.pure(\/-(cachedEvents))
              } else
                loadEventsFromDb(userOrd)
            case -\/(err) =>
              if (err.isLocalKnownToBeOutOfDate)
                F pure -\/(MsgError.ServerBehindRedis(err))
              else
                for {
                  _    <- F.delay(logger.warn("Failed to parse cache. Rebuilding. Error: ", err))
                  load <- loadEventsFromDb(userOrd)
                } yield load
          }

        for {
          newState <- redisSubscribe
          mdOpt    <- runDB(db.getProjectMetaData(pid, uid))
          md        = mdOpt.get // This will fail during connection usage after project deleted
          dbLatest  = md.latestOrd
          eventsE  <- if (dbLatest > userOrd)
                        loadEvents(dbLatest)
                      else
                        F.pure(\/-(VerifiedEvent.Seq.empty))
          updateE  <- fRight(eventsE)(es => supplimentaryDataForEvents(es).map(StateUpdate(es, _)))
        } yield updateE.map(MsgFnOut(_, newState))
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onSync: MsgFn[NonEmptySet[EventOrd], Unit] = in => {
        val pid  = in.static.projectId
        val ords = in.input

        for {
          eventsOrErr <- runDB(db.getProjectEvents(pid, DB.EventFilter.Set(ords)))
          result      <- eventsOrErr.traverse(redis.writeEvents(pid, VerifiedEvent.Seq.empty, _))
        } yield result match {
          case \/-(_) => \/-(MsgFnOut((), None))
          case -\/(e) => -\/(MsgError.ServerBehindDatabase(e))
        }
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onUpdateAccess: MsgFn[UpdateAccessCmd, EventResult] = in =>
        UpdateAccessCmd.resolve(in.input)(
          userId     = in.static.userIdPublic,
          getUserId  = u => runDB(db.getUserId(u)).map(_.map(Obfuscators.userId.obfuscate)),
          onNotFound = \/-(MsgFnOut(-\/(ErrorMsg("User not found.")), None)),
          modify     = (m, p) => updateProject(MakeEvent.updateAccess, p)(in.copy(input = m))
        )

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private val supplimentaryDataForEvents: VerifiedEvent.Seq => F[Supplimentary] =
        SupplimentaryLogic[F](
          needUsernamesByUserId = a => runDB(db.needUsernamesByUserId(a)),
          obfuscate             = Obfuscators.userId.obfuscate,
          deobfuscate           = Obfuscators.userId.deobfuscateOrThrow,
        )

      private val projectUpdater: ProjectUpdater[D, F] =
        new ProjectUpdater(writeSnapshotInsteadOfEvents, submitDataPropError, supplimentaryDataForEvents)

    } // new ProjectSpaLogic
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private final case class MsgFnIn[F[_], I](input          : I,
                                            static         : WebSocketStatic,
                                            state          : WebSocketState[F],
                                            push           : BinaryData => F[Unit],
                                            onListenerError: ListenerError => F[Unit])

  private final case class MsgFnOut[F[_], O](output: O, newState: Option[WebSocketState[F]])

  private final class MsgResult[F[_]](reqType: FreeOption[WsReqRes], len: Long, val newState: Option[WebSocketState[F]]) {
    def msgType : String  = reqType.fold("unknown", _.name)
    def ok      : Boolean = len >= 0
    def bytesOut: Long    = if (ok) len else 0
  }

  private final class ProjectUpdater[D[_], F[_]](writeSnapshot: Int => Boolean,
                                                 submitDataPropError: (UserId, ErrorMsg) => F[Unit],
                                                 supplimentaryDataForEvents: VerifiedEvent.Seq => F[Supplimentary])
                                                (implicit
                                                 F       : Sync[F],
                                                 apEvent : ApplyEventAlgebra[F],
                                                 db      : DB.ForProjectSpa[D],
                                                 metrics : MetricsAlgebra[F],
                                                 redis   : Redis.ProjectAlgebra[F],
                                                 runDB   : D ~> F,
                                                 trace   : Trace.Algebra[F]) {
    import ProjectUpdater._

    def apply(pid         : ProjectId,
              creator     : ProjectCreator,
              userId      : UserId,
              mkEvent     : Project => MakeEvent.Result,
              requiredRole: ProjectRole,
             ): F[Result] = {

      var gas = 200

      def loop(s: State): F[State \/ Result] = {
        import Status._

        if (gas > 0) gas -= 1 else throw new IllegalStateException(s"Infinite loop! state=$s")

        val main: F[State \/ Result] = s.status match {

          case ReadRedis =>
            def useCache(cache: Redis.ProjectCache): F[State \/ Result] =
              cache.build(pid, creator).map {
                case \/-(p) => -\/(s.copy(local = p, redis = cache, status = if (cache.ord > s.local.ord) WriteDb else ReadDb))
                case -\/(e) => \/-(Result.Reject(e))
              }
            redis.read(pid).flatMap {
              case \/-(cache) => useCache(cache)
              case -\/(err) =>
                if (err.isLocalKnownToBeOutOfDate)
                  F pure \/-(Result.ServerBehindRedis(err))
                else
                  F.delay {
                    logger.warn("Failed to parse cache. Rebuilding. Error: ", err)
                  } >> useCache(Redis.ProjectCache.empty)
            }

          case ReadDb =>
            for {
              cacheBuilt     <- s.redis.buildNonEmpty(pid, creator)
              p1             = s.local max cacheBuilt
              newEventsOrErr <- runDB(db.getProjectEvents(pid, DB.EventFilter.after(p1.ord)))
              result         <- newEventsOrErr match {
                                 case \/-(newEvents) => apEvent.append(pid, p1, newEvents) map {
                                   case \/-(p2) => -\/(s.copy(local = p2, status = WriteRedis1(newEvents)))
                                   case -\/(e)  => \/-(Result.Reject(e))
                                 }
                                 case -\/(e) => F pure \/-(Result.ServerBehindDatabase(e))
                               }
            } yield result

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
            val project = s.local
            val userPubId = Obfuscators.userId.obfuscate(userId)

            // This logic is duplicated in TestGlobal
            def permCheck: PotentialChange[ErrorMsg, Unit] =
              project.access.require(requiredRole, userPubId) match {
                case Allow => PotentialChange.unit
                case Deny  => PotentialChange.Failure(requiredRole.errorMsgWhenUnsatisfied)
              }

            val result: PotentialChange[ErrorMsg, ApplyNewEvent.Updated] =
              for {
                _ <- permCheck
                e <- mkEvent(project)
                u <- ApplyNewEvent(e, project)
              } yield u

            result match {
              case PotentialChange.Success(updated) =>
                val f = db.saveProjectEvent(pid, s.local.history.nextOrd, updated.event, updated.projectPartial, userId)
                val run = updated.event match {
                  case _: Event.AccessUpdate =>
                    // updateProjectAccess in DbInterpreter has assertTransactionLevelSerializable
                    db.inStrictTxn(runDB)(f)
                  case _ =>
                    runDB(f)
                }
                run map {
                  case \/-(ve) =>
                    val p2 = updated.completeProject(ve)
                    val nextStatus = WriteRedis2(p2, ve)
                    -\/(s.copy(status = nextStatus))
                  case -\/(DB.SaveProjectEventError.OrdInUse) =>
                    -\/(s.copy(status = ReadRedis))
                  case -\/(DB.SaveProjectEventError.OnAccess.CantRemoveLastAdmin) =>
                    \/-(Result.Reject(userFacingErrorMsgCantRemoveAdmin))
                }

              case PotentialChange.Unchanged =>
                F pure \/-(Result.Ok(StateUpdate.empty))

              case PotentialChange.Failure(e) =>
                ApplyEvent.propertyFailure(e) match {
                  case Some(propFailure) =>
                    for {
                      _ <- submitDataPropError(userId, propFailure)
                    } yield \/-(Result.Reject(userFacingErrorMsgWhenDataPropFails))
                  case None =>
                    F pure \/-(Result.Reject(e))
                }
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
              _    <- writeRedis
              supp <- supplimentaryDataForEvents(newEvents)
            } yield \/-(Result.Ok(StateUpdate(newEvents, supp)))
        }

        trace.newSpan(s.status.name)(_ =>
          metrics.projectSpaWebSocketStep("update", s.status.name)(
            main))
      }

      F.tailRecM(initialState(creator))(loop)
    }
  }

  private object ProjectUpdater {

    sealed trait Result
    object Result {
      final case class ServerBehindDatabase(err: DB.ReadProjectEventError)    extends Result
      final case class ServerBehindRedis   (err: SafePickler.DecodingFailure) extends Result
      final case class Reject              (errMsg: ErrorMsg)                 extends Result
      final case class Ok                  (update: StateUpdate)              extends Result
    }

    final case class State(local : Project,
                           redis : Redis.ProjectCache,
                           status: Status)

    def initialState(creator: ProjectCreator) = State(
      local  = Project.init(creator),
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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** Generates code to paste into ProjectSpaProtocolsTest.scala */
  protected object GenerateUnitTest {
    import io.circe._
    import io.circe.syntax._
    import org.apache.commons.text.StringEscapeUtils
    import scala.collection.immutable.TreeSet
    import ProjectSpaProtocols.WebSocket
    import shipreq.webapp.member.project.protocol.json.Latest.encoderVerifiedEvent

    type WSH = WebSocketServerHelper[WebSocket#Req, WebSocket.Push]

    def req(webSocketHelper: WSH, msg: BinaryData)(reqId: WebSocketShared.ReqId, req: WebSocket#Req): Unit =
      println(
        s"""
           |"${req.toString.takeWhile(_ != '.')}" - {
           |  "req" - {
           |    "${webSocketHelper.protocolCS.codec.version.verStr}" - {
           |      val bin    = BinaryData.fromHex("${msg.hex}")
           |      val expect = ${(reqId, req)}
           |      assertRequest(bin, expect)
           |    }
           |  }
           |}
           |""".stripMargin)

    // InitApp               - Unit                         - ErrorMsg \/ InitAppData
    // Reconnect             - Option[EventOrd.Latest]      - VerifiedEvent.Seq
    // Sync                  - NonEmptySet[EventOrd]        - Unit
    // UpdateConfig          - UpdateConfigCmd              - ErrorMsg \/ VerifiedEvent.Seq
    // CreateContent         - CreateContentCmd             - ErrorMsg \/ VerifiedEvent.Seq
    // UpdateContent         - UpdateContentCmd             - ErrorMsg \/ VerifiedEvent.Seq
    // ProjectNameSet        - String                       - ErrorMsg \/ VerifiedEvent.Seq
    // UpdateSavedViews      - SavedViewCmd                 - ErrorMsg \/ VerifiedEvent.Seq
    // UpdateManualIssues    - ManualIssueCmd               - ErrorMsg \/ VerifiedEvent.Seq
    // FieldMandatorinessMod - (CustomFieldId, Mandatory)   - ErrorMsg \/ VerifiedEvent.Seq
    // ReqTypeImplicationMod - (CustomReqTypeId, Mandatory) - ErrorMsg \/ VerifiedEvent.Seq

    private def asJsonStr[A: Encoder](a: A): String =
      "\"\"\"" + a.asJson.noSpacesSortKeys + "\"\"\""

    private final case class WrapVerifiedEvents(ves: VerifiedEvent.Seq) {
      override def toString = {
        def jsons = ves.iterator.map(asJsonStr(_))
        if (ves.isEmpty)
          "VerifiedEvent.Seq.empty"
        else if (ves.size == 1)
          jsons.mkString("verifiedEventsFromJson(", ", ", ")")
        else
          jsons.mkString("verifiedEventsFromJson(\n", ",\n", ")")
      }
    }

    private def show(input: Any): Any = input match {
      case t: TreeSet[_] => WrapVerifiedEvents(t.asInstanceOf[VerifiedEvent.Seq])
      case \/-(a)        => \/-(show(a))
      case (a, b)        => (a, show(b))
      case s: String     => StringEscapeUtils.escapeJava(s)
      case a             => a
    }

    def resp(webSocketHelper: WSH, req: WsReqRes.AndReq, resp: WebSocketShared.ServerToClient[_])(out: BinaryData): Unit = {
      val name = req.toString.takeWhile(_ != '.')
      val resp2  = resp.map { case (reqId, protocolAndValue) => (reqId, protocolAndValue.value) }
      val respCode = show(resp2).toString

      println(
        s"""
           |"$name" - {
           |  "resp" - {
           |    "${webSocketHelper.protocolSC.codec.version.verStr}" - {
           |      val bin    = BinaryData.fromHex("${out.hex}")
           |      val expect = $respCode
           |      assertResponse($name)(bin, expect)
           |    }
           |  }
           |}
           |""".stripMargin)
    }
  }
}
