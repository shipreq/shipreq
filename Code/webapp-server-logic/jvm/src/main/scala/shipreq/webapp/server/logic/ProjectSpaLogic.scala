package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.univeq._
import java.time.{Duration, Instant}
import scalaz.syntax.monad._
import scalaz.{-\/, BindRec, Monad, \/, \/-, ~>}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.data.{Obfuscated, Project, ProjectId, ProjectMetaData}
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.event.EventOrd.Implicits._
import shipreq.webapp.base.protocol.entrypoint.ProjectSpaEntryPoint
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.protocol.websocket.ProjectSpaProtocols.WsReqRes.EventResult
import shipreq.webapp.base.protocol.websocket.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.user.{User, UserId, Username}
import shipreq.webapp.server.logic.dispatch.Cookie

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def initPage(projectId: ProjectId, username: Username): F[ProjectSpaEntryPoint.InitData]

  def onConnect(cookies  : Cookie.LookupFn,
                projectId: ProjectId.Public): F[ConnectRejection \/ (WebSocketStatic, WebSocketState[F])]

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
                                   sessionId  : Security.SessionId,
                                   span       : Any,
                                   connectedAt: Instant,
                                   expiresAt  : Instant)

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
    case object ProjectNotFound  extends ConnectRejection
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
            owner   <- C.optionF(security.db.getProjectOwner(pid), ProjectNotFound)
            _       <- C.ensure(user.id ==* owner, AccessDenied)
            now     <- C.rightF(svr.now)
          } yield {
            val static = WebSocketStatic(
                           user        = user,
                           projectId   = pid,
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
                             case -\/(ProjectNotFound ) => "ProjectNotFound"
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
            _ <- F.point(logger.warn("Failed to parse pub/sub event from Redis: ", err))
            _ <- onError(ListenerError.RedisDecodingFailure(err))
          } yield ()

        case -\/(Redis.ListenerError.RedisLibraryException(err)) =>
          for {
            _ <- F.point(logger.warn("Failed to parse pub/sub event from Redis: ", err))
            _ <- onError(ListenerError.RedisLibraryException(err))
          } yield ()
      }

      private def pushEvent(span: Span, push: BinaryData => F[Unit], e: VerifiedEvent): F[Unit] =
        pushEvents(span, push, VerifiedEvent.NonEmptySeq.one(e))

      private def pushEvents(span: Span, push: BinaryData => F[Unit], es: VerifiedEvent.NonEmptySeq): F[Unit] =
        trace.newSubSpan("push", span) { _ =>
          for {
            msgBin <- F point webSocketHelper.protocolSC.codec.encode(-\/(es))
            _      <- metrics.projectSpaWebSocketPush(msgBin.length)
            _      <- push(msgBin)
          } yield ()
        }

      private def hasExpired(static: WebSocketStatic): F[Boolean] =
        svr.now.map(_.isAfter(static.expiresAt))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      override def onClose(staticO: Option[WebSocketStatic],
                           stateO : Option[WebSocketState[F]]) = {

        val main: F[Unit] =
          stateO.flatMap(_.sub).fold(fUnit)(_.unsubscribe)

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
        F.point {
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
            fUnit
        }

      override def onMessage(static         : WebSocketStatic,
                             state          : WebSocketState[F],
                             msg            : BinaryData,
                             respond        : BinaryData => F[Throwable \/ Unit],
                             push           : BinaryData => F[Unit],
                             onListenerError: ListenerError => F[Unit],
                             onError        : MsgError => F[Unit]): F[OptionState] = {
        val M = OnMsgError

        val span = getSpan(static)

        def body(implicit span: Span): F[MsgResult[F]] = {

          def handleError(wsReqRes: FreeOption[WsReqRes], err: MsgError): F[MsgResult[F]] =
            onError(err) >> logError(err, () => msg.describe(1000)) >| new MsgResult(wsReqRes, -1L, None)

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
                    case \/-(_) => F.point(new MsgResult(wsReqRes, resBin.length, msgFnOut.newState))
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
          _        <- F.point(logger.info(s"WebSocket for project #$pid processed request in ${dur.conciseDesc}"))
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
        onFieldMandatorinessMod = _ => F.pure(-\/(MsgError.FunctionNoLongerSupported("fieldMandatorinessMod"))),
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
        in => projectUpdater(in.static.projectId, in.static.user.id, mkEvent(in.input, _)).map {
          case ProjectUpdater.Result.Ok(events)              => \/-(MsgFnOut(\/-(events), None))
          case ProjectUpdater.Result.Reject(e)               => \/-(MsgFnOut(-\/(e), None))
          case ProjectUpdater.Result.ServerBehindDatabase(e) => -\/(MsgError.ServerBehindDatabase(e))
          case ProjectUpdater.Result.ServerBehindRedis(e)    => -\/(MsgError.ServerBehindRedis(e))
        }

      private def updateProjectI[I](mkEvent: I => MakeEvent.Result): MsgFn[I, EventResult] =
        updateProject((i, _) => mkEvent(i))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onInitApp: MsgFn[Unit, ErrorMsg \/ InitAppData] = in => {
        val pid = in.static.projectId

        type Result = ErrorMsg \/ InitAppData

        def projectNotFound = -\/(ErrorMsg("Project not found."))

        def step[A](name: String)(f: F[A]): F[A] =
          trace.newSpan(name)(_ => metrics.projectSpaWebSocketStep("load", name)(f))

        def ignoreCache(c: Redis.ProjectCache): F[MsgError \/ Result] = {

          def readDb(p: ProjectAndOrd): F[MsgError \/ Result] =
            step("readDb")(
              for {
                (mdo, read) <- runDB(
                                 for {
                                   a <- db.getProjectMetaData(pid)
                                   b <- db.getProjectEvents(pid, DB.EventFilter.given(p.ord))
                                 } yield (a, b)
                               )
                result      <- read.traverse(apEvent.append(pid, p, _))
              } yield
                result match {
                  case \/-(buildResult) =>
                    mdo match {
                      case Some(md) => \/-(buildResult.map(InitAppData(_, md)))
                      case None     => \/-(projectNotFound)
                    }
                  case -\/(e) =>
                    -\/(MsgError.ServerBehindDatabase(e))
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
            cache  <- c.buildNonEmpty(pid)
            result <- readDb(cache getOrElse ProjectAndOrd.empty)
            _      <- result match {
                        case \/-(\/-(d)) => writeRedis(d)
                        case _           => fUnit
                      }
          } yield result
        }

        def useCache(c: Redis.ProjectCache, md: ProjectMetaData): F[\/-[Result]] =
          step("useCache") {
            for {
              r <- c.build(pid)
            } yield \/-(r.map(InitAppData(_, md)))
          }

        for {
          cacheRes <- redis.read(pid)
          mdOpt    <- runDB(db.getProjectMetaData(pid))
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

      private def onReconnect: MsgFn[Option[EventOrd.Latest], VerifiedEvent.Seq] = in => {
        val pid     = in.static.projectId
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
            runDB(db.getProjectEvents(pid, DB.EventFilter.given(o)))
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
                  _    <- F.point(logger.warn("Failed to parse cache. Rebuilding. Error: ", err))
                  load <- loadEventsFromDb(userOrd)
                } yield load
          }

        for {
          newState <- redisSubscribe
          mdOpt    <- runDB(db.getProjectMetaData(pid))
          md       = mdOpt.get // This will fail during connection usage after project deleted
          dbLatest = md.latestOrd
          events   <- if (dbLatest > userOrd)
                       loadEvents(dbLatest)
                     else
                       F.pure(\/-(VerifiedEvent.Seq.empty))
        } yield events.map(MsgFnOut(_, newState))
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

  private final class ProjectUpdater[D[_], F[_]](writeSnapshot: Int => Boolean)
                                                (implicit
                                                 F       : Monad[F] with BindRec[F],
                                                 apEvent : ApplyEventLogic[F],
                                                 db      : DB.ForProjectSpa[D],
                                                 metrics : MetricsLogic[F],
                                                 redis   : Redis.ProjectAlgebra[F],
                                                 runDB   : D ~> F,
                                                 trace   : Trace.Algebra[F]) {
    import ProjectUpdater._

    def apply(pid: ProjectId, userId: UserId, mkEvent: Project => MakeEvent.Result): F[Result] = {
      var gas = 200

      def loop(s: State): F[State \/ Result] = {
        import Status._

        if (gas > 0) gas -= 1 else throw new IllegalStateException(s"Infinite loop! state=$s")

        val main: F[State \/ Result] = s.status match {

          case ReadRedis =>
            def useCache(cache: Redis.ProjectCache): F[State \/ Result] =
              cache.build(pid).map {
                case \/-(p) => -\/(s.copy(local = p, redis = cache, status = if (cache.ord > s.local.ord) WriteDb else ReadDb))
                case -\/(e) => \/-(Result.Reject(e))
              }
            redis.read(pid).flatMap {
              case \/-(cache) => useCache(cache)
              case -\/(err) =>
                if (err.isLocalKnownToBeOutOfDate)
                  F pure \/-(Result.ServerBehindRedis(err))
                else
                  F.point {
                    logger.warn("Failed to parse cache. Rebuilding. Error: ", err)
                  } >> useCache(Redis.ProjectCache.empty)
            }

          case ReadDb =>
            for {
              cacheBuilt     <- s.redis.buildNonEmpty(pid)
              p1             = s.local max cacheBuilt
              newEventsOrErr <- runDB(db.getProjectEvents(pid, DB.EventFilter.given(p1.ord)))
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
            mkEvent(s.local.project).flatMap(ApplyNewEvent(_, s.local.project)) match {
              case PotentialChange.Success(updated) =>
                runDB(db.saveProjectEvent(pid, s.local.nextOrd, updated.event, updated.project, userId)) map {
                  case \/-(ve) =>
                    val nextStatus = WriteRedis2(updated.project, ve)
                    -\/(s.copy(status = nextStatus))
                  case -\/(DB.SaveProjectEventError.OrdInUse) =>
                    -\/(s.copy(status = ReadRedis))
                }

              case PotentialChange.Unchanged =>
                F pure \/-(Result.Ok(VerifiedEvent.Seq.empty))

              case PotentialChange.Failure(e) =>
                F pure \/-(Result.Reject(ErrorMsg(e)))
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
            } yield \/-(Result.Ok(newEvents))
        }

        trace.newSpan(s.status.name)(_ =>
          metrics.projectSpaWebSocketStep("update", s.status.name)(
            main))
      }

      F.tailrecM(loop)(initialState)
    }
  }

  private object ProjectUpdater {

    sealed trait Result
    object Result {
      final case class ServerBehindDatabase(err: DB.ReadProjectEventError)    extends Result
      final case class ServerBehindRedis   (err: SafePickler.DecodingFailure) extends Result
      final case class Reject              (errMsg: ErrorMsg)                 extends Result
      final case class Ok                  (events: VerifiedEvent.Seq)        extends Result
    }

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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** Generates code to paste into ProjectSpaProtocolsTest.scala */
  private[this] object GenerateUnitTest {
    import io.circe._
    import io.circe.syntax._
    import org.apache.commons.text.StringEscapeUtils
    import scala.collection.immutable.TreeSet
    import shipreq.webapp.base.protocol.json.v1.Rev1.encoderVerifiedEvent
    import ProjectSpaProtocols.WebSocket

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
