package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.univeq._
import scala.util.{Failure, Success}
import scalaz.syntax.monad._
import scalaz.{-\/, BindRec, Monad, \/, \/-, ~>}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.data.{Obfuscated, Project, ProjectId, ProjectMetaData}
import shipreq.webapp.base.event.{ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.EventResult
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, InitPageData, WsReqRes}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.user.{User, Username}

// TODO metrics

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def initPage(projectId: ProjectId, username: Username): F[InitPageData]

  def onConnect(cookies  : Cookie.LookupFn,
                projectId: ProjectId.Public): F[ConnectRejection \/ (WebSocketStatic, WebSocketState[F])]

  def onOpen(static: WebSocketStatic,
             state : WebSocketState[F],
             push  : BinaryData => F[Unit]): F[WebSocketState[F]]

  def onMessage(static : WebSocketStatic,
                msg    : BinaryData,
                respond: BinaryData => F[Throwable \/ Unit],
                onError: MsgError => F[Unit]): F[Unit]

  def onClose(static: WebSocketStatic,
              state : WebSocketState[F]): F[Unit]
}

object ProjectSpaLogic extends StrictLogging {

  final case class WebSocketStatic(user: User, projectId: ProjectId, span: Any)

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

  def apply[D[_], F[_]](implicit
                        D       : Monad[D],
                        F       : Monad[F] with BindRec[F],
                        redis   : Redis.ProjectAlgebra[F],
                        db      : DB.ForProjectSpa[D],
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

      override def initPage(pid: ProjectId, username: Username): F[InitPageData] =
        for {
          name <- runDB(db.projectSpaInitPage(pid))
        } yield {
          val pidPub = Obfuscators.projectId.obfuscate(pid)
          InitPageData(username, pidPub, name)
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
          } yield {
            val static = WebSocketStatic(user, pid, span)
            val state  = WebSocketState.empty[F]
            (static, state)
          }

        trace.newSpan("WebSocket")(span =>
          trace.newSubSpan("onConnect", span)(_ =>
            security.protect(
              main(span).value)))
      }

      override def onOpen(static: WebSocketStatic,
                          state: WebSocketState[F],
                          push : BinaryData => F[Unit]) = {
        val span = getSpan(static)
        val main: F[WebSocketState[F]] =
          state.sub match {
            case None =>
              for {
                sub <- redis.subscribe(static.projectId, pushEvent(span, push, _))
              } yield WebSocketState(Some(sub))
            case Some(_) =>
              F.pure(state)
          }
        trace.newSubSpan("onOpen", span)(_ => main)
      }

      override def onClose(static: WebSocketStatic,
                           state : WebSocketState[F]) =
        trace.newSubSpan("onClose", getSpan(static))(_ =>
          state.sub match {
            case Some(sub) => sub.unsubscribe
            case None      => fUnit
          }
        )

      private def pushEvent(span: Span, push: BinaryData => F[Unit], e: VerifiedEvent): F[Unit] =
        pushEvents(span, push, VerifiedEvent.NonEmptySeq.one(e))

      private def pushEvents(span: Span, push: BinaryData => F[Unit], es: VerifiedEvent.NonEmptySeq): F[Unit] =
        trace.newSubSpan("push", span) { _ =>
          val msgBin = BinaryJvm.encode(webSocketHelper.protocolSC)(-\/(es))
          push(msgBin)
        }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // Message responding

      override def onMessage(static : WebSocketStatic,
                             msg    : BinaryData,
                             respond: BinaryData => F[Throwable \/ Unit],
                             onError: MsgError => F[Unit]): F[Unit] = {
        val M = OnMsgError

        val span = getSpan(static)

        def main(implicit span: Span): M.Result[Unit] =
          for {
            (reqId, req)   ← M.lift(parseMsg(msg))
            _              ← M.rightF(trace.rename("onMessage: " + req.reqRes.name))
            res            ← M.rightF(msgFold(req.reqRes)((req.req, static)))
            protocolAndRes = req.reqRes.protocolRes.andValue(res)
            fullRes        = \/-((reqId, protocolAndRes))
            resBin         = BinaryJvm.encode(webSocketHelper.protocolSC)(fullRes)
            _              ← M(respond(resBin).map(_.leftMap[MsgError](MsgError.RespondError)))
          } yield ()

        val handleError: MsgError \/ Unit => F[Unit] = {
          case \/-(_) =>
            fUnit

          case -\/(m @ MsgError.DecodingFailure) =>
            F.point(logger.warn(s"Failed to decode message: ${msg.describe(1000)}")) >> onError(m)

          case -\/(m @ MsgError.RespondError(err)) =>
            F.point(logger.error(s"Error sending response.", err)) >> onError(m)
        }

        for {
          (_, dur) ← svr.measureDuration(trace.newSubSpan("onMessage", span)(main(_).value.flatMap(handleError)))
          durMs    = dur.toMillis
          pid      = static.projectId.value
          _        ← F.point(logger.info(s"WebSocket for project #$pid processed request in $durMs ms"))
        } yield ()
      }

      private def parseMsg(msg: BinaryData) = {
        BinaryJvm.decode(msg, webSocketHelper.protocolCS) match {
          case Success(r) => \/-(r)
          case Failure(_) => -\/(MsgError.DecodingFailure)
        }
      }

      private type MsgFnIn[I] = (I, WebSocketStatic)
      private type MsgFnOut[O] = F[O]
      private type MsgFn[I, O] = (I, WebSocketStatic) => MsgFnOut[O]
      private type MsgFoldIn[R <: WsReqRes] = MsgFnIn[R#RequestType]
      private type MsgFoldOut[R <: WsReqRes] = MsgFnOut[R#ResponseType]

      private val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
        onInitApp               = onInitApp.tupled,
        onCreateContent         = updateProject (MakeEvent.createContent),
        onUpdateContent         = updateProject (MakeEvent.updateContent),
        onProjectNameSet        = updateProjectI(MakeEvent.projectNameSetFn),
        onUpdateSavedViews      = updateProject (MakeEvent.updateSavedViews),
        onFieldMandatorinessMod = updateProjectI(MakeEvent.fieldMandatorinessMod),
        onReqTypeImplicationMod = updateProjectI(MakeEvent.reqTypeImplicationMod),
        onCustomIssueTypeCrud   = updateProject (MakeEvent.customIssueTypeCrud),
        onCustomReqTypeCrud     = updateProject (MakeEvent.customReqTypeCrud),
        onFieldMod              = updateProject (MakeEvent.fieldCrud),
        onTagMod                = updateProject (MakeEvent.tagCrud),
      )

      private def updateProject[I](mkEvent: (I, Project) => MakeEvent.Result): MsgFnIn[I] => MsgFnOut[EventResult] = input => {
        val i = input._1
        val pid = input._2.projectId
        ProjectUpdater[D, F](pid, mkEvent(i, _))
      }

      private def updateProjectI[I](mkEvent: I => MakeEvent.Result): MsgFnIn[I] => MsgFnOut[EventResult] =
        updateProject((i, _) => mkEvent(i))

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // InitApp

      private def onInitApp: MsgFn[Unit, ErrorMsg \/ InitAppData] = (_, static) => {
        val pid = static.projectId

        type Result = ErrorMsg \/ InitAppData

        def projectNotFound = -\/(ErrorMsg("Project not found."))

        def ignoreCache(c: Redis.ProjectCache): F[Result] = {

          def readDb(p: ProjectAndOrd) =
          trace.newSpan("readDb")(_ =>
            runDB(
              db.inDbTransaction(for {
                md <- db.getProjectMetaData(pid)
                es <- db.getProjectEvents(pid, DB.EventFilter.given(p.ord))
              } yield (es, md))
            ).map {
              case (es, Some(md)) =>
                // Build outside of DB transaction
                trace.newSpanImpure("ApplyEvents")(_ =>
                  ApplyEvents.append(pid, p, es).map(InitAppData(_, md)))
              case (_, None) =>
                projectNotFound
            }
          )

          def writeRedis(i: InitAppData): F[Boolean] =
            trace.newSpan("writeRedis")(_ =>
              // TODO Maybe write events instead of snapshot
              redis.writeSnapshot(pid, i.project, VerifiedEvent.Seq.empty))

            for {
              result <- readDb(c.nonEmptyCompleteBuild(pid) getOrElse ProjectAndOrd.empty)
              _      <- result.fold[F[_]](_ => fUnit, writeRedis)
            } yield result
        }

        def useCache(c: Redis.ProjectCache, md: ProjectMetaData): F[Result] =
          trace.newSpan("useCache") { _ =>
            val result = c.build(pid).map(InitAppData(_, md))
            F pure result
          }

        for {
          cache <- redis.read(pid)
          mdOpt <- runDB(db.getProjectMetaData(pid))
          r     <- mdOpt match {
                     case Some(md) => if (cache.isCompleteTo(md.latestOrd)) useCache(cache, md) else ignoreCache(cache)
                     case None     => F pure projectNotFound
                   }
        } yield r
      }

    } // new ProjectSpaLogic
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ProjectUpdater {

    type Result = ErrorMsg \/ VerifiedEvent.Seq

    def apply[D[_], F[_]](pid: ProjectId,
                          mkEvent: Project => MakeEvent.Result)
                         (implicit
                          D       : Monad[D],
                          F       : Monad[F] with BindRec[F],
                          redis   : Redis.ProjectAlgebra[F],
                          db      : DB.ForProjectSpa[D],
                          runDB   : D ~> F,
                          security: Security.Algebra[F],
                          trace   : Trace.Algebra[F]): F[Result] = {

      def loop(s: State): F[State \/ Result] = {
        import Status._
        val main: F[State \/ Result] = s.status match {

          case ReadRedis =>
            for {
              r0 <- redis.read(pid)
            } yield {
              val r = r0.filterComplete
              r.build(pid) match {
                case \/-(p) => -\/(s.copy(local = p, redis = r, status = if (r > s.local.ord) WriteDb else ReadDb))
                case -\/(e) => \/-(-\/(e))
              }
            }

          case ReadDb =>
            val p1 = s.local max s.redis.nonEmptyCompleteBuild(pid)
            for {
              newEvents <- runDB(db.getProjectEvents(pid, DB.EventFilter.given(p1.ord)))
            } yield
              ApplyEvents.append(pid, p1, newEvents) match {
                case \/-(p2) => -\/(s.copy(local = p2, status = WriteRedis1))
                case -\/(e)  => \/-(-\/(e))
              }

          case WriteRedis1 =>
            for {
              // TODO Maybe write events instead of snapshot
              ok <- redis.writeSnapshot(pid, s.local, VerifiedEvent.Seq.empty)
            } yield -\/(s.copy(status = if (ok) WriteDb else ReadRedis))

          case WriteDb =>
            mkEvent(s.local.project).flatMap(ApplyNewEvent(_, s.local.project)) match {
              case PotentialChange.Success(updated) =>
                val saveCmd = DB.SaveProjectEventCmd(s.local.nextOrd, updated.event, updated.hashRecs)
                runDB(db.saveProjectEvent(pid, saveCmd)) map {
                  case \/-(ve) =>
                    -\/(s.copy(status = WriteRedis2(VerifiedEvent.Seq.one(ve))))
                  case -\/(_) =>
                    -\/(s.copy(status = ReadRedis))
                }

              case PotentialChange.Unchanged =>
                F pure \/-(\/-(VerifiedEvent.Seq.empty))

              case PotentialChange.Failure(e) =>
                F pure \/-(-\/(ErrorMsg(e)))
            }

          case WriteRedis2(newEvents) =>
            for {
              // TODO Maybe write snapshot instead of events
              ok <- redis.writeEvents(pid, VerifiedEvent.Seq.empty, newEvents)
            } yield \/-(\/-(newEvents))
        }

        trace.newSpan(s.status.name)(_ => main)
      }

      F.tailrecM(loop)(initialState)
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
      case object ReadRedis                                      extends Status("ReadRedis")
      case object ReadDb                                         extends Status("ReadDb")
      case object WriteRedis1                                    extends Status("WriteRedis1")
      case object WriteDb                                        extends Status("WriteDb")
      final case class WriteRedis2(newEvents: VerifiedEvent.Seq) extends Status("WriteRedis2")
    }
  }
}
