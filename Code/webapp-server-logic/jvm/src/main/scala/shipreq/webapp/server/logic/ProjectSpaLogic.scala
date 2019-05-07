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

// TODO Logging, timing, tracing, metrics

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def initPage(projectId: ProjectId, username: Username): F[InitPageData]

  def onConnect(cookies  : Cookie.LookupFn,
                projectId: ProjectId.Public): F[ConnectRejection \/ (WebSocketStatic, WebSocketState[F])]

  def onOpen(static: WebSocketStatic,
             state: WebSocketState[F],
             push : BinaryData => F[Unit]): F[WebSocketState[F]]

  def onMessage(static: WebSocketStatic,
                msg   : BinaryData): F[MsgError \/ BinaryData]

  def onClose(state : WebSocketState[F]): F[Unit]
}

object ProjectSpaLogic extends StrictLogging {

  final case class WebSocketStatic(user: User, projectId: ProjectId)
  object WebSocketStatic {
    implicit def univEq: UnivEq[WebSocketStatic] = UnivEq.derive
  }

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
    implicit def univEq: UnivEq[MsgError] = UnivEq.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  def apply[D[_], F[_]](implicit
                        D       : Monad[D],
                        F       : Monad[F] with BindRec[F],
                        redis   : Redis.ProjectAlgebra[F],
                        db      : DB.ForProjectSpa[D],
                        runDB   : D ~> F,
                        security: Security.Algebra[F],
                        trace   : Trace.Algebra[F]): ProjectSpaLogic[F] = {

    val webSocketHelper = WebSocketServerHelper(ProjectSpaProtocols.WebSocket(Obfuscated(null)))

    val OnConnect  = Monads.FDisj[F, ConnectRejection]
    val OnMsgError = Monads.FDisj[F, MsgError]

    val fUnit = F.pure(())

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

        val main: C.Result[(WebSocketStatic, WebSocketState[F])] =
          for {
            pid     <- C.lift(Obfuscators.projectId.deobfuscate(projectId).leftMap(_ => InvalidProjectId))
            session <- C.optionF(security.sessionRestore(cookies), NoSession)
            user    <- C.option(session.authenticatedUser, AnonymousSession)
            owner   <- C.optionF(security.db.getProjectOwner(pid), ProjectNotFound)
            _       <- C.ensure(user.id ==* owner, AccessDenied)
          } yield {
            val static = WebSocketStatic(user, pid)
            val state  = WebSocketState.empty[F]
            (static, state)
          }

        security.protect(main.value)
      }

      override def onOpen(static: WebSocketStatic,
                          state: WebSocketState[F],
                          push : BinaryData => F[Unit]) =
        state.sub match {
          case None =>
            for {
              sub <- redis.subscribe(static.projectId, pushEvent(push, _))
            } yield WebSocketState(Some(sub))
          case Some(_) =>
            F.pure(state)
        }

      override def onClose(state : WebSocketState[F]) =
        state.sub match {
          case Some(sub) => sub.unsubscribe
          case None      => fUnit
        }

      private def pushEvent(push: BinaryData => F[Unit], e: VerifiedEvent): F[Unit] =
        pushEvents(push, VerifiedEvent.NonEmptySeq.one(e))

      private def pushEvents(push: BinaryData => F[Unit], es: VerifiedEvent.NonEmptySeq): F[Unit] = {
        val msgBin = BinaryJvm.encode(webSocketHelper.protocolSC)(-\/(es))
        push(msgBin)
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // Message responding

      override def onMessage(static: WebSocketStatic,
                             msg   : BinaryData) = {
        val M = OnMsgError

        val main: M.Result[BinaryData] =
          for {
            (reqId, req)  <- M.lift(parseMsg(msg))
             res          <- M.rightF(msgFold(req.reqRes)((req.req, static)))
          } yield {
            val protocolAndRes = req.reqRes.protocolRes.andValue(res)
            val fullRes        = \/-((reqId, protocolAndRes))
            val resBin         = BinaryJvm.encode(webSocketHelper.protocolSC)(fullRes)
            resBin
          }

        main.value
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
            runDB(
              db.inDbTransaction(for {
                md <- db.getProjectMetaData(pid)
                es <- db.getProjectEvents(pid, DB.EventFilter.given(p.ord))
              } yield (es, md))
            ).map {
              case (es, Some(md)) =>
                // Build outside of DB transaction
                ApplyEvents.append(pid, p, es).map(InitAppData(_, md))
              case (_, None) =>
                projectNotFound
            }

          def writeRedis(i: InitAppData): F[Boolean] =
            // TODO Maybe write events instead of snapshot
            redis.writeSnapshot(pid, i.project, VerifiedEvent.Seq.empty)

            for {
              result <- readDb(c.nonEmptyCompleteBuild(pid) getOrElse ProjectAndOrd.empty)
              _      <- result.fold[F[_]](_ => fUnit, writeRedis)
            } yield result
        }

        def useCache(c: Redis.ProjectCache, md: ProjectMetaData): F[Result] = {
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
        s.status match {

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

    sealed trait Status
    object Status {
      case object ReadRedis                                      extends Status
      case object ReadDb                                         extends Status
      case object WriteRedis1                                    extends Status
      case object WriteDb                                        extends Status
      final case class WriteRedis2(newEvents: VerifiedEvent.Seq) extends Status
    }
  }
}
