package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.univeq._
import scala.util.{Failure, Success}
import scalaz.{-\/, Monad, \/, \/-, ~>}
import scalaz.syntax.monad._
import shipreq.base.util.{BinaryData, ErrorMsg, Monads}
import shipreq.webapp.base.data.{Obfuscated, Project, ProjectId}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol2.ProjectSpaProtocols.WsReqRes.EventResult
import shipreq.webapp.base.protocol2.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol2.{BinaryJvm, ProjectSpaProtocols, WebSocketServerHelper}
import shipreq.webapp.base.user.User

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

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
                        F       : Monad[F],
                        redis   : Redis.ProjectAlgebra[F],
                        db      : DB.ForProjectSpa[D],
                        runDB   : D ~> F,
                        security: Security.Algebra[F]): ProjectSpaLogic[F] = {

    val webSocketHelper = WebSocketServerHelper(ProjectSpaProtocols.WebSocket(Obfuscated(null)))

    val OnConnect  = Monads.FDisj[F, ConnectRejection]
    val OnMsgError = Monads.FDisj[F, MsgError]

    val fUnit = F.pure(())

    new ProjectSpaLogic[F] { self =>

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
            p       <- C.optionF(runDB(db.getProjectHeader(pid)), ProjectNotFound)
            _       <- C.ensure(user.id ==* p.userId, AccessDenied)
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
             res          <- M.rightF(req.reqRes.fold(msgFold)((req.req, static)))
          } yield {
            val protocolAndRes = req.reqRes.protocolRes.andValue(res)
            val fullRes        = \/-((reqId, protocolAndRes))
            val resBin         = BinaryJvm.encode(webSocketHelper.protocolSC)(fullRes)
            resBin
          }

        main.value
      }

      private def parseMsg(msg: BinaryData) = {
        BinaryJvm.attemptDecode(msg, webSocketHelper.protocolCS) match {
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
        onProjectNameSet        = onProjectNameSet.tupled,
        onFieldMandatorinessMod = _ => ???,
        onReqTypeImplicationMod = _ => ???,
        onCreateContent         = _ => ???,
        onUpdateContent         = _ => ???,
        onUpdateSavedViews      = _ => ???,
      )

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
      // InitApp

      private def onInitApp: MsgFn[Unit, ErrorMsg \/ InitAppData] = (_, static) => {
        val pid = static.projectId

        type Result = ErrorMsg \/ InitAppData

        def ignoreCache(c: Redis.ProjectCache): F[Result] = {

          def readDb(startingPoint: (Project, Option[EventOrd.Latest])) =
            runDB(
              db.inDbTransaction(for {
                ia <- db.projectSpaInitApp(pid)
                es <- db.getProjectEvents(pid, DB.EventFilter.given(startingPoint._2))
              } yield (es, ia))
            ).map { case (es, ia) =>
              // Build outside of DB transaction
              ApplyEvents.append(pid, startingPoint, es).map(r => InitAppData(r._1, r._2, ia.lastUpdatedOrCreatedAt))
            }

          def writeRedis(i: InitAppData): F[Boolean] =
            // TODO Maybe write events instead of snapshot
            i.latestEventOrd match {
              case Some(l) =>
                redis.writeSnapshot(pid, Redis.ProjectSnapshot(i.project, l), VerifiedEvent.Seq.empty)
              case None =>
                // Don't try to write an empty project to the cache
                F pure true
            }

            for {
              result <- readDb(c.nonEmptyCompleteBuild(pid) getOrElse ApplyEvents.emptyStartingPoint)
              _      <- result.fold[F[_]](_ => fUnit, writeRedis)
            } yield result
        }

        def useCache(c: Redis.ProjectCache, md: DB.ProjectSpaInitApp): F[Result] = {
          val result = c.build(pid).map(r => InitAppData(r._1, r._2, md.lastUpdatedOrCreatedAt))
          F pure result
        }

        for {
          cache <- redis.read(pid)
          ia    <- runDB(db.projectSpaInitApp(pid))
          r     <- if (cache.isCompleteTo(ia.latestOrd)) useCache(cache, ia) else ignoreCache(cache)
        } yield r
      }

      // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

      private def onProjectNameSet: MsgFn[String, EventResult] = (newName, static) => {
        ???
      }

    } // new ProjectSpaLogic
  }
}
