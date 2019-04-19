package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import japgolly.univeq._
import scala.util.{Failure, Success}
import scalaz.{-\/, Monad, \/, \/-, ~>}
import scalaz.syntax.monad._
import shipreq.base.util.{BinaryData, ErrorMsg, Monads}
import shipreq.webapp.base.data.{Obfuscated, Project, ProjectId}
import shipreq.webapp.base.event.{ApplyEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol2.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol2.{BinaryJvm, ProjectSpaProtocols, WebSocketServerHelper}
import shipreq.webapp.base.user.User

trait ProjectSpaLogic[F[_]] {
  import ProjectSpaLogic._

  def onConnect(cookies: Cookie.LookupFn,
                projectId: ProjectId.Public): F[ConnectRejection \/ (WebSocketStatic, WebSocketState)]

//  final type FRes[R <: ReqRes] = F[Res[R]]
//  val onRequest: ReqRes.Fold[Req, FRes]

  def onMessage(static: WebSocketStatic)
               (state : WebSocketState,
                msg   : BinaryData): F[MsgError \/ (BinaryData, Option[WebSocketState])]
}

object ProjectSpaLogic extends StrictLogging {

  final case class WebSocketStatic(user: User, projectId: ProjectId)

  final case class WebSocketState()
  object WebSocketState {
    val empty = apply()
  }

  sealed trait ConnectRejection
  object ConnectRejection {
    case object NoSession        extends ConnectRejection
    case object AnonymousSession extends ConnectRejection
    case object InvalidProjectId extends ConnectRejection
    case object ProjectNotFound  extends ConnectRejection
    case object AccessDenied     extends ConnectRejection
  }

  sealed trait MsgError
  object MsgError {
    case object DecodingFailure extends MsgError
  }

//  final case class Req[R <: ReqRes](req: R#RequestType, state: WebSocketState, user: User)
//  final case class Res[R <: ReqRes](res: R#ResponseType, stateUpdate: Option[WebSocketState])

  def apply[D[_], F[_]](implicit
                        D: Monad[D],
                        F: Monad[F],
                        db: DB.ForProjectSpa[D],
                        runDB: D ~> F,
                        security: Security.Algebra[F]): ProjectSpaLogic[F] = {

    val webSocketHelper = WebSocketServerHelper(ProjectSpaProtocols.WebSocket(Obfuscated(null)))

    val OnConnect  = Monads.FDisj[F, ConnectRejection]
    val OnMsgError = Monads.FDisj[F, MsgError]

    new ProjectSpaLogic[F] { self =>

      override def onConnect(cookies: Cookie.LookupFn,
                             projectId: ProjectId.Public) = {
        val C = OnConnect
        import ConnectRejection._

        val main: C.Result[(WebSocketStatic, WebSocketState)] =
          for {
            pid     <- C.lift(Obfuscators.projectId.deobfuscate(projectId).leftMap(_ => InvalidProjectId))
            session <- C.optionF(security.sessionRestore(cookies), NoSession)
            user    <- C.option(session.authenticatedUser, AnonymousSession)
            p       <- C.optionF(runDB(db.getProjectHeader(pid)), ProjectNotFound)
            _       <- C.ensure(user.id ==* p.userId, AccessDenied)
          } yield {
            val static = WebSocketStatic(user, pid)
            val state  = WebSocketState.empty
            (static, state)
          }

        security.protect(main.value)
      }

      override def onMessage(static: WebSocketStatic)
                            (state : WebSocketState,
                             msg   : BinaryData) = {
        val M = OnMsgError

        val main: M.Result[(BinaryData, Option[WebSocketState])] =
          for {
            (reqId, req)  <- M.lift(parseMsg(msg))
            (res, state2) <- M.rightF(req.reqRes.fold(msgFold)((req.req, static)))
          } yield {
            val protocolAndRes = req.reqRes.protocolRes.andValue(res)
            val fullRes        = \/-((reqId, protocolAndRes))
            val resBin         = BinaryJvm.encode(webSocketHelper.protocolSC)(fullRes)
            (resBin, state2)
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
      private type MsgFnOut[O] = F[(O, Option[WebSocketState])]
      private type MsgFn[I, O] = (I, WebSocketStatic) => MsgFnOut[O]
      private type MsgFoldIn[R <: WsReqRes] = MsgFnIn[R#RequestType]
      private type MsgFoldOut[R <: WsReqRes] = MsgFnOut[R#ResponseType]

      private val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
        onInitApp               = onInitApp.tupled,
        onProjectNameSet        = _ => ???,
        onFieldMandatorinessMod = _ => ???,
        onReqTypeImplicationMod = _ => ???,
        onCreateContent         = _ => ???,
        onUpdateContent         = _ => ???,
        onUpdateSavedViews      = _ => ???,
      )

      // final case class LoadedState(project: Project, projectMetaData: ProjectMetaData, nextOrd: EventOrd) {
      // final case class InitAppData(project: Project, projectMetaData: ProjectMetaData, latestEventOrd: EventOrd)
      private def onInitApp: MsgFn[Unit, ErrorMsg \/ InitAppData] = (_, static) => {
        val pid = static.projectId

        def buildProject(load: VerifiedEvent.Seq): ErrorMsg \/ (Project, EventOrd) =
          if (load.isEmpty) {
            val ord = EventOrd(1) // Nice to reserve 0 for ApplyTemplate.
            \/-((Project.empty, ord))
          } else
            ApplyEvent.trusted.applyVerified(load)(Project.empty) match {
              case \/-(p) => \/-((p, load.lastKey.ord))
              case -\/(e) =>
                logger.error(s"Failed to apply events [${load.head.ord},${load.last.ord}] on project #${pid.value}: $e")
                -\/(ErrorMsg(s"${Server.ErrorMsgs.ShouldNeverHappen.value}\n\nEvent application failure.\n$e"))
            }

        def load: F[ErrorMsg \/ InitAppData] =
          runDB(
            db.inDbTransaction(for {
              pl <- db.getAllProjectEvents(pid)
              md <- db.getProjectMetaData(pid) // only really need createdAt and lastUpdatedAt
            } yield (pl, md))
          ).map { case (pl, md) =>
            // Build outside of DB transaction
            buildProject(pl).map(b => InitAppData(b._1, md.get, b._2))
          }

        for {
          result <- load
        } yield (result, None)
      }

    }
  }
}
