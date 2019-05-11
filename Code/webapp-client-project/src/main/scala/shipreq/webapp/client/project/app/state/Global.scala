package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px}
import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.project.app.state.Global.State

abstract class Global(onFirstLoad: (Global, InitAppData) => Callback,
                      onInitFailure: ErrorMsg => Callback) extends Broadcaster[Changes] {

  protected val logger = LoggerJs.on

  private var _state: State =
    State.Loading(VerifiedEvent.Seq.empty)

  final protected def unsafeState = _state

  final protected def unsafeSetState(s: State): Unit = {
    _state = s
    _pxProject.refresh()
  }

  final val cbProjectMetaData: CallbackTo[ProjectMetaData] =
    CallbackTo(unsafeState match {
      case State.Active(s)  => s.projectMetaData
      case _: State.Loading => null // Safe because I know I don't access this before initial load
    })

  final private val _pxProject: Px.ThunkM[Project] = {
    def f() = unsafeState match {
      case State.Active(s)  => s.project
      case _: State.Loading => Project.empty
    }
    Px(f()).withReuse.manualRefresh
  }

  final val pxProject: Px[Project] =
    _pxProject

  final def unsafeProject(): Project =
    pxProject.value()

  val wsClient: WebSocketClient[WsReqRes]

  final protected def onWebSocketReadyStateChange(rs: ReadyState): Callback = Callback.lazily {
    val result: Callback = rs match {
      case ReadyState.Open =>
        unsafeState match {
          case _: State.Loading => load
          case _: State.Active  => logger(_.debug("ReadyState became Open while State.Active"))
        }

      case ReadyState.Closed =>
        unsafeState match {
          case _: State.Loading => onInitFailure(ErrorMsg("Connection to server failed."))
          case _: State.Active  => Callback.empty
        }

      case ReadyState.Connecting
         | ReadyState.Closing => Callback.empty
    }

    logger(_.info(s"WebSocket ReadyState: $rs")) >> result
  }

  final private def load: Callback =
    for {
      _  <- logger(l => l.info("WebSocket opened. Requesting InitApp...") >> l.time("initApp"))
      a1 <- wsClient.send(WsReqRes.InitApp)(())
      a2  = a1 <* logger.async(_.timeEnd("initApp"))
      _  <- a2.completeWith {

        case Success(\/-(i)) => Callback {
          unsafeState match {
            case State.Loading(es) =>
              val s = ProjectState.init(i.project, i.projectMetaData).addEventsSimple(es)
              unsafeSetState(State.Active(s))
              onFirstLoad(this, i).runNow()
            case _: State.Active =>
              logger.runNow(_.warn("InitApp response received but already State.Active"))
          }
        }

        case Success(-\/(errMsg)) =>
          onInitFailure(errMsg) >> wsClient.close

        case Failure(err) =>
          for {
            _ <- logger(_.warn(s"Connection failure: ${err.getMessage}"))
          } yield ()
      }
    } yield ()

  final def addEvents(recvEvents: VerifiedEvent.Seq): Callback =
    Callback.when(recvEvents.nonEmpty)(Callback {
      logger.runNow(_.debug("Adding events: " + recvEvents))

      unsafeState match {

        case State.Active(ps1) =>
          for ((ps2, appliedEvents) <- ps1.addEvents(recvEvents)) {
            unsafeSetState(State.Active(ps2))
            for (ves <- VerifiedEvent.NonEmptySeq.maybe(appliedEvents)) {
              val changes = Changes(ves, ps1.project, ps2.project)
              broadcast(changes).runNow()
            }
          }

        case State.Loading(es) =>
          unsafeSetState(State.Loading(es ++ recvEvents))
      }
    })

  final protected def onPush(recvEvents: VerifiedEvent.NonEmptySeq): Callback = Callback {
    logger.runNow(_.info("Server pushed: " + recvEvents))
    addEvents(recvEvents.values).runNow()
  }

  private final def sspToEvents(p: WsReqRes {type ResponseType = WsReqRes.EventResult}): ServerSideProcInvoker[p.RequestType, ErrorMsg, VerifiedEvent.Seq] =
    wsClient.invoker(p)
      .mergeFailure
      .onSuccess((ves, s) => addEvents(ves) >> s)

  final lazy val sspCreateContent         = sspToEvents(WsReqRes.CreateContent)
  final lazy val sspUpdateContent         = sspToEvents(WsReqRes.UpdateContent)
  final lazy val sspProjectNameSet        = sspToEvents(WsReqRes.ProjectNameSet)
  final lazy val sspUpdateSavedViews      = sspToEvents(WsReqRes.UpdateSavedViews)
  final lazy val sspFieldMandatorinessMod = sspToEvents(WsReqRes.FieldMandatorinessMod)
  final lazy val sspReqTypeImplicationMod = sspToEvents(WsReqRes.ReqTypeImplicationMod)
  final lazy val sspCustomIssueTypeCrud   = sspToEvents(WsReqRes.CustomIssueTypeCrud)
  final lazy val sspCustomReqTypeCrud     = sspToEvents(WsReqRes.CustomReqTypeCrud)
  final lazy val sspFieldMod              = sspToEvents(WsReqRes.FieldMod)
  final lazy val sspTagMod                = sspToEvents(WsReqRes.TagMod)
}

object Global {

  def apply(wscBuilder   : WebSocketClient.WithoutCallbacks[WsReqRes, Push],
            onFirstLoad  : (Global, InitAppData) => Callback,
            onInitFailure: ErrorMsg => Callback,
            logger       : LoggerJs.Dsl): Global =
    new Global(onFirstLoad, onInitFailure) {
      override val wsClient: WebSocketClient[WsReqRes] = {
        logger.runNow(_.debug("Creating WebSocket..."))
        wscBuilder.build(onPush, _ => onWebSocketReadyStateChange, logger)
      }
    }

  sealed trait State
  object State {
    final case class Loading(events: VerifiedEvent.Seq) extends State
    final case class Active(projectState: ProjectState) extends State
  }

  implicit def reusability: Reusability[Global] = Reusability.always
}
