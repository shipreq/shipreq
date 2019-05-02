package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import japgolly.scalajs.react.extra.{Broadcaster, Px}
import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.base.protocol.WebSocketShared.ReadyState
import shipreq.webapp.base.protocol._

final class Global(wsClientBuilder: WebSocketClient.WithoutCallbacks[WsReqRes, Push],
                   onFirstLoad    : (Global, InitAppData) => Callback,
                   onInitFailure  : ErrorMsg => Callback) extends Broadcaster[Changes] {

  // TODO keep alive
  // TODO reload
  // TODO sync

  import Global.State

  private var _state: State =
    State.Loading(VerifiedEvent.Seq.empty)

  private def unsafeState = _state

  private def unsafeSetState(s: State): Unit = {
    _state = s
    _pxProject.refresh()
  }

  val cbProjectMetaData: CallbackTo[ProjectMetaData] =
    CallbackTo(unsafeState match {
      case State.Active(s)  => s.projectMetaData
      case _: State.Loading => null // TODO Safe because I know I don't access this before initial load
    })

  private val _pxProject: Px.ThunkM[Project] = {
    def f() = unsafeState match {
      case State.Active(s)  => s.project
      case _: State.Loading => Project.empty
    }
    Px(f()).withReuse.manualRefresh
  }

  val pxProject: Px[Project] =
    _pxProject

  def unsafeProject(): Project =
    pxProject.value()

  val wsClient: WebSocketClient[WsReqRes] = {
    LoggerJs.runNow(_.debug("Creating WebSocket..."))
    wsClientBuilder.build(onPush, _ => onWebSocketReadyStateChange)
  }

  private def onWebSocketReadyStateChange(rs: ReadyState): Callback = {
    val result: Callback = rs match {
      case ReadyState.Open =>
        unsafeState match {
          case _: State.Loading => load
          case _: State.Active  => LoggerJs(_.debug("ReadyState became Open while State.Active"))
        }

      case ReadyState.Closed =>
        Callback.empty

      // TODO Handle initial connection failure
      case ReadyState.Connecting
         | ReadyState.Closing => Callback.empty
    }

    LoggerJs(_.info(s"WebSocket ReadyState: $rs")) >> result
  }

  private def load: Callback =
    for {
      _  <- LoggerJs(l => l.info("WebSocket opened. Requesting InitApp...") >> l.time("initApp"))
      a1 <- wsClient.send(WsReqRes.InitApp)(())
      a2  = a1 <* LoggerJs.async(_.timeEnd("initApp"))
      _  <- a2.completeWith {

              case Success(\/-(i)) => Callback {
                unsafeState match {
                  case State.Loading(es) =>
                    val s = ProjectState.init(i.project, i.projectMetaData).addEventsSimple(es)
                    unsafeSetState(State.Active(s))
                    onFirstLoad(this, i).runNow()
                  case _: State.Active =>
                    LoggerJs.runNow(_.warn("InitApp response received but already State.Active"))
                }
              }

              case Success(-\/(errMsg)) =>
                onInitFailure(errMsg) >> wsClient.close

              case Failure(err) =>
                for {
                  _ <- LoggerJs(_.warn(s"Connection failure: ${err.getMessage}"))
                } yield ()
            }
    } yield ()

  private def onPush(recvEvents: VerifiedEvent.NonEmptySeq): Callback = Callback {
    LoggerJs.runNow(_.debug("Server pushed: " + recvEvents))

    unsafeState match {

      case State.Active(ps1) =>
        for ((ps2, appliedEvents) <- ps1.addEvents(recvEvents.values)) {
          unsafeSetState(State.Active(ps2))
          val changes = Changes(appliedEvents, ps1.project, ps2.project)
          broadcast(changes).runNow()
        }

      case State.Loading(es) =>
        unsafeSetState(State.Loading(es ++ recvEvents.values))
    }
  }

}

object Global {

  sealed trait State
  object State {
    final case class Loading(events: VerifiedEvent.Seq) extends State
    final case class Active(projectState: ProjectState) extends State
  }

  implicit def reusability: Reusability[Global] = Reusability.always
}
