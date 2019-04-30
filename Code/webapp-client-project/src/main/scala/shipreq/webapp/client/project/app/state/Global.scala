package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import japgolly.scalajs.react.extra.{Broadcaster, Px}
import scala.util.Success
import scalaz.\/-
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.lib.DataReusability._
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

  val wsClient: WebSocketClient[WsReqRes] =
    wsClientBuilder.build(onPush, _ => onWebSocketReadyStateChange)

  private def onWebSocketReadyStateChange(rs: ReadyState): Callback =
    rs match {
      case ReadyState.Open =>
        unsafeState match {
          case _: State.Loading => load
          case _: State.Active  => Callback.empty
        }

      case ReadyState.Connecting
         | ReadyState.Closing
         | ReadyState.Closed => Callback.empty

      // TODO Handle initial connection failure
    }

  private def load: Callback =
    for {
      a <- wsClient.send(WsReqRes.InitApp)(())
      _ <- a.completeWith {
        case Success(\/-(i)) => Callback {
          unsafeState match {
            case State.Loading(es) =>
              val s = ProjectState.init(i.project, i.projectMetaData).addEventsSimple(es)
              unsafeSetState(State.Active(s))
              onFirstLoad(this, i).runNow()

            case State.Active(_) =>
              // TODO ?
          }
        }
        case x => Callback {
          println("FAILURE: " + x) // TODO Think about how to handle this....
        }
      }
    } yield ()

  private def onPush(recvEvents: VerifiedEvent.NonEmptySeq): Callback = Callback {
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
