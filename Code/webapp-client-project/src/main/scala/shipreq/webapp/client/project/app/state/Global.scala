package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.{Broadcaster, Px}
import java.time.{Duration, Instant}
import org.scalajs.dom.window
import scala.util.{Failure, Success}
import shipreq.base.util.{ErrorMsg, JsTimers}
import shipreq.webapp.base.data.{ProjectCreator, ProjectId, Rolodex, UserId, Username}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Metadata
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.app.pages.root.ConnectionStatus
import shipreq.webapp.client.project.app.state.Global.State
import shipreq.webapp.client.ww.api._
import shipreq.webapp.member.project.data.{Project, ProjectMetaData}
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.library.{NewEvents, ProjectLibrary}
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.{InitAppData, StateUpdate, Supplimentary, WsReqRes}
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.ReauthenticationModal

abstract class Global(userId          : UserId.Public,
                      creator         : ProjectCreator,
                      onFirstLoad     : (Global, InitAppData) => Callback,
                      onInitFailure   : ErrorMsg => Callback,
                      initialState    : State,
                      ww              : WebWorkerClient.Instance,
                      final val logger: LoggerJs) extends Broadcaster[ProjectLibrary.Update] {

  val localStorage: AbstractWebStorage

  val reauthModal: ReauthenticationModal

  protected def unsafeNow() = Instant.now()

  private var _state: State =
    initialState

  private val state =
    CallbackTo(_state)

  final protected def unsafeState() =
    _state

  final protected def unsafeSetState(s: State): Unit = {
    _state = s
    _pxProject.refresh()
  }

  final protected def unsafeModState(f: State => State): Unit =
    unsafeSetState(f(unsafeState()))

  final val cbProjectMetaData: CallbackTo[ProjectMetaData] =
    CallbackTo(unsafeState() match {
      case s: State.Active  => s.projectLibrary.latestMetaData
      case _: State.Loading => null // Safe because I know I don't access this before initial load
    })

  final val pxProjectMetaData: Px[ProjectMetaData] =
    Px.callback(cbProjectMetaData).withReuse.autoRefresh

  final private val _pxProject: Px.ThunkM[Project] = {
    def f() = unsafeState() match {
      case s: State.Active  => s.projectLibrary.latest
      case _: State.Loading => Project.init(creator)
    }
    Px(f()).withReuse.manualRefresh
  }

  final val pxProject: Px[Project] =
    _pxProject

  final def unsafeProject(): Project =
    pxProject.value()

  def projectMetadata(id: ProjectId.Public): CallbackTo[Metadata.Project] =
    CallbackTo(unsafeState() match {
      case s: State.Active =>
        Metadata.Project(
          id           = id,
          ord          = Some(s.projectLibrary.latest.ordAsInt),
          futureEvents = s.projectLibrary.futureEvents.iterator.map(_.ord.value).toSet)

      case _: State.Loading =>
        Metadata.Project(
          id           = id,
          ord          = None,
          futureEvents = Set.empty)
    })

  val wsClient: WebSocketClient[WsReqRes]

  final protected def onWebSocketStateChange(s: WebSocketClient.State): Callback = Callback.lazily {
    import WebSocketClient.State._

    val updateConnectionStatus: Callback =
      s match {
        case Authorised(ReadyState.Open) =>
          connectedStatusHub(ConnectionStatus.Connected)

        case Authorised(ReadyState.Closed)
           | Authorised(ReadyState.Connecting)
           | Authorised(ReadyState.Closing)
           | Unauthorised =>
          connectedStatusHub(ConnectionStatus.Disconnected)
      }

    val action: Callback =
      unsafeState() match {
        case _: State.Loading =>
          s match {
            case Authorised(ReadyState.Open) =>
              load

            case Authorised(ReadyState.Closed) =>
              onInitFailure(ErrorMsg("Connection to server failed."))

            case Unauthorised =>
              val errMsg = ErrorMsg("Your session has expired. Please login again.")
              onInitFailure(errMsg) >> Callback(window.location.reload())

            case Authorised(ReadyState.Connecting)
               | Authorised(ReadyState.Closing) =>
              Callback.empty
          }

        case g: State.Active =>
          s match {
            case Authorised(ReadyState.Open) =>
              reconnect(g.projectLibrary)

            case Authorised(ReadyState.Closed)
               | Authorised(ReadyState.Connecting)
               | Authorised(ReadyState.Closing)
               | Unauthorised =>
              Callback.empty
          }
      }

    logger.pure(_.info(s"WebSocket State: $s")) >> updateConnectionStatus >> action
  }

  final object connectedStatusHub extends Broadcaster[ConnectionStatus] {
    var _connectionStatus: ConnectionStatus =
      ConnectionStatus.Disconnected

    private[Global] def apply(c: ConnectionStatus): Callback =
      Callback {
        if (_connectionStatus !=* c) {
          _connectionStatus = c
          broadcast(c).runNow()
        }
      }

    def unsafeGet() = _connectionStatus
  }

  final private def load: Callback =
    for {
      _  <- logger.pure(l => l.info("WebSocket opened. Requesting InitApp...") >> l.time("initApp"))
      s1 <- state
      a1 <- wsClient.send(WsReqRes.InitApp)(s1.ord)
      a2  = a1 <* logger.async(_.timeEnd("initApp"))
      _  <- a2.completeWith {

              case Success(\/-(i)) => Callback {
                unsafeState() match {

                  case State.Loading(pl1, supp) =>
                    unsafeOnSuccessfulFirstLoad(i, pl1, supp)

                  case _: State.Active =>
                    logger(_.warn("InitApp response received but already State.Active"))
                }
              }

              case Success(-\/(errMsg)) =>
                onInitFailure(errMsg) >> wsClient.close

              case Failure(err) =>
                logger.pure(_.warn(s"Connection failure: ${err.getMessage}"))
            }
    } yield ()

  final private def unsafeOnSuccessfulFirstLoad(i: InitAppData, pl1: ProjectLibrary, supp: Supplimentary): Unit = {
    // Update state
    val pl2 = pl1.updated(i.projectData, unsafeNow())
    val s = ProjectLibrary.WithMetaData(pl2, i.projectMetaData, userId)
    unsafeSetState(State.Active(s, supp ++ i.supp))

    // Notify first-load listener
    onFirstLoad(this, i).runNow()

    // Update web worker state
    ww.send(WebWorkerCmd.UpdateProject(i.projectData)).runNow()

    // Start handling web worker pushes
    ww.replaceOnPush(onWebWorkerPush).runNow()
  }

  final protected def onWebWorkerPush: WebWorkerPushCmd => Callback = {

    case WebWorkerPushCmd.MissingEvents(ords) =>
      state.flatMap { s =>
        val events = s.projectLibrary.events(ords.whole)
        val cmd    = WebWorkerCmd.UpdateProject(\/-(events))
        ww.send(cmd).toCallback.when_(events.nonEmpty)
      }
  }

  protected def reconnect(pl: ProjectLibrary): Callback =
    for {
      _  <- logger.pure(l => l.info("WebSocket re-established. Requesting Reconnect...") >> l.time("reconnect"))
      a1 <- wsClient.send(WsReqRes.Reconnect)(pl.ord)
      a2  = a1 <* logger.async(_.timeEnd("reconnect"))
      _  <- a2.completeWith {
              case Success(upd) => applyUpdateFromServer(upd).void
              case Failure(err) => logger.pure(_.warn(s"Connection failure: ${err.getMessage}"))
            }
    } yield ()

  final def addEvents(recvEvents: VerifiedEvent.Seq): CallbackTo[NewEvents] =
    CallbackTo[NewEvents] {
      logger(_.debug("Adding events: " + recvEvents))
      unsafeState() match {

        case s1: State.Active =>
          s1.projectLibrary.update(recvEvents, unsafeNow()) match {

            case Some(update) =>

              // Update state
              unsafeSetState(State.Active(update.newLibrary, s1.supp))

              // Broadcast changes
              if (update.newlyAppliedEvents.nonEmpty) {
                onProjectUpdate(update).runNow()
              }

              update.newEvents

            case None =>
              // This usually happens when a SSP returns the events it creates, but the server push happens first
              // meaning that the events have already been applied.
              NewEvents(recvEvents, s1.projectLibrary.latest)
          }

        case State.Loading(pl, supp) =>
          unsafeSetState(State.Loading(pl.addEvents(recvEvents, unsafeNow()), supp))
          NewEvents.empty(creator)
      }
    }

  final private def onProjectUpdate(update: ProjectLibrary.Update): Callback = {
    val updateApp: Callback =
      broadcast(update)

    val updateWebWorker: Callback =
      Callback.traverseOption(VerifiedEvent.NonEmptySeq.maybe(update.newlyAppliedEvents))(ves =>
        ww.send(WebWorkerCmd.UpdateProject(\/-(ves))).toCallback
      )

    updateApp >> updateWebWorker
  }

  final protected def onPush(push: Push): Callback = Callback {
    logger(_.info("Server pushed: " + push))
    applyUpdateFromServer(push).runNow()
  }

  final protected def applyUpdateFromServer(upd: StateUpdate) = CallbackTo[NewEvents] {
    unsafeAddSupplimentary(upd.supp)
    addEvents(upd.events).runNow()
  }

  private def unsafeAddSupplimentary(s: Supplimentary): Unit =
    unsafeModState {
      case State.Active (pl, supp) => State.Active (pl, supp ++ s)
      case State.Loading(pl, supp) => State.Loading(pl, supp ++ s)
    }

  private final def sspToEvents(p: WsReqRes {type ResponseType = WsReqRes.EventResult}): ServerSideProcInvoker[p.RequestType, ErrorMsg, NewEvents] =
    wsClient.invoker(p)
      .mergeFailure
      .mapC(applyUpdateFromServer)

  def requestSyncIfStaleFor(tolerance: Duration): Callback = {
    val missingEvents = CallbackTo[Option[NonEmptySet[EventOrd]]] {
      unsafeState() match {
        case s: State.Active =>
          for {
            missingNE <- s.projectLibrary.missingEventsIfStale(unsafeNow(), tolerance)
          } yield {
            logger(_.info {
              val events = missingNE.iterator.map(_.value).toList.sorted.mkString(",")
              s"Client is stale. Requesting sync for events: $events"
            })
            missingNE
          }
        case _ => None
      }
    }

    for {
      es <- missingEvents.asCBO
      _  <- wsClient.readyState.asCBO.filter(_ == ReadyState.Open)
      _  <- wsClient.send(WsReqRes.Sync)(es).toCBO
    } yield ()
  }

  lazy val setConnectionStatus: ConnectionStatus => Reusable[Callback] = {
    val connect    = Reusable.callbackByRef(wsClient.connect)
    val disconnect = Reusable.callbackByRef(wsClient.close)

    {
      case ConnectionStatus.Connected    => connect
      case ConnectionStatus.Disconnected => disconnect
    }
  }

  final lazy val sspUpdateConfig          = sspToEvents(WsReqRes.UpdateConfig)
  final lazy val sspCreateContent         = sspToEvents(WsReqRes.CreateContent)
  final lazy val sspUpdateContent         = sspToEvents(WsReqRes.UpdateContent)
  final lazy val sspProjectNameSet        = sspToEvents(WsReqRes.ProjectNameSet)
  final lazy val sspUpdateSavedViews      = sspToEvents(WsReqRes.UpdateSavedViews)
  final lazy val sspUpdateManualIssues    = sspToEvents(WsReqRes.UpdateManualIssues)
  final lazy val sspReqTypeImplicationMod = sspToEvents(WsReqRes.ReqTypeImplicationMod)
  final lazy val sspUpdateAccess          = sspToEvents(WsReqRes.UpdateAccess)
}

object Global {

  def apply(userId       : UserId.Public,
            username     : Username,
            creator      : ProjectCreator,
            reauth       : ReauthenticationModal,
            wscBuilder   : WebSocketClient.Builder[WsReqRes, Push],
            onFirstLoad  : (Global, InitAppData) => Callback,
            onInitFailure: ErrorMsg => Callback,
            localStorage : AbstractWebStorage,
            initialData  : ProjectLibrary,
            ww           : WebWorkerClient.Instance,
            logger       : LoggerJs): Global = {

    val _localStorage = localStorage

    val supp = Supplimentary(Rolodex.init(userId, username))

    val initialState = State.Loading(initialData, supp)

    new Global(userId, creator, onFirstLoad, onInitFailure, initialState, ww, logger) {

      override val localStorage = _localStorage

      override val reauthModal = reauth

      override val wsClient: WebSocketClient[WsReqRes] = {
        logger(_.debug("Creating WebSocket..."))
        wscBuilder.build(
          reauthorise   = reauthModal.run,
          onServerPush  = onPush,
          onStateChange = _ => onWebSocketStateChange,
          timers        = JsTimers.real,
          localStorage  = _localStorage,
          logger        = logger,
        )
      }
    }
  }

  sealed trait State {
    val projectLibrary: ProjectLibrary
    val supp: Supplimentary

    @inline final def ord = projectLibrary.ord
  }

  object State {

    /** Waiting to be initialised by the web socket. */
    final case class Loading(projectLibrary: ProjectLibrary, supp: Supplimentary) extends State

    final case class Active(projectLibrary: ProjectLibrary.WithMetaData, supp: Supplimentary) extends State
  }

  implicit def reusability: Reusability[Global] = Reusability.always
}
