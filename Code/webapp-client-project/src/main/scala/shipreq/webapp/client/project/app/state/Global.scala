package shipreq.webapp.client.project.app.state

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.extra.{Broadcaster, Px}
import japgolly.scalajs.react._
import japgolly.univeq._
import java.time.{Duration, Instant}
import org.scalajs.dom.window
import scala.util.{Failure, Success}
import scalaz.{-\/, \/-}
import shipreq.base.util.{ErrorMsg, JsTimers}
import shipreq.webapp.base.data.{Project, ProjectId, ProjectMetaData}
import shipreq.webapp.base.event.{EventOrd, EventSeqSummary, VerifiedEvent}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol.CommonProtocols.{Metadata, SubmitFeedback}
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.base.protocol.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.ui.ReauthenticationModal
import shipreq.webapp.client.project.app.root.ConnectionStatus
import shipreq.webapp.client.project.app.state.Global.State

abstract class Global(onFirstLoad  : (Global, InitAppData) => Callback,
                      onInitFailure: ErrorMsg => Callback) extends Broadcaster[EventSeqSummary.WithProject] {

  val reauthModal: ReauthenticationModal

  protected val logger = LoggerJs.on

  protected def unsafeNow() = Instant.now()

  private var _state: State =
    State.Loading(VerifiedEvent.Seq.empty)

  final protected def unsafeState = _state

  final protected def unsafeSetState(s: State): Unit = {
    _state = s
    _pxProject.refresh()
  }

  final val cbProjectMetaData: CallbackTo[ProjectMetaData] =
    CallbackTo(unsafeState match {
      case s: State.Active  => s.projectState.projectMetaData
      case _: State.Loading => null // Safe because I know I don't access this before initial load
    })

  final val pxProjectMetaData: Px[ProjectMetaData] =
    Px.callback(cbProjectMetaData).withReuse.autoRefresh

  final private val _pxProject: Px.ThunkM[Project] = {
    def f() = unsafeState match {
      case s: State.Active  => s.projectState.project
      case _: State.Loading => Project.empty
    }
    Px(f()).withReuse.manualRefresh
  }

  final val pxProject: Px[Project] =
    _pxProject

  final def unsafeProject(): Project =
    pxProject.value()

  def projectMetadata(id: ProjectId.Public): CallbackTo[Metadata.Project] =
    CallbackTo(unsafeState match {
      case s: State.Active =>
        Metadata.Project(
          id           = id,
          ord          = Some(s.projectState.projectAndOrd.ordAsInt),
          futureEvents = s.projectState.futureEvents.iterator.map(_.ord.value).toSet)

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
      unsafeState match {
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
              reconnect(g.projectState)

            case Authorised(ReadyState.Closed)
               | Authorised(ReadyState.Connecting)
               | Authorised(ReadyState.Closing)
               | Unauthorised =>
              Callback.empty
          }
      }

    logger(_.info(s"WebSocket State: $s")) >> updateConnectionStatus >> action
  }

  final object connectedStatusHub extends Broadcaster[ConnectionStatus] {
    var _connectionStatus: ConnectionStatus = ConnectionStatus.Disconnected
    private[Global] def apply(c: ConnectionStatus) = {
      _connectionStatus = c
      broadcast(c)
    }
    def unsafeGet() = _connectionStatus
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
                    unsafeSetState(State.Active(s, None))
                    onFirstLoad(this, i).runNow()
                  case _: State.Active =>
                    logger.runNow(_.warn("InitApp response received but already State.Active"))
                }
              }

              case Success(-\/(errMsg)) =>
                onInitFailure(errMsg) >> wsClient.close

              case Failure(err) =>
                logger(_.warn(s"Connection failure: ${err.getMessage}"))
            }
    } yield ()

  protected def reconnect(ps: ProjectState): Callback =
    for {
      _  <- logger(l => l.info("WebSocket re-established. Requesting Reconnect...") >> l.time("reconnect"))
      a1 <- wsClient.send(WsReqRes.Reconnect)(ps.ord)
      a2  = a1 <* logger.async(_.timeEnd("reconnect"))
      _  <- a2.completeWith {
              case Success(ves) => addEvents(ves)
              case Failure(err) => logger(_.warn(s"Connection failure: ${err.getMessage}"))
            }
    } yield ()

  final def addEvents(recvEvents: VerifiedEvent.Seq): Callback =
    Callback.when(recvEvents.nonEmpty)(Callback {
      logger.runNow(_.debug("Adding events: " + recvEvents))
      unsafeState match {

        case s1: State.Active =>
          for (update <- s1.projectState.addEvents(recvEvents)) {

            // Update state
            val similarlyStale = update.newState.ord ==* s1.projectState.ord
            val staleSince =
              if (similarlyStale)
                s1.staleSince.orElse(Some(unsafeNow()))
              else if (update.newState.futureEvents.nonEmpty)
                Some(unsafeNow())
              else
                None
            unsafeSetState(State.Active(update.newState, staleSince))

            // Broadcast changes
            for (ves <- update.newlyAppliedEventsNE) {
              val ess = EventSeqSummary(ves.iterator.map(_.event)).withProject(update.newState.project)
              broadcast(ess).runNow()
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

  def requestSyncIfStaleFor(tolerance: Duration): Callback = {
    val missingEvents = CallbackTo[Option[NonEmptySet[EventOrd]]] {
      unsafeState match {
        case s: State.Active =>
          for {
            staleSince  ← s.staleSince
            stalePeriod = Duration.between(staleSince, unsafeNow())
            _           ← Option.when(stalePeriod.isLongerThan(tolerance))(())
            lastEvent   ← s.projectState.futureEvents.lastOption
            first       = s.projectState.projectAndOrd.nextOrd.value
            last        = lastEvent.ord.value - 1
            got         = s.projectState.futureEvents.iterator.map(_.ord.value).toSet
            missing     = first.to(last).iterator.filterNot(got.contains).map(EventOrd(_)).toSet
            missingNE   ← NonEmptySet.option(missing)
          } yield {
            logger.runNow(_.info {
              val dur = stalePeriod.conciseDesc
              val events = missingNE.iterator.map(_.value).toList.sorted.mkString(",")
              s"Client has been stale for $dur. Requesting sync for events: $events"
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
  final lazy val sspFieldMandatorinessMod = sspToEvents(WsReqRes.FieldMandatorinessMod)
  final lazy val sspReqTypeImplicationMod = sspToEvents(WsReqRes.ReqTypeImplicationMod)
}

object Global {

  def apply(reauth       : ReauthenticationModal,
            wscBuilder   : WebSocketClient.Builder[WsReqRes, Push],
            onFirstLoad  : (Global, InitAppData) => Callback,
            onInitFailure: ErrorMsg => Callback,
            logger       : LoggerJs.Dsl): Global =
    new Global(onFirstLoad, onInitFailure) {

      override val reauthModal = reauth

      override val wsClient: WebSocketClient[WsReqRes] = {
        logger.runNow(_.debug("Creating WebSocket..."))
        wscBuilder.build(
          reauthorise   = reauthModal.run,
          onServerPush  = onPush,
          onStateChange = _ => onWebSocketStateChange,
          timers        = JsTimers.real,
          logger        = logger)
      }
    }

  sealed trait State
  object State {
    final case class Loading(events: VerifiedEvent.Seq) extends State
    final case class Active(projectState: ProjectState, staleSince: Option[Instant]) extends State
  }

  implicit def reusability: Reusability[Global] = Reusability.always
}
