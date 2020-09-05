package shipreq.webapp.client.project.test

import japgolly.scalajs.react.test.SimEvent
import japgolly.scalajs.react.{Callback, CallbackTo}
import java.time.{Duration, Instant}
import org.scalajs.dom.document
import shipreq.base.util.JsExt._
import shipreq.base.util.{Allow, ErrorMsg, JsTimers, PotentialChange, Retries}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket.ProjectSpaProtocols.WsReqRes
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.protocol.websocket.WebSocketShared.CloseCode
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.user.Username
import shipreq.webapp.client.project.app.state.{Global, ProjectState}
import shipreq.webapp.server.logic.{ApplyNewEvent, MakeEvent}

final class TestGlobal(initialProjectState: ProjectState) extends Global((_, _) => Callback.empty, _ => Callback.empty, LoggerJs.off) {

  override def toString = unsafeState match {
    case Global.State.Active(a, b) => s"TestGlobal(Active($a, $b))"
    case Global.State.Loading(es)  => s"TestGlobal(Loading(${es.map(_.ord.value).mkString(",")}))"
  }

  val reauth = TestReauthenticationModal(Some(\/-(Allow)))

  val optionalFullscreen = TestOptionalFullscreen()

  val username = Username("nimander")

  override val reauthModal = reauth.modal(username)

  override protected def unsafeNow() = now
  var now = Instant.now()
  def advanceTime(d: Duration): Unit = now = now.plusNanos(d.getNano).plusSeconds(d.getSeconds)
  def advanceTimeByMs(ms: Long) = advanceTime(Duration.ofMillis(ms))

  lazy val protocol = ProjectSpaProtocols.WebSocket(initialProjectState.projectMetaData.id)

  lazy val svr = WebSocketServerHelper(protocol)

  type Response = Protocol.AndValue[SafePickler]

  case class Req(msg: FakeWebSocket.Message) {
    lazy val (reqId, req) = svr.protocolCS.codec.decode(msg.binaryData).getOrThrow()

    private var _responded = false
    def responded = _responded

    def assertType(r: WsReqRes): r.RequestType = {
      assertEq(req.reqRes, r)
      val x = req.asInstanceOf[r.AndReq]
      x.req
    }

    def respondWith(r: Response): Unit = {
      val res = \/-((reqId, r))
      val bd = svr.protocolSC.codec.encode(res)
      val msg = FakeWebSocket.Message.ArrayBuffer(bd.toArrayBuffer)
      respond(msg)
    }

    def fail(): Unit = {
      type MsgFoldIn [R <: WsReqRes] = Unit
      type MsgFoldOut[R <: WsReqRes] = R#ResponseType
      def failLeft = (_: Any) => -\/(ErrorMsg("TestGlobal.failLast()"))
      val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
        onInitApp               = _ => ???,
        onReconnect             = _ => ???,
        onSync                  = _ => ???,
        onUpdateConfig          = failLeft,
        onCreateContent         = failLeft,
        onUpdateContent         = failLeft,
        onProjectNameSet        = failLeft,
        onUpdateSavedViews      = failLeft,
        onUpdateManualIssues    = failLeft,
        onFieldMandatorinessMod = _ => (),
        onReqTypeImplicationMod = failLeft,
      )
      def reqReq = req.req
      val res = msgFold(req.reqRes)(reqReq)
      val protocolAndRes = req.reqRes.protocolRes.andValue(res)
      respondWith(protocolAndRes)
    }

    private def respond(msg: FakeWebSocket.Message.ArrayBuffer): Unit = {
      if (_responded)
        sys.error("Request has already been responded to.")
      _responded = true
      ws().recv(msg)
    }
  }

  private var _reqs = Vector.empty[Req]

  private var _fakeWS = Vector.empty[FakeWebSocket]

  private var _autoRespond = true
  private val _autoRespondLogic: Req => Option[Response] = autoEventResponse

  override val wsClient: WebSocketClient[WsReqRes] = {
    val newWS = CallbackTo {
      val f = new FakeWebSocket("whatever", ReadyState.Open)
      f.onSend.set { m =>
        val req = Req(m)
        _reqs :+= req
        if (_autoRespond)
          _autoRespondLogic(req).foreach(req.respondWith)
      }
      _fakeWS :+= f
      f
    }
    WebSocketClient.Builder(newWS, protocol, Retries.none)
      .build(
        reauthorise   = reauthModal.run,
        onServerPush  = onPush,
        onStateChange = _ => onWebSocketStateChange,
        timers        = JsTimers.real,
        logger        = logger)
  }

  val nextEventOrd: CallbackTo[EventOrd] =
    CallbackTo {
      val s = unsafeState.asInstanceOf[Global.State.Active].projectState
      // assert(s.futureEvents.isEmpty, s"TestGlobal.nextEventOrd: s.futureEvents = ${s.futureEvents.map(_.ord.value)}")
      s.projectAndOrd.nextOrd
    }

  def ws() = _fakeWS.last
  def reqs(): Vector[TestGlobal#Req] = _reqs

  def assertReqsSent(count: Int)(implicit s: sourcecode.Enclosing, l: sourcecode.Line): Unit =
    assertEq(s"[${s.value}: ${l.value}] Requests sent", reqs().size, count)

  def respondToLast(p: WsReqRes)(o: p.ResponseType): Unit =
    reqs().last.respondWith(p.protocolRes.andValue(o))

  def addEvent(e: VerifiedEvent) =
    addEvents(VerifiedEvent.Seq.one(e))

  def verifyEventsCB(es: Event*): CallbackTo[VerifiedEvent.Seq] = {
    val eventList = es.toList // avoid Scala bug
    pxProject.toCallback.flatMap(p =>
      CallbackTo.liftTraverse((e: Event) => nextEventOrd.map(verifyEvent(p, e, _))).std[List]
        .map(VerifiedEvent.Seq.empty ++ _ (eventList)))
  }

  def applyTestEventsCB(es: Event*): Callback =
    verifyEventsCB(es: _*).flatMap(addEvents).void

  def setAutoRespond(e: Boolean): this.type = {
    _autoRespond = e
    this
  }
  def disableAutoResponse() = setAutoRespond(false)
  def enableAutoResponse() = setAutoRespond(true)
  def clearReqs(): Unit = _reqs = Vector.empty

  def autoRespondToLast(): Unit = {
    val req = _reqs.last
    _autoRespondLogic(req) match {
      case Some(res) => req.respondWith(res)
      case None      => sys.error("Don't know how to auto-respond to " + req.req)
    }
  }

  def failLast(): Unit =
    _reqs.last.fail()

  override def reconnect(ps: ProjectState): Callback =
    Callback.empty

  private def autoEventResponse: TestGlobal#Req => Option[TestGlobal#Response] = {
    type MsgFnIn   [I]              = I
    type MsgFnOut  [O]              = Option[CallbackTo[O]]
    type MsgFoldIn [R <: WsReqRes]  = MsgFnIn[R#RequestType]
    type MsgFoldOut[R <: WsReqRes]  = MsgFnOut[R#ResponseType]
    type MsgFn     [I]              = MsgFnIn[I] => MsgFnOut[WsReqRes.EventResult]

    def updateProject[I](mkEvent: (I, Project) => MakeEvent.Result): MsgFn[I] = input => Some {
      def run(p1: Project): CallbackTo[WsReqRes.EventResult] = {
        val er = mkEvent(input, p1)
        ApplyNewEvent(er, p1) match {

          case PotentialChange.Success(ApplyNewEvent.Updated(_, event)) =>
            nextEventOrd
              .map(o => VerifiedEvent.Seq.empty + VerifiedEvent(o, event, Instant.now()))
              .flatTap(addEvents)
              .map(\/-(_))

          case PotentialChange.Unchanged =>
            CallbackTo.pure(\/-(VerifiedEvent.Seq.empty))

          case PotentialChange.Failure(e) =>
            CallbackTo.pure(-\/(e))
        }
      }
      pxProject.toCallback.flatMap(run)
    }

    def updateProjectI[I](mkEvent: I => MakeEvent.Result): MsgFn[I] =
      updateProject((i, _) => mkEvent(i))

    val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
      onInitApp               = _ => None,
      onReconnect             = _ => None,
      onSync                  = _ => None,
      onUpdateConfig          = updateProject (MakeEvent.updateConfig),
      onCreateContent         = updateProject (MakeEvent.createContent),
      onUpdateContent         = updateProject (MakeEvent.updateContent),
      onProjectNameSet        = updateProjectI(MakeEvent.projectNameSetFn),
      onUpdateSavedViews      = updateProject (MakeEvent.updateSavedViews),
      onUpdateManualIssues    = updateProject (MakeEvent.updateManualIssues),
      onFieldMandatorinessMod = _ => None,
      onReqTypeImplicationMod = updateProjectI(MakeEvent.reqTypeImplicationMod),
    )

    testReq => {
      import testReq.req
      msgFold(req.reqRes)(req.req).map { proc =>
        val res = proc.runNow()
        val protocolAndRes = req.reqRes.protocolRes.andValue(res)
        protocolAndRes
      }
    }
  }

  unsafeSetState(Global.State.Active(initialProjectState, None))
  wsClient.connect.runNow()
}

object TestGlobal {

  def apply(p: Project): TestGlobal = {
    val md  = looseProjectMetaData(p, eventsTotal = 200)
    val pao = ProjectAndOrd(p, Some(EventOrd.Latest(200)))
    val ps  = ProjectState.init(pao, md)
    new TestGlobal(ps)
  }

  import shipreq.webapp.base.test.TestState._

  final class Obs($: DomZipperJs, g: TestGlobal) {
    val activeElement       = document.activeElement
    val reqs                = g.reqs()
    val reauthModal         = TestReauthenticationModal.Obs(g.reauthModal.id)($.parent.parent, g.reauth)
    val fullscreenCount     = $.collect0n(BaseStyles.fullscreen.selector).size
    val isBrowserFullscreen = g.optionalFullscreen.currentlyFullscreen
  }

  class TestDsl[R, O, S](final val * : Dsl[Id, R, O, S, String])(getRef: R => TestGlobal) {
    protected final implicit def autoRef(r: R): TestGlobal =
      getRef(r)

    val clearReqs =
      *.action("Server: Clear requests.")(_.ref.clearReqs())

    val disableAutoResponse =
      *.action("Server: Disable auto-respond.")(_.ref.disableAutoResponse())

    val enableAutoResponse =
      *.action("Server: Enable auto-respond.")(_.ref.enableAutoResponse())

    val autoRespondToLast =
      *.action("Server responds.")(_.ref.autoRespondToLast())

    val failLastRequest =
      *.action("Fail last server request.")(_.ref.failLast())

    def receiveExternalEvent(e: Event): *.Actions =
      *.action("Receive external event: " + e)(_.ref.applyTestEventsCB(e).void.runNow())

    def disconnect(code: CloseCode): *.Actions =
      *.action("Disconnect with " + code)(_.ref.ws().close(code))

    val expireSession: *.Actions =
      disconnect(CloseCode.unauthorised).rename("Expire session")
  }

  final class TestDslWithObs[R, O, S](dsl   : Dsl[Id, R, O, S, String])
                                     (getRef: R => TestGlobal,
                                      getObs: O => Obs) extends TestDsl(dsl)(getRef) {
    protected final implicit def autoObs(o: O): Obs =
      getObs(o)

    val activeElement = *.focus("activeElement").value(_.obs.activeElement)

    val fullscreenCount = *.focus("Fullscreen elements").value(_.obs.fullscreenCount)

    val isBrowserFullscreen = *.focus("Browser is fullscreen").value(_.obs.isBrowserFullscreen)

    val requestCount = *.focus("Server requests").value(_.obs.reqs.length)

    val lastTwoRequests = *.focus("Last two requests").compare(_.obs.reqs.last, _.obs.reqs.init.last)

    val assertLastTwoRequestsAreEqual = lastTwoRequests.map(_.req).assert.equal(Equal.by_==, implicitly)

    def press(k: SimEvent.Keyboard): *.Actions =
      *.action(s"Press ${k.desc}.")(k simulateKeyDownPressUp _.obs.activeElement)

    def assertCellHasFocus(f: CommonObs.Editor.TestDsl[R, O, S]) =
      activeElement.assert.equalBy(f.cellDom.run)

    def assertEditorHasFocus(f: CommonObs.Editor.TestDsl[R, O, S]) =
      activeElement.assert.equalBy(f.editorDom.run(_).orNull)
  }
}
