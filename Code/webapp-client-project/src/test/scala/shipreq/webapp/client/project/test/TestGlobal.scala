package shipreq.webapp.client.project.test

import boopickle.Pickler
import japgolly.scalajs.react.{Callback, CallbackTo}
import java.time.{Duration, Instant}
import scalaz.{-\/, \/-}
import shipreq.base.util.JsExt._
import shipreq.base.util.{ErrorMsg, PotentialChange, Retries}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes
import shipreq.webapp.base.protocol.WebSocket.ReadyState
import shipreq.webapp.client.project.app.state.{Global, ProjectState}
import shipreq.webapp.server.logic.{ApplyNewEvent, MakeEvent}

final class TestGlobal(initialProjectState: ProjectState) extends Global((_, _) => Callback.empty, _ => Callback.empty) {

  override def toString = unsafeState match {
    case Global.State.Active(a, b) => s"TestGlobal(Active($a, $b))"
    case Global.State.Loading(es)  => s"TestGlobal(Loading(${es.map(_.ord.value).mkString(",")}))"
  }

  override protected val logger = LoggerJs.off

  override protected def unsafeNow() = now
  var now = Instant.now()
  def advanceTime(d: Duration): Unit = now = now.plusNanos(d.getNano).plusSeconds(d.getSeconds)
  def advanceTimeByMs(ms: Long) = advanceTime(Duration.ofMillis(ms))

  lazy val protocol = ProjectSpaProtocols.WebSocket(initialProjectState.projectMetaData.id)

  lazy val svr = WebSocketServerHelper(protocol)

  type Response = Protocol.AndValue[Pickler]

  case class Req(msg: FakeWebSocket.Message) {
    lazy val (reqId, req) = BinaryJs.decodeUnsafe(msg.binaryData, svr.protocolCS)

    private var _responded = false
    def responded = _responded

    def assertType(r: WsReqRes): r.RequestType = {
      assertEq(req.reqRes, r)
      val x = req.asInstanceOf[r.AndReq]
      x.req
    }

    def respondWith(r: Response): Unit = {
      val res = \/-((reqId, r))
      val bd = BinaryJs.encode(svr.protocolSC)(res)
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
        onCreateContent         = failLeft,
        onUpdateContent         = failLeft,
        onProjectNameSet        = failLeft,
        onUpdateSavedViews      = failLeft,
        onFieldMandatorinessMod = failLeft,
        onReqTypeImplicationMod = failLeft,
        onCustomIssueTypeCrud   = failLeft,
        onCustomReqTypeCrud     = failLeft,
        onFieldMod              = failLeft,
        onTagMod                = failLeft,
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
  private var _autoRespondLogic: Req => Option[Response] = autoEventResponse

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
    WebSocketClient(newWS, protocol, Retries.none)
      .build(onPush, _ => onWebSocketReadyStateChange, logger)
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
    verifyEventsCB(es: _*).flatMap(addEvents)

  def setAutoRespond(e: Boolean): this.type = {
    _autoRespond = e
    this
  }
  def disableAutoResponse() = setAutoRespond(false)
  def enableAutoResponse() = setAutoRespond(true)

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

          case PotentialChange.Success(ApplyNewEvent.Updated(_, event, hrs)) =>
            nextEventOrd
              .map(o => VerifiedEvent.Seq.empty + VerifiedEvent(o, event, hrs))
              .flatTap(addEvents)
              .map(\/-(_))

          case PotentialChange.Unchanged =>
            CallbackTo.pure(\/-(VerifiedEvent.Seq.empty))

          case PotentialChange.Failure(e) =>
            CallbackTo.pure(-\/(ErrorMsg(e)))
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
    val md  = looseProjectMetaData(p, totalEventCount = 200)
    val pao = ProjectAndOrd(p, Some(EventOrd.Latest(200)))
    val ps  = ProjectState.init(pao, md)
    new TestGlobal(ps)
  }

}
