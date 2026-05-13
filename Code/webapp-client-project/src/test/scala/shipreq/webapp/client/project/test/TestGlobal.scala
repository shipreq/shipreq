package shipreq.webapp.client.project.test

import japgolly.scalajs.react.ReactCats._
import japgolly.scalajs.react.test.SimEvent
import japgolly.scalajs.react.{Callback, CallbackTo}
import java.time.{Duration, Instant}
import org.scalajs.dom.{EventTarget, document, html}
import scala.scalajs.js
import shipreq.base.util.JsExt._
import shipreq.base.util.{Allow, Deny, ErrorMsg, JsTimers, PotentialChange, Retries}
import shipreq.webapp.base.data.{EmailAddr, ProjectCreator, ProjectRole, Rolodex, UserId, Username}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket.WebSocket.ReadyState
import shipreq.webapp.base.protocol.websocket.WebSocketShared.CloseCode
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.base.test._
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.app.state.Global
import shipreq.webapp.member.project.data.{Project, ProjectAccess}
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.library.{CacheJs, ProjectLibrary}
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.{StateUpdate, Supplimentary, WsReqRes}
import shipreq.webapp.member.project.protocol.websocket.{ProjectSpaProtocols, SupplimentaryLogic, UpdateAccessCmd}
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test._
import shipreq.webapp.member.ui.BaseStyles
import shipreq.webapp.server.logic.event._
import shipreq.webapp.server.logic.util.Obfuscators

final class TestGlobal(initialProjectLibrary: ProjectLibrary.WithMetaData,
                       val userId           : UserId.Public,
                       val username         : Username,
                       creator              : ProjectCreator,
                       initialSupp          : Supplimentary,
                       ww                   : WebWorkerClient.Instance,
  ) extends Global(
    userId,
    creator,
    (_, _) => Callback.empty,
    _ => Callback.empty,
    Global.State.Loading(initialProjectLibrary.withoutMetaData, initialSupp),
    ww,
    LoggerJs.off) {

  override def toString = unsafeState() match {
    case Global.State.Active (pl, s) => s"TestGlobal(Active($pl, $s))"
    case Global.State.Loading(pl, s) => s"TestGlobal(Loading($pl, $s))"
  }

  override val localStorage: AbstractWebStorage.InMemory =
    AbstractWebStorage.inMemory()

  val reauth = TestReauthenticationModal(Some(\/-(Allow)))

  val optionalFullscreen = TestOptionalFullscreen()

  override val reauthModal = reauth.modal(username)(localStorage)

  override protected def unsafeNow() = now
  var now = Instant.now()
  def advanceTime(d: Duration): Unit = now = now.plusNanos(d.getNano).plusSeconds(d.getSeconds)
  def advanceTimeByMs(ms: Long) = advanceTime(Duration.ofMillis(ms))

  lazy val protocol = ProjectSpaProtocols.WebSocket(initialProjectLibrary.latestMetaData.id, creator)

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
        onUpdateAccess          = failLeft,
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
        localStorage  = localStorage,
        logger        = logger)
  }

  val nextEventOrd: CallbackTo[EventOrd] =
    CallbackTo {
      val s = unsafeState().asInstanceOf[Global.State.Active].projectLibrary
      // assert(s.futureEvents.isEmpty, s"TestGlobal.nextEventOrd: s.futureEvents = ${s.futureEvents.map(_.ord.value)}")
      s.latest.history.nextOrd
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
    pxProject.toCallback.map(verifyEvents(_)(eventList: _*))
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

  override def reconnect(ps: ProjectLibrary): Callback =
    Callback.empty

  private def autoEventResponse: TestGlobal#Req => Option[TestGlobal#Response] = {
    type MsgFnIn   [I]              = I
    type MsgFnOut  [O]              = Option[CallbackTo[O]]
    type MsgFoldIn [R <: WsReqRes]  = MsgFnIn[R#RequestType]
    type MsgFoldOut[R <: WsReqRes]  = MsgFnOut[R#ResponseType]
    type MsgFn     [I]              = MsgFnIn[I] => MsgFnOut[WsReqRes.EventResult]

    val supplimentaryDataForEvents: VerifiedEvent.Seq => CallbackTo[Supplimentary] =
      SupplimentaryLogic[CallbackTo](
        needUsernamesByUserId = ids => CallbackTo(ids.iterator.map { id =>
          val publicId = Obfuscators.userId.obfuscate(id)
          val username = TestGlobal.inverseUserDb.get(publicId).flatMap(_.left.toOption).getOrElse(
            throw new IllegalStateException(s"Username not found for user ID $id (obfuscated: $publicId)")
          )
          id -> username
        }.toMap),
        obfuscate             = Obfuscators.userId.obfuscate,
        deobfuscate           = Obfuscators.userId.deobfuscateOrThrow,
      )

    def updateProject[I](mkEvent: (I, Project) => MakeEvent.Result, requiredRole: ProjectRole): MsgFn[I] = input => Some {
      def run(p1: Project): CallbackTo[WsReqRes.EventResult] = {

        // This logic is duplicated in ProjectSpaLogic
        def permCheck: PotentialChange[ErrorMsg, Unit] =
          p1.access.require(requiredRole, userId) match {
            case Allow => PotentialChange.unit
            case Deny  => PotentialChange.Failure(ErrorMsg(s"$requiredRole rights required."))
          }

        val result: PotentialChange[ErrorMsg, ApplyNewEvent.Updated] =
          for {
            _ <- permCheck
            er = mkEvent(input, p1)
            u <- ApplyNewEvent(er, p1)
          } yield u

        result match {

          case PotentialChange.Success(ApplyNewEvent.Updated(_, event)) =>
            for {
              ord  <- nextEventOrd
              ves   = VerifiedEvent.Seq.empty + VerifiedEvent(ord, event, Instant.now())
              supp <- supplimentaryDataForEvents(ves)
            } yield \/-(StateUpdate(ves, supp))

          case PotentialChange.Unchanged =>
            CallbackTo.pure(\/-(StateUpdate.empty))

          case PotentialChange.Failure(e) =>
            CallbackTo.pure(-\/(e))
        }
      }
      pxProject.toCallback.flatMap(run)
    }

    def updateProjectI[I](mkEvent: I => MakeEvent.Result, requiredRole: ProjectRole): MsgFn[I] =
      updateProject((i, _) => mkEvent(i), requiredRole)

    // This logic is duplicated in ProjectSpaLogic
    val msgFold = WsReqRes.Fold[MsgFoldIn, MsgFoldOut](
      onInitApp               = _ => None,
      onReconnect             = _ => None,
      onSync                  = _ => None,
      onUpdateConfig          = updateProject (MakeEvent.updateConfig, ProjectRole.Collaborator),
      onCreateContent         = updateProject (MakeEvent.createContent, ProjectRole.Collaborator),
      onUpdateContent         = updateProject (MakeEvent.updateContent, ProjectRole.Collaborator),
      onProjectNameSet        = updateProjectI(MakeEvent.projectNameSetFn, ProjectRole.Admin),
      onUpdateSavedViews      = updateProject (MakeEvent.updateSavedViews, ProjectRole.Collaborator),
      onUpdateManualIssues    = updateProject (MakeEvent.updateManualIssues, ProjectRole.Collaborator),
      onFieldMandatorinessMod = _ => None,
      onReqTypeImplicationMod = updateProjectI(MakeEvent.reqTypeImplicationMod, ProjectRole.Collaborator),
      onUpdateAccess          = cmd =>
        UpdateAccessCmd.resolve[CallbackTo, MsgFoldOut[WsReqRes.UpdateAccess.type]](cmd)(
          userId     = userId,
          getUserId  = u => CallbackTo(TestGlobal.userDb.get(u)),
          onNotFound = Some(CallbackTo.pure(-\/(ErrorMsg("User not found.")))),
          modify     = (m, p) => CallbackTo.pure(updateProject(MakeEvent.updateAccess, p)(m))
        ).runNow(),
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

  unsafeModState(s => Global.State.Active(initialProjectLibrary, s.supp))
  wsClient.connect.runNow()
  ww.replaceOnPush(onWebWorkerPush).runNow()
}

object TestGlobal {

  val userDb: Map[Username \/ EmailAddr, UserId.Public] = Map(
    -\/(Username1) -> PublicUserId1,
    -\/(Username2) -> PublicUserId2,
    -\/(Username3) -> PublicUserId3,
    -\/(Username4) -> PublicUserId4,
  )

  val inverseUserDb = userDb.iterator.map(_.swap).toMap

  def rolodexForProject(p: Project): Rolodex =
    Rolodex(p.access.asMap.map { case (id, _) =>
      id -> userDb.find { case (_, pubId) => pubId ==* id }.get._1.swap.toOption.get
    })

  def apply(p       : Project                  = Project.empty,
            userId  : UserId.Public            = PublicUserId1,
            username: Username                 = Username1,
            creator : ProjectCreator           = ProjectCreator(PublicUserId1),
            ww      : WebWorkerClient.Instance = TestWebWorkerClient(),
           ): TestGlobal = {
    val p2 = if (p.access.asMap.nonEmpty) p else p.copy(access = ProjectAccess.init(creator))
    val md = looseProjectMetaData(p2, eventsTotal = p2.ordAsInt)
    val ps = ProjectLibrary.WithMetaData.init(creator, p2, md, userId, CacheJs(creator))
    val supp = Supplimentary(rolodexForProject(p2))
    new TestGlobal(ps, userId, username, creator, supp, ww)
  }

  import shipreq.webapp.base.test.TestState._

  final class Obs($: DomZipperJs, g: TestGlobal) {
    val activeElement       = Option(document.activeElement).filter(_ ne document.body)
    val reqs                = g.reqs()
    val reauthModal         = TestReauthenticationModal.Obs(g.reauthModal.id)($.parent.parent, g.reauth)
    val fullscreenCount     = $.collect0n(BaseStyles.fullscreen.selector).size
    val isBrowserFullscreen = g.optionalFullscreen.currentlyFullscreen

    def needFocus() =
      activeElement.getOrElse(throw new RuntimeException("Nothing is focused."))
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
    protected implicit def autoObs(o: O): Obs =
      getObs(o)

    val activeElement = *.focus("activeElement").value(_.obs.activeElement.orNull)

    val fullscreenCount = *.focus("Fullscreen elements").value(_.obs.fullscreenCount)

    val isBrowserFullscreen = *.focus("Browser is fullscreen").value(_.obs.isBrowserFullscreen)

    val requestCount = *.focus("Server requests").value(_.obs.reqs.length)

    val lastRequest = *.focus("Last request").option(_.obs.reqs.lastOption)

    val lastRequestMsg = *.focus("Last request").option(_.obs.reqs.lastOption.map(_.req.req: Any))

    val lastTwoRequests = *.focus("Last two requests").compare(_.obs.reqs.last, _.obs.reqs.init.last)

    val assertLastTwoRequestsAreEqual = lastTwoRequests.map(_.req).assert.equal(Equal.by_==, implicitly)

    def press(k: SimEvent.Keyboard): *.Actions =
      *.action(s"Press ${k.desc}.") { x =>
        x.obs.activeElement match {
          case Some(f) => k.simulateKeyDownPressUp(f)
          case None    => manualKeyPress(document, k)
        }
      }

    private def manualKeyPress(target: EventTarget, k: SimEvent.Keyboard): Unit = {
      dispatchEvent(target, "keypress", e => k.assign(e.asInstanceOf[js.Dynamic]))
    }

    def assertFocusBy(desc: String, f: *.OS => html.Element) =
      *.point(s"$desc must have focus") { os =>
        val actual = os.obs.needFocus()
        val expect = f(os)
        Option.unless(actual == expect) {
          val h = Option(actual).fold("∅")(_.outerHTML.replace('\n', ' ').take(600))
          s"Focused is $h"
        }
      }

    def assertCellHasFocus(f: CommonObs.Editor.TestDsl[R, O, S]) =
      assertFocusBy(f.field + " cell", f.cellDom.run)

    def assertEditorHasFocus(f: CommonObs.Editor.TestDsl[R, O, S]) =
      assertFocusBy(f.field + " editor", f.editorDom.run(_).orNull)

    def assertLastRequestMsg(msg: Any) =
      lastRequestMsg.assert(Some(msg))(Equal.by_==, implicitly)
  }
}
