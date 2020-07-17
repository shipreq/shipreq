package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.scalaz_ext.ScalazMacros
import scalaz.{Equal, Name}
import shipreq.base.util.{BinaryData, Direction}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol.websocket.WebSocketShared.ReqId
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.Text
import shipreq.webapp.server.logic.ProjectSpaLogic.{WebSocketState => _, _}
import shipreq.webapp.server.logic.Redis.ProjectSnapshot
import shipreq.webapp.server.logic.Security.{SessionId, SessionToken}
import shipreq.webapp.server.logic.dispatch.Cookie
import utest._

object ProjectSpaLogicTestS extends ProjectSpaLogicTest(Config.default.copy(writeEvents = false))
object ProjectSpaLogicTestE extends ProjectSpaLogicTest(Config.default.copy(writeSnapshots = false))

object ProjectSpaLogicTest {
  private sealed trait CacheState
  private object CacheState {
    final case class Empty     (desc: String) extends CacheState
    final case class UpToDate  (desc: String) extends CacheState
    final case class Stale     (desc: String) extends CacheState
    final case class Incomplete(desc: String) extends CacheState
  }
}

abstract class ProjectSpaLogicTest(cfg: Config) extends TestSuite {
  import ProjectSpaLogicTest._

  private implicit def univEqMsgError: UnivEq[MsgError] = UnivEq.force
  private implicit def univEqStatic: UnivEq[WebSocketStatic] = UnivEq.force

  private type WebSocketState = ProjectSpaLogic.WebSocketState[Name]
  private val emptyState      = ProjectSpaLogic.WebSocketState.empty[Name]
  private val subscribedState = ProjectSpaLogic.WebSocketState[Name](Some(null))

  protected implicit val equalSub: Equal[Redis.Subscription[Name]] = Equal.equal((_, _) => true)
  private implicit val equalState = ScalazMacros.deriveEqual[WebSocketState]
  private implicit val eqInitAppData = ScalazMacros.deriveEqual[InitAppData]

  private val initAppMsg = WsReqRes.InitApp.AndReq(())
  private val cmdNewUC = CreateContentCmd.CreateUseCase(Set.empty, Map.empty, Direction.Values.both(Set.empty), Set.empty, Text.empty)
  private val newUC = WsReqRes.CreateContent.AndReq(cmdNewUC)

  private class Tester extends MockInterpreters(_.copy(projectSpa = cfg)) {
    def broadcastAll(): Unit =
      redis.publishAll.value

    implicit def tokenToCookieLookup(t: SessionToken[Any]): Cookie.LookupFn =
      security.sessionPersist(t).value.add.map(c => c.name -> c.value).toMap.get

    implicit def otokenToCookieLookup(o: Option[SessionToken[Any]]): Cookie.LookupFn =
      o match {
        case Some(t) => tokenToCookieLookup(t)
        case None    => _ => None
      }

    implicit def pidToPublic(a: ProjectId): ProjectId.Public =
      Obfuscators.projectId.obfuscate(a)

    object p1 {

      val events = Vector[Event](
        Event.ProjectTemplateApply(ProjectTemplate.V1),
        Event.ProjectNameSet("hell"),
        Event.ProjectNameSet("hello"),
      )

      val verifiedEvents       = verifyEvents(Project.empty)(events: _*)
      val instance             = applyVerifiedEventSuccessfully(Project.empty, verifiedEvents.toList: _*)
      val latestOrd            = verifiedEvents.last.ord.asLatest
      val id                   = db.createProject(user2.id, events.map(_.active), instance).value
      val data1                = db.getProjectMetaData(id).value.get

      lazy val projectAndOrd   = ProjectAndOrd(instance, Some(verifiedEvents.last.ord.asLatest))
      lazy val initAppData     = InitAppData(projectAndOrd, data1)
      lazy val static          = WebSocketStatic(user2.toUser, id, SessionId.random(), (), svr.now.value, svr.now.value.plusSeconds(99999))

      lazy val eventsA         = events.take(1)
      lazy val verifiedEventsA = verifiedEvents.take(1)
      lazy val instanceA       = applyVerifiedEventSuccessfully(Project.empty, verifiedEventsA.toList: _*)
      lazy val latestOrdA      = verifiedEventsA.last.ord.asLatest

      lazy val eventsB         = events.drop(1).take(1)
      lazy val verifiedEventsB = verifiedEvents.drop(1).take(1)
      lazy val instanceB       = applyVerifiedEventSuccessfully(instanceA, verifiedEventsB.toList: _*)
      lazy val latestOrdB      = verifiedEventsB.last.ord.asLatest

      lazy val eventsC         = events.drop(2)
      lazy val verifiedEventsC = verifiedEvents.drop(2)

      lazy val verifiedEventsBC = verifiedEventsB ++ verifiedEventsC
    }
  }

  private val pushProtocol = {
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null))
    implicit def picklerPush: SafePickler[ProjectSpaProtocols.WebSocket#Push] = p.push.codec
    WebSocketShared.protocolSC(_ => ???)
  }

  private def wsHelper(reqId: ReqId, responseType: WsReqRes) = {
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null))
    implicit def picklerReq: SafePickler[p.Req] = p.req.codec
    implicit def picklerPush: SafePickler[p.Push] = p.push.codec
    val responseUnpickler: ReqId => Option[Protocol[SafePickler]] = i => if (i.value == reqId.value) Some(responseType.protocolRes) else ???
    new WebSocketServerHelper[p.Req, p.Push](
      WebSocketShared.protocolCS,
      WebSocketShared.protocolSC(responseUnpickler))
  }

  private def sendMsg(msg: WsReqRes.AndReq, static: WebSocketStatic, state: WebSocketState)(implicit t: Tester) = {
    import t._
    val reqId = ReqId(7)
    val h = wsHelper(reqId, msg.reqRes)
    val msgBin = h.protocolCS.codec.encode((reqId, msg))
    var resp: MsgError \/ BinaryData = null
    @nowarn
    val proc = projectSpa.onMessage(
      static          = static,
      state           = state,
      msg             = msgBin,
      respond         = a => Name{resp = \/-(a); \/-(())},
      push            = e => Name(???),
      onListenerError = e => Name(???),
      onError         = a => Name{resp = -\/(a)}
    )
    val newState = proc.value
    val result: MsgError \/ msg.reqRes.ResponseType =
      resp match {
        case \/-(b) =>
          h.protocolSC.codec.decode(b) match {
            case \/-(\/-((reqId2, protocolAndValue))) =>
              assertEq(reqId2, reqId)
              val result = protocolAndValue.unsafeForceType[msg.reqRes.ResponseType].value
              \/-(result)
            case \/-(-\/(push)) =>
              fail("Received push: " + push)
            case -\/(err) =>
              fail(err.toString)
          }
        case -\/(e) => -\/(e)
      }
    (result, newState)
  }

  private def sendMsgAndBroadcast(msg: WsReqRes.AndReq, static: WebSocketStatic, state: WebSocketState)(implicit t: Tester): msg.reqRes.ResponseType = {
    val r = sendMsg(msg, static, state)._1.getOrThrow()
    t.broadcastAll()
    r
  }

  private def keepAlive(static: WebSocketStatic)(implicit t: Tester): Option[MsgError] = {
    import t._
    var result = Option.empty[MsgError]
    projectSpa.onKeepAlive(static, msg => Name {result = Some(msg)}).value
    result
  }

  private def onPush(f: VerifiedEvent.NonEmptySeq => Unit): BinaryData => Name[Unit] =
    bin => Name {
      val value = pushProtocol.codec.decode(bin).getOrThrow()
      val push = value.swap.getOrThrow()
      f(push)
    }

  private def withAllCacheConfig(run: Tester => CacheState => Unit): Unit = {
    import CacheState._

    def runWith(c: CacheState)(setup: Tester => Unit): Unit = {
      val t = new Tester
      setup(t)
      run(t)(c)
    }

    def z = VerifiedEvent.Seq.empty

    runWith(Empty(""))(_ => ())

    runWith(UpToDate("snapshot")) { t => import t._
      redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instance, p1.latestOrd), z).value
    }

    runWith(UpToDate("events")) { t => import t._
      redis.writeEvents(p1.id, p1.verifiedEvents, z).value
    }

    runWith(UpToDate("both")) { t => import t._
      redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceB, p1.latestOrdB), z).value
      redis.writeEvents(p1.id, p1.verifiedEventsC, z).value
    }

    runWith(Stale("snapshot")) { t => import t._
      redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceA, p1.latestOrdA), z).value
    }

    runWith(Stale("events")) { t => import t._
      redis.writeEvents(p1.id, p1.verifiedEventsA, z).value
    }

    runWith(Stale("both")) { t => import t._
      redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceA, p1.latestOrdA), z).value
      redis.writeEvents(p1.id, p1.verifiedEventsB, z).value
    }

    runWith(Incomplete("")) { t => import t._
      redis.writeEvents(p1.id, p1.verifiedEventsC, z).value
    }
  }

  override def tests = Tests {

    "msgName" - {
      val n = WsReqRes.InitApp.name
      assertEq(n, "InitApp")
    }

    "connect" - {
      implicit val t = new Tester; import t._
      import ConnectRejection._

      def test(c: Cookie.LookupFn, p: ProjectId.Public)(expect: ConnectRejection \/ (WebSocketStatic, WebSocketState)): Unit =
        assertProtected {
          val a = projectSpa.onConnect(c, p).value
          assertEq(a, expect)
        }

      "noSession"        - test(None, p1.id)(-\/(NoSession))
      "anonymousSession" - test(SessionToken.anonymous(), p1.id)(-\/(AnonymousSession))
      "invalidProjectId" - test(user2.token, Obfuscated("!"))(-\/(InvalidProjectId))
      "projectNotFound"  - test(user2.token, ProjectId(23432))(-\/(ProjectNotFound))
      "accessDenied"     - test(user3.token, p1.id)(-\/(AccessDenied))
      "ok"               - test(user2.token, p1.id)(\/-((p1.static.copy(sessionId = user2.token.sessionId, expiresAt = security.expiry()), emptyState)))
    }

    "initApp" - {
      def test(expectCacheWrites: Int, expectFullDbReads: Int)(implicit t: Tester): Unit = {
        import t._
        assertDifference("full db reads", db.loadProjectLog.length)(expectFullDbReads) {
          assertDifference("redis writes", redis.writeCount())(expectCacheWrites) {

            val result = sendMsg(initAppMsg, p1.static, subscribedState)
            val actual = result._1
            val expect = \/-(p1.initAppData)
            assertEq(actual, \/-(expect))
            assert(result._2.isEmpty)

            val cache = redis.read(p1.id).value.getOrThrow()
            assertEq(cache.ord, Some(p1.latestOrd))
          }
        }
      }
      withAllCacheConfig { implicit t => {
        case _: CacheState.Empty      => test(1, 1)
        case _: CacheState.UpToDate   => test(0, 0)
        case _: CacheState.Stale      => test(1, 1)
        case _: CacheState.Incomplete => test(1, 1)
      }}
    }

    "expiry" - {
      implicit val t = new Tester; import t._
      val \/-((static, state)) = projectSpa.onConnect(user2.token, p1.id).value
      def twice(f: => Unit) = {f; f}

      twice {
        val msgResult = sendMsg(newUC, static, state)._1
        assert(msgResult.isRight)

        val kaResult = keepAlive(static)
        assertEq(kaResult, None)
      }

      svr.incTime(config.security.jwtLifespan)
      svr.incTimeSec(1)

      twice {
        val msgResult = sendMsg(newUC, static, state)._1
        assertEq(msgResult, -\/(MsgError.SessionExpired))

        val kaResult = keepAlive(static)
        assertEq(kaResult, Some(MsgError.SessionExpired))
      }
    }

    "updateProject" - {
      def test(c: CacheState, expectCacheWrites: Int, expectFullDbReads: Int)(implicit t: Tester): Unit = {
        import t._
        assertDifference(s"[$c] db reads", db.loadProjectLog.length)(expectFullDbReads) {
          assertDifference(s"[$c] redis writes", redis.writeCount())(expectCacheWrites) {

            val result = sendMsg(newUC, p1.static, subscribedState)
            val actual = result._1
            val expect = \/-(List(EventOrd(4)))
            assertEq(s"[$c] response", actual.map(_.map(_.toList.map(_.ord))), \/-(expect))
            assert(result._2.isEmpty)

            val cache = redis.read(p1.id).value.getOrThrow()
            assertEq(s"[$c] cache state", cache.ord, Some((p1.latestOrd.asEventOrd + 1).asLatest))
          }
        }
      }
      withAllCacheConfig { implicit t => {
        case c: CacheState.Empty      => test(c, 2, 1)
        case c: CacheState.UpToDate   => test(c, 1, 0)
        case c: CacheState.Stale      => test(c, 2, 1)
        case c: CacheState.Incomplete => test(c, 2, 1)
      }}
    }

    "updatesAndListeners" - {
      implicit val t = new Tester; import t._
      val static = p1.static

      var recv1     = Vector.empty[VerifiedEvent.NonEmptySeq]
      val subState1 = projectSpa.onOpen(static, emptyState, onPush(recv1 :+= _), _ => ???).value
      val initData1 = sendMsgAndBroadcast(initAppMsg, static, subState1).getOrThrow()
      assertEq("[1]", recv1, Vector.empty)

      val ves1 = sendMsgAndBroadcast(newUC, static, subState1).getOrThrow().needNES
      assertEq("[2]", recv1, Vector(ves1))

      var recv2     = Vector.empty[VerifiedEvent.NonEmptySeq]
      val subState2 = projectSpa.onOpen(static, emptyState, onPush(recv2 :+= _), _ => ???).value
      val initData2 = sendMsgAndBroadcast(initAppMsg, static, subState2).getOrThrow()
      assertEq("[3]", recv2, Vector.empty)
      assertEq("[4]", recv1, Vector(ves1))
      assertEq("[5]", initData2.project.ord, Some(initData1.project.nextOrd.asLatest))
      assertEq("[6]", initData2.project.project.content.reqs.size, 1)

      val ves2 = sendMsgAndBroadcast(newUC, static, subState2).getOrThrow().needNES
      assertEq("[7]", recv1, Vector(ves1, ves2))
      assertEq("[8]", recv2, Vector(ves2))

      val ves3 = sendMsgAndBroadcast(newUC, static, subState1).getOrThrow().needNES
      assertEq("[9]", recv1, Vector(ves1, ves2, ves3))
      assertEq("[A]", recv2, Vector(ves2, ves3))

      subState1.sub.get.unsubscribe.value
      val ves4 = sendMsgAndBroadcast(newUC, static, subState2).getOrThrow().needNES
      assertEq("[B]", recv1, Vector(ves1, ves2, ves3))
      assertEq("[C]", recv2, Vector(ves2, ves3, ves4))
    }

    "reconnect" - {

      "current" - {
        implicit val t = new Tester; import t._
        assertDifference("db reads", db.loadProjectLog.length)(0) {
          val (\/-(events), newState) = sendMsg(WsReqRes.Reconnect.AndReq(p1.projectAndOrd.ord), p1.static, emptyState)
          assertEq(events, VerifiedEvent.Seq.empty)
          assert(newState.exists(_.sub.isDefined))
        }
      }

      "stale" - {
        def test(c: CacheState, expectFullDbReads: Int)(implicit t: Tester): Unit = {
          import t._
          import IgnoreEqualityOfVerifiedEventTimestamps._
          assertDifference(s"[$c] db reads", db.loadProjectLog.length)(expectFullDbReads) {
            val (\/-(events), newState) = sendMsg(WsReqRes.Reconnect.AndReq(Some(p1.latestOrdA)), p1.static, emptyState)
            assertEq(events, p1.verifiedEventsBC)
            assert(newState.exists(_.sub.isDefined))
          }
        }
        withAllCacheConfig { implicit t => {
          case c: CacheState.Empty              => test(c, 1)
          case c@ CacheState.UpToDate("events") => test(c, 0)
          case c@ CacheState.UpToDate(_)        => test(c, 1)
          case c: CacheState.Stale              => test(c, 1)
          case c: CacheState.Incomplete         => test(c, 1)
        }}
      }
    }

    "sync" - {
      implicit val t = new Tester; import t._
      val ords     = NonEmptySet(1, 3, 666).map(EventOrd(_))
      var recv     = Vector.empty[VerifiedEvent.NonEmptySeq]
      val subState = projectSpa.onOpen(p1.static, emptyState, onPush(recv :+= _), _ => ???).value
      val (\/-(_), newState) = sendMsg(WsReqRes.Sync.AndReq(ords), p1.static, subState)
      assertEq(recv, Vector.empty)
      assert(newState.isEmpty)

      broadcastAll()
      val actual = recv.flatMap(_.values.iterator.map(_.ord)).toSet
      val expect = ords.whole.filter(_ <= p1.verifiedEvents.last.ord)
      assertEq(actual, expect)
    }

    "dataPropFailure" - {
      implicit val t = new Tester; import t._
      val subState = projectSpa.onOpen(p1.static, emptyState, onPush(_ => ()), _ => ???).value
      val text = ArraySeq(Text.UseCaseTitle.Literal(" "))
      val cmd = CreateContentCmd.CreateUseCase(Set.empty, Map.empty, Direction.Values.both(Set.empty), Set.empty, text)
      val cmdWS = WsReqRes.CreateContent.AndReq(cmd)
      def sendBadMsg() = {
        val result = sendMsg(cmdWS, p1.static, subState)
        val err = result._1.getOrThrow().getLeftOrThrow()
        assertContains(err.value, "Our staff have been notified")
      }
      sendBadMsg()
      assertEq(taskman.msgs.length, 1)
      sendBadMsg()
      assertEq(taskman.msgs.length, 1)
    }

  }
}
