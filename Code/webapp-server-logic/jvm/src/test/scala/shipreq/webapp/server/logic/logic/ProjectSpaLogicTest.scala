package shipreq.webapp.server.logic.logic

import cats.{Eq, Eval}
import japgolly.microlibs.cats_ext.CatsMacros
import shipreq.base.util.{BinaryData, Direction, ErrorMsg}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.websocket.WebSocketShared.ReqId
import shipreq.webapp.base.protocol.websocket._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WebSocket.Push
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.{InitAppData, StateUpdate, Supplimentary, WsReqRes}
import shipreq.webapp.member.project.protocol.websocket._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes.projectCreatorFromUserId
import shipreq.webapp.server.logic.algebra.Redis
import shipreq.webapp.server.logic.algebra.Redis.ProjectSnapshot
import shipreq.webapp.server.logic.algebra.Security.{SessionId, SessionToken}
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.logic.ProjectSpaLogic.{WebSocketState => _, _}
import shipreq.webapp.server.logic.test.MockInterpreters
import shipreq.webapp.server.logic.util.Obfuscators
import sourcecode.Line
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

  private[ProjectSpaLogicTest] object Internals {

    implicit def pushFromVerifiedEventsNE[A](es: A)(implicit f: A => VerifiedEvent.NonEmptySeq): Push =
      StateUpdate(es.values, Supplimentary.empty)

    implicit def nonErrorMsgResponse(a: MsgError \/ StateUpdate): MsgError \/ (ErrorMsg \/ StateUpdate) =
      a.map(\/-(_))

    implicit def univEqMsgError: UnivEq[MsgError] = UnivEq.force
    implicit def univEqStatic: UnivEq[WebSocketStatic] = UnivEq.force

    def assertResponse(actual: MsgError \/ (ErrorMsg \/ StateUpdate))(implicit q: Line): AssertResponseDsl =
      new AssertResponseDsl(None, actual)

    def assertResponse(name: String, actual: MsgError \/ (ErrorMsg \/ StateUpdate))(implicit q: Line): AssertResponseDsl =
      new AssertResponseDsl(Option(name), actual)

    def assertStateUpdate(actual: StateUpdate)(implicit q: Line): AssertResponseDsl =
      assertResponse(\/-(\/-(actual)))

    def assertStateUpdate(name: String, actual: StateUpdate)(implicit q: Line): AssertResponseDsl =
      assertResponse(name, \/-(\/-(actual)))

    final class AssertResponseDsl(name: Option[String], actual: MsgError \/ (ErrorMsg \/ StateUpdate))(implicit q: Line) {
      def expectEventOrds(ords: EventOrd*): AssertResponseDsl2 =
        new AssertResponseDsl2(supp => {
          type T = MsgError \/ (ErrorMsg \/ (List[EventOrd], Supplimentary))
          val a: T = actual.map(_.map(s => (s.events.iterator.map(_.ord).toList.sorted, s.supp)))
          val e: T = \/-(\/-((ords.toList.sorted, supp)))
          assertEqO(name, a, e)
        })

      @nowarn("cat=unused")
      def expectEvents(events: VerifiedEvent.Seq)(implicit ee: Eq[VerifiedEvent.Seq]): AssertResponseDsl2 = {
        // implicit val es: Eq[StateUpdate] = Eq.instance((x, y) => ee.eqv(x.events, y.events) && (x.supp ==* y.supp))
        val equalVerifiedEventSeq = () // shadow this
        implicit val es: Eq[StateUpdate] = CatsMacros.deriveEq
        new AssertResponseDsl2(supp => {
          val expect: MsgError \/ (ErrorMsg \/ StateUpdate) = \/-(\/-(StateUpdate(events, supp)))
          assertEqO(name, actual, expect)
        })
      }

      def expectNoEvents =
        expectEvents(VerifiedEvent.Seq.empty)
    }

    final class AssertResponseDsl2(f: Supplimentary => Unit) {
      def expectSupp(supp: Supplimentary = Supplimentary.empty): Unit =
        f(supp)

      def expectRolodex(r: Rolodex): Unit =
        expectSupp(Supplimentary(r))

      def expectRolodex(entries: (UserId.Public, Username)*): Unit =
        expectRolodex(Rolodex(entries.toMap))
    }
  }
}

abstract class ProjectSpaLogicTest(cfg: Config) extends TestSuite {
  import ProjectSpaLogicTest._
  import ProjectSpaLogicTest.Internals._

  private type WebSocketState = ProjectSpaLogic.WebSocketState[Eval]
  private val emptyState      = ProjectSpaLogic.WebSocketState.empty[Eval]
  private val subscribedState = ProjectSpaLogic.WebSocketState[Eval](Some(null))

  protected implicit val equalSub: Eq[Redis.Subscription[Eval]] = Eq.instance((_, _) => true)
  private implicit val equalState = CatsMacros.deriveEq[WebSocketState]
  private implicit val eqInitAppData = CatsMacros.deriveEq[InitAppData]

  private val initAppMsg_0 = WsReqRes.InitApp.AndReq(None)
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

    implicit def uidToPublic(a: UserId): UserId.Public =
      Obfuscators.userId.obfuscate(a)

    implicit def pidToPublic(a: ProjectId): ProjectId.Public =
      Obfuscators.projectId.obfuscate(a)

    db.addUsers(user2, user3)

    object p1 {

      val events = Vector[Event](
        Event.ProjectTemplateApply(ProjectTemplate.V1),
        Event.ProjectNameSet("hell"),
        Event.ProjectNameSet("hello"),
        Event.AccessUpdate(Map(user3.idP -> Some(ProjectPerm.Collaborator))),
      )

      val creator              = ProjectCreator(user2.idP)
      val emptyProject         = Project.init(creator)
      var verifiedEvents       = verifyEvents(emptyProject)(events: _*)
      var instance             = applyVerifiedEventSuccessfully(emptyProject, verifiedEvents.toList: _*)
      val latestOrd            = verifiedEvents.last.ord.asLatest
      val id                   = db.createProject(user2.id, events.map(_.active), instance, crypto.generateProjectKey()).value
      val data1                = db.getProjectMetaData(id, user2.id).value.get
      verifiedEvents           = db.getProjectEvents(id).value.getOrThrow()
      instance                 = applyVerifiedEventSuccessfully(emptyProject, verifiedEvents.toList: _*)
      db.loadProjectLog        = Vector.empty

      lazy val initRolodex     = Rolodex(Map(user2.idP -> user2.username, user3.idP -> user3.username))
      lazy val initAppData     = InitAppData(-\/(instance), data1, Supplimentary(initRolodex))
      lazy val static          = WebSocketStatic(user2.toUser, id, user2.id, SessionId.random(), (), svr.now.value, svr.now.value.plusSeconds(99999))

      lazy val eventsA         = events.take(1)
      lazy val verifiedEventsA = verifiedEvents.take(1)
      lazy val instanceA       = applyVerifiedEventSuccessfully(emptyProject, verifiedEventsA.toList: _*)
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
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null), Creator1)
    implicit def picklerPush: SafePickler[ProjectSpaProtocols.WebSocket#Push] = p.push.codec
    WebSocketShared.protocolSC(_ => ???)
  }

  private def wsHelper(reqId: ReqId, responseType: WsReqRes) = {
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null), Creator1)
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
      respond         = a => Eval.always{resp = \/-(a); \/-(())},
      push            = e => Eval.always(???),
      onListenerError = e => Eval.always(???),
      onError         = a => Eval.always{resp = -\/(a)}
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
    projectSpa.onKeepAlive(static, msg => Eval.always {result = Some(msg)}).value
    result
  }

  private def onPush(f: Push => Unit): BinaryData => Eval[Unit] =
    bin => Eval.always {
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

  private def getProject(i: InitAppData, prev: Project): Project =
    i.projectData match {
      case -\/(p) => p
      case \/-(e) => prev.updateOrThrow(e)
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
          val a = projectSpa.onConnect(c, p, user2.id).value
          assertEq(a, expect)
        }

      "noSession"        - test(None, p1.id)(-\/(NoSession))
      "anonymousSession" - test(SessionToken.anonymous(), p1.id)(-\/(AnonymousSession))
      "invalidProjectId" - test(user2.token, Obfuscated("!"))(-\/(InvalidProjectId))
      "projectNotFound"  - test(user2.token, ProjectId(23432))(-\/(AccessDenied))
      "accessDenied"     - test(db.newUserEntry().token, p1.id)(-\/(AccessDenied))
      "ok"               - test(user2.token, p1.id)(\/-((p1.static.copy(sessionId = user2.token.sessionId, expiresAt = security.expiry()), emptyState)))
      "collaboratorOk"   - test(user3.token, p1.id)(\/-((p1.static.copy(user = user3.toUser, sessionId = user3.token.sessionId, expiresAt = security.expiry()), emptyState)))
      "revokedAccess"    - {
        implicit val t = new Tester; import t._
        val u = db.newUserEntry()
        db.updateProjectAccess(p1.id, Set.empty, Map(u.id -> ProjectPerm.Collaborator)).value.getOrThrow()
        assert(projectSpa.onConnect(u.token, p1.id, user2.id).value.isRight)
        db.updateProjectAccess(p1.id, Set(u.id), Map.empty).value.getOrThrow()
        val a = projectSpa.onConnect(u.token, p1.id, user2.id).value
        assertEq(a, -\/(ConnectRejection.AccessDenied))
      }
    }

    "initApp" - {

      "total" - {
        def test(expectCacheWrites: Int, expectFullDbReads: Int)(implicit t: Tester): Unit = {
          import t._
          assertDifference("full db reads", db.loadProjectLog.length)(expectFullDbReads) {
            assertDifference("redis writes", redis.writeCount())(expectCacheWrites) {

              val result = sendMsg(initAppMsg_0, p1.static, subscribedState)
              val actual = result._1

              val noProjectData = -\/(p1.emptyProject)
              val actualWithoutProjectData = actual.map(_.map(_.copy(projectData = noProjectData)))
              val expectWithoutProjectData = \/-(p1.initAppData.copy(projectData = noProjectData))
              assertEq(actualWithoutProjectData, \/-(expectWithoutProjectData))

              assert(result._2.isEmpty)

              val initAppData = actual.getOrThrow().getOrThrow()

              val actualProject = getProject(initAppData, p1.emptyProject)
              assertEq(actualProject, p1.instance)

              val expectSupp = Supplimentary(p1.initRolodex)
              assertEq(initAppData.supp, expectSupp)

              val cache = redis.read(p1.id).value.getOrThrow()
              assertEq(cache.ord, p1.instance.ord)
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

      "partial" - {
        val client = Option(EventOrd.Latest(2))
        val initAppMsg = WsReqRes.InitApp.AndReq(client)
        def test(expectCacheWrites: Int, expectFullDbReads: Int)(implicit t: Tester): Unit = {
          import t._
          assertDifference("full db reads", db.loadProjectLog.length)(expectFullDbReads) {
            assertDifference("redis writes", redis.writeCount())(expectCacheWrites) {

              val result = sendMsg(initAppMsg, p1.static, subscribedState)
              val actual = result._1

              val expectedEvents = p1.verifiedEvents.filter(_.ord > client)
              assert(expectedEvents.nonEmpty)
              val expect = \/-(p1.initAppData.copy(projectData = \/-(expectedEvents)))
              assertEq(actual, \/-(expect))

              assert(result._2.isEmpty)

              val initAppData = actual.getOrThrow().getOrThrow()
              val expectSupp = Supplimentary(p1.initRolodex)
              assertEq(initAppData.supp, expectSupp)

              val cache = redis.read(p1.id).value.getOrThrow()
              assertEq(cache.ord, p1.instance.ord)
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
    }

    "expiry" - {
      implicit val t = new Tester; import t._
      val \/-((static, state)) = projectSpa.onConnect(user2.token, p1.id, user2.id).value
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

    "updateAccess" - {
      def test(c: CacheState, expectCacheWrites: Int, expectFullDbReads: Int)(implicit t: Tester): Unit = {
        import t._

        val u = db.newUserEntry()
        val cmd = UpdateAccessCmd.Modify(Map(u.idP -> Some(ProjectPerm.Collaborator)))
        val req = WsReqRes.AccessUpdate.AndReq(cmd)

        assertDifference(s"[$c] db reads", db.loadProjectLog.length)(expectFullDbReads) {
          assertDifference(s"[$c] redis writes", redis.writeCount())(expectCacheWrites) {

            val result = sendMsg(req, p1.static, subscribedState)
            val actual = result._1

            assertResponse(s"[$c] response", actual)
              .expectEventOrds(EventOrd(p1.events.length + 1))
              .expectSupp(Supplimentary(Rolodex.init(u.idP, u.username)))

            assert(result._2.isEmpty)

            val cache = redis.read(p1.id).value.getOrThrow()
            assertEq(s"[$c] cache state", cache.ord, Some((p1.latestOrd.asEventOrd + 1).asLatest))
          }
        }
      }

      "modify" - withAllCacheConfig { implicit t => {
        case c: CacheState.Empty      => test(c, 2, 1)
        case c: CacheState.UpToDate   => test(c, 1, 0)
        case c: CacheState.Stale      => test(c, 2, 1)
        case c: CacheState.Incomplete => test(c, 2, 1)
      }}

      "add" - {
        implicit val t = new Tester; import t._
        val u = db.newUserEntry()
        val cmd = UpdateAccessCmd.Add(-\/(u.username), ProjectPerm.Collaborator)
        val req = WsReqRes.AccessUpdate.AndReq(cmd)
        val result = sendMsg(req, p1.static, subscribedState)._1
        assertResponse(result)
          .expectEventOrds(EventOrd(p1.events.length + 1))
          .expectSupp(Supplimentary(Rolodex.init(u.idP, u.username)))
      }

      "addNotFound" - {
        implicit val t = new Tester; import t._
        val cmd = UpdateAccessCmd.Add(-\/(Username("nobody")), ProjectPerm.Collaborator)
        val req = WsReqRes.AccessUpdate.AndReq(cmd)
        val result = sendMsg(req, p1.static, subscribedState)._1
        assertEq(result, \/-(-\/(ErrorMsg("User not found."))))
      }

      "denyCollaborator" - {
        implicit val t = new Tester; import t._
        val static3 = p1.static.copy(user = user3.toUser)
        val cmd = UpdateAccessCmd.Modify(Map(user2.idP -> None))
        val req = WsReqRes.AccessUpdate.AndReq(cmd)
        val result = sendMsg(req, static3, subscribedState)._1
        assertEq(result, \/-(-\/(ErrorMsg("Admin rights required."))))
      }
    }

    "updatesAndListeners" - {
      implicit val t = new Tester; import t._
      val static = p1.static

      var recv1     = Vector.empty[Push]
      val subState1 = projectSpa.onOpen(static, emptyState, onPush(recv1 :+= _), _ => ???).value
      val initData1 = sendMsgAndBroadcast(initAppMsg_0, static, subState1).getOrThrow()
      val project1  = getProject(initData1, p1.emptyProject)
      assertEq("[1]", recv1, Vector.empty)

      val res1 = sendMsgAndBroadcast(newUC, static, subState1).getOrThrow()
      assertEq("[2]", recv1, Vector[Push](res1))

      var recv2     = Vector.empty[Push]
      val subState2 = projectSpa.onOpen(static, emptyState, onPush(recv2 :+= _), _ => ???).value
      val initData2 = sendMsgAndBroadcast(initAppMsg_0, static, subState2).getOrThrow()
      val project2  = getProject(initData2, p1.emptyProject)
      assertEq("[3]", recv2, Vector.empty)
      assertEq("[4]", recv1, Vector[Push](res1))
      assertEq("[5]", project2.ord, Some(project1.history.nextOrd.asLatest))
      assertEq("[6]", project2.content.reqs.size, 1)

      val res2 = sendMsgAndBroadcast(newUC, static, subState2).getOrThrow()
      assertEq("[7]", recv1, Vector[Push](res1, res2))
      assertEq("[8]", recv2, Vector[Push](res2))

      val res3 = sendMsgAndBroadcast(newUC, static, subState1).getOrThrow()
      assertEq("[9]", recv1, Vector[Push](res1, res2, res3))
      assertEq("[A]", recv2, Vector[Push](res2, res3))

      subState1.sub.get.unsubscribe.value
      val res4 = sendMsgAndBroadcast(newUC, static, subState2).getOrThrow()
      assertEq("[B]", recv1, Vector[Push](res1, res2, res3))
      assertEq("[C]", recv2, Vector[Push](res2, res3, res4))
    }

    "reconnect" - {

      "current" - {
        implicit val t = new Tester; import t._
        assertDifference("db reads", db.loadProjectLog.length)(0) {
          val (res, newState) = sendMsg(WsReqRes.Reconnect.AndReq(p1.instance.ord), p1.static, emptyState)
          assertResponse(res)
            .expectNoEvents
            .expectSupp()
          assert(newState.exists(_.sub.isDefined))
        }
      }

      "stale" - {
        def test(c: CacheState, expectFullDbReads: Int)(implicit t: Tester): Unit = {
          import t._
          import IgnoreEqualityOfVerifiedEventTimestamps._
          assertDifference(s"[$c] db reads", db.loadProjectLog.length)(expectFullDbReads) {
            val (res, newState) = sendMsg(WsReqRes.Reconnect.AndReq(Some(p1.latestOrdA)), p1.static, emptyState)
            assertResponse(res)
              .expectEvents(p1.verifiedEventsBC)
              .expectSupp(Supplimentary(Rolodex(Map(user3.idP -> user3.username))))
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
      var recv     = Vector.empty[Push]
      val subState = projectSpa.onOpen(p1.static, emptyState, onPush(recv :+= _), _ => ???).value
      val (\/-(_), newState) = sendMsg(WsReqRes.Sync.AndReq(ords), p1.static, subState)
      assertEq(recv, Vector.empty)
      assert(newState.isEmpty)

      broadcastAll()

      val recvAll = recv.reduce(_ ++ _)

      assertStateUpdate(recvAll)
        .expectEventOrds(ords.whole.toSeq.filter(_ <= p1.verifiedEvents.last.ord): _*)
        .expectSupp()
    }

    "initPage" - {
      "noAccess" - {
        implicit val t = new Tester; import t._
        val u2 = db.newUserEntry()
        val result = projectSpa.initPage(p1.id, u2.id, u2.username, assetManifest).value
        assertEq(result, None)
      }
      "revokedAccess" - {
        implicit val t = new Tester; import t._
        val u2 = db.newUserEntry()
        db.updateProjectAccess(p1.id, Set.empty, Map(u2.id -> ProjectPerm.Collaborator)).value.getOrThrow()
        assert(projectSpa.initPage(p1.id, u2.id, u2.username, assetManifest).value.isDefined)
        db.updateProjectAccess(p1.id, Set(u2.id), Map.empty).value.getOrThrow()
        val result = projectSpa.initPage(p1.id, u2.id, u2.username, assetManifest).value
        assertEq(result, None)
      }
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
