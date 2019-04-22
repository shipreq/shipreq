package shipreq.webapp.server.logic

import boopickle.Pickler
import japgolly.microlibs.scalaz_ext.ScalazMacros
import scalaz.{-\/, Equal, Name, \/, \/-}
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol2.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol2.WebSocketShared.ReqId
import shipreq.webapp.base.protocol2._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.server.logic.ProjectSpaLogic.{WebSocketState => _, _}
import shipreq.webapp.server.logic.Redis.ProjectSnapshot
import shipreq.webapp.server.logic.Security.SessionToken

object ProjectSpaLogicTest extends TestSuite {

  private type WebSocketState = ProjectSpaLogic.WebSocketState[Name]
  private val  WebSocketState = ProjectSpaLogic.WebSocketState
  private val emptyState      = ProjectSpaLogic.WebSocketState.empty[Name]
  private val subscribedState = ProjectSpaLogic.WebSocketState[Name](Some(null))

  private implicit val equalSub: Equal[Redis.Subscription[Name]] = Equal.equal((_, _) => true)
  private implicit val equalState = ScalazMacros.deriveEqual[WebSocketState]
  private implicit val eqInitAppData = ScalazMacros.deriveEqual[InitAppData]

  private class Tester extends MockInterpreters {

    implicit def tokenToCookieLookup(t: SessionToken): Cookie.LookupFn =
      security.sessionPersist(t).value.add.map(c => c.name -> c.value).toMap.get

    implicit def otokenToCookieLookup(o: Option[SessionToken]): Cookie.LookupFn =
      o match {
        case Some(t) => tokenToCookieLookup(t)
        case None    => _ => None
      }

    implicit def pidToPublic(a: ProjectId): ProjectId.Public =
      Obfuscators.projectId.obfuscate(a)

    object p1 {

      val id = db.createEmptyProject(user2.id, 2).value

      val events = List[Event](
        ProjectTemplateApply(ProjectTemplate.V2),
        ProjectNameSet("hell"),
        ProjectNameSet("hello"),
      )

      val verifiedEvents = verifyEvents(Project.empty)(events: _*)

      val latestOrd = verifiedEvents.last.ord.asLatest

      for (e <- verifiedEvents)
        db.saveProjectEvent(id)(e.ord, e.event.asInstanceOf[ActiveEvent], e.hashRecs).value.foreach(throw _)

      val data1                = db.getProjectSpa1(id).value

      lazy val instance        = applyVerifiedEventSuccessfully(Project.empty, verifiedEvents.toList: _*)

      lazy val initAppData     = InitAppData(instance, verifiedEvents.last.ord.asLatest, data1.lastUpdatedOrCreatedAt)

      lazy val static          = WebSocketStatic(user2.toUser, id)

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
    }
  }

  private def wsHelper(reqId: ReqId, responseType: WsReqRes) = {
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null))
    implicit def picklerReq: Pickler[p.Req] = p.req.codec
    implicit def picklerPush: Pickler[p.Push] = p.push.codec
    val responseUnpickler: ReqId => Protocol[Pickler] = i => if (i.value == reqId.value) responseType.protocolRes else ???
    new WebSocketServerHelper[p.Req, p.Push](
      Protocol(WebSocketShared.protocolCS),
      Protocol(WebSocketShared.protocolSC(responseUnpickler)))
  }

  private def sendMsg(msg: WsReqRes.AndReq, static: WebSocketStatic)(implicit t: Tester): MsgError \/ (msg.reqRes.ResponseType, Option[WebSocketState]) = {
    import t._
    val reqId = ReqId(7)
    val h = wsHelper(reqId, msg.reqRes)
    val msgBin = BinaryJvm.encode(h.protocolCS)((reqId, msg))
    val resp = projectSpa.onMessage(static)(emptyState, msgBin).value
    resp match {
      case \/-((b, s)) =>
        BinaryJvm.unsafeDecode(b, h.protocolSC) match {
          case \/-((reqId2, protocolAndValue)) =>
            assertEq(reqId2, reqId)
            val result = protocolAndValue.unsafeForceType[msg.reqRes.ResponseType].value
            \/-((result, s))
          case -\/(push) =>
            fail("Received push: " + push)
        }
      case -\/(e) => -\/(e)
    }
  }

  override def tests = Tests {
    implicit val t = new Tester; import t._

    'connect {
      import ConnectRejection._

      def test(c: Cookie.LookupFn, p: ProjectId.Public)(expect: ConnectRejection \/ (WebSocketStatic, WebSocketState)): Unit =
        assertProtected {
          val a = projectSpa.onConnect(c, p).value
          assertEq(a, expect)
        }

      'noSession        - test(None, p1.id)(-\/(NoSession))
      'anonymousSession - test(SessionToken.anonymous, p1.id)(-\/(AnonymousSession))
      'invalidProjectId - test(user2.token, Obfuscated("!"))(-\/(InvalidProjectId))
      'projectNotFound  - test(user2.token, ProjectId(23432))(-\/(ProjectNotFound))
      'accessDenied     - test(user3.token, p1.id)(-\/(AccessDenied))
      'ok               - test(user2.token, p1.id)(\/-((p1.static, emptyState)))
    }

    'initApp - {
      def z = VerifiedEvent.Seq.empty

      def test(expectCacheWrites: Int, expectFullDbReads: Int): Unit =
        assertDifference("full db reads", db.loadProjectLog.length)(expectFullDbReads) {
          assertDifference("redis writes", redis.writeCount())(expectCacheWrites) {

            val actual = sendMsg(WsReqRes.InitApp.AndReq(()), p1.static)
            val expect = \/-(p1.initAppData)
            assertEq(actual, \/-((expect, None)))

            val cache = redis.read(p1.id).value
            assertEq(cache.ord, Some(p1.latestOrd))
          }
        }

      'cacheEmpty - test(1, 1)

      'cacheValid - {
        'snapshot - {
          redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instance, p1.latestOrd), z).value
          test(0, 0)
        }
        'events - {
          redis.writeEvents(p1.id, p1.verifiedEvents, z).value
          test(0, 0)
        }
        'both - {
          redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceB, p1.latestOrdB), z).value
          redis.writeEvents(p1.id, p1.verifiedEventsC, z).value
          test(0, 0)
        }
      }

      'cacheStale - {
        'snapshot - {
          redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceA, p1.latestOrdA), z).value
          test(1, 1)
        }
        'events - {
          redis.writeEvents(p1.id, p1.verifiedEventsA, z).value
          test(1, 1)
        }
        'both - {
          redis.writeSnapshot(p1.id, ProjectSnapshot(p1.instanceA, p1.latestOrdA), z).value
          redis.writeEvents(p1.id, p1.verifiedEventsB, z).value
          test(1, 1)
        }
        'eventTail - {
          redis.writeEvents(p1.id, p1.verifiedEventsC, z).value
          test(1, 1)
        }
      }
    } // initApp

  }
}
