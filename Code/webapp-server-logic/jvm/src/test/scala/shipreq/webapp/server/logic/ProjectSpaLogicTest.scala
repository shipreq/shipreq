package shipreq.webapp.server.logic

import boopickle.Pickler
import japgolly.microlibs.scalaz_ext.ScalazMacros
import scalaz.{-\/, \/, \/-}
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol2.ProjectSpaProtocols.{InitAppData, WsReqRes}
import shipreq.webapp.base.protocol2.WebSocketShared.ReqId
import shipreq.webapp.base.protocol2._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.logic.Security.SessionToken

object ProjectSpaLogicTest extends TestSuite {

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

    object project1 {

      val id = db.createEmptyProject(user2.id).value

      val events = List[Event](
        ProjectTemplateApply(ProjectTemplate.V2),
        ProjectNameSet("hello"),
      )

      val verifiedEvents = verifyEvents(Project.empty)(events: _*)

      for (e <- verifiedEvents)
        db.saveProjectEvent(id)(e.ord, e.event.asInstanceOf[ActiveEvent], e.hashRecs).value.foreach(throw _)

      val metadata = db.getProjectMetaData(id).value.get

      lazy val instance = applyVerifiedEventSuccessfully(Project.empty, verifiedEvents.toList: _*)

      lazy val initAppData = InitAppData(instance, metadata, verifiedEvents.last.ord)

      lazy val static = WebSocketStatic(user2.toUser, id)
    }
  }

  private val t = new Tester; import t._

  private def wsHelper(reqId: ReqId, responseType: WsReqRes) = {
    val p = ProjectSpaProtocols.WebSocket(Obfuscated(null))
    implicit def picklerReq: Pickler[p.Req] = p.req.codec
    implicit def picklerPush: Pickler[p.Push] = p.push.codec
    val responseUnpickler: ReqId => Protocol[Pickler] = i => if (i.value == reqId.value) responseType.protocolRes else ???
    new WebSocketServerHelper[p.Req, p.Push](
      Protocol(WebSocketShared.protocolCS),
      Protocol(WebSocketShared.protocolSC(responseUnpickler)))
  }

  private def sendMsg(msg: WsReqRes.AndReq, static: WebSocketStatic): MsgError \/ (msg.reqRes.ResponseType, Option[WebSocketState]) = {
    val reqId = ReqId(7)
    val h = wsHelper(reqId, msg.reqRes)
    val msgBin = BinaryJvm.encode(h.protocolCS)((reqId, msg))
    val resp = projectSpa.onMessage(static)(WebSocketState.empty, msgBin).value
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

    'connect {
      import ConnectRejection._

      def test(c: Cookie.LookupFn, p: ProjectId.Public)(expect: ConnectRejection \/ (WebSocketStatic, WebSocketState)): Unit =
        assertProtected {
          val a = projectSpa.onConnect(c, p).value
          assertEq(a, expect)
        }

      'noSession        - test(None, project1.id)(-\/(NoSession))
      'anonymousSession - test(SessionToken.anonymous, project1.id)(-\/(AnonymousSession))
      'invalidProjectId - test(user2.token, Obfuscated("!"))(-\/(InvalidProjectId))
      'projectNotFound  - test(user2.token, ProjectId(23432))(-\/(ProjectNotFound))
      'accessDenied     - test(user3.token, project1.id)(-\/(AccessDenied))
      'ok               - test(user2.token, project1.id)(\/-((project1.static, WebSocketState.empty)))
    }

    'initApp - {
      val actual = sendMsg(WsReqRes.InitApp.AndReq(()), project1.static)
      val expect = \/-(project1.initAppData)
      implicit val eqInitAppData = ScalazMacros.deriveEqual[InitAppData]
      assertEq(actual, \/-((expect, None)))
    }
  }
}
