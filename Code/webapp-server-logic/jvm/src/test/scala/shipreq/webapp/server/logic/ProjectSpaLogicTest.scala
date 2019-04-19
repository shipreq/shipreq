package shipreq.webapp.server.logic

import scalaz.{-\/, \/, \/-}
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.server.logic.ProjectSpaLogic._
import shipreq.webapp.server.logic.Security.SessionToken

object ProjectSpaLogicTest extends TestSuite {

  class Tester extends MockInterpreters {

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
    }

  }

  override def tests = Tests {

    'connect {
      import ConnectRejection._
      val t = new Tester; import t._

      def test(c: Cookie.LookupFn, p: ProjectId.Public)(expect: ConnectRejection \/ (WebSocketStatic, WebSocketState)): Unit =
        assertProtected {
          val a = t.projectSpa.onConnect(c, p).value
          assertEq(a, expect)
        }

      'noSession        - test(None, project1.id)(-\/(NoSession))
      'anonymousSession - test(SessionToken.anonymous, project1.id)(-\/(AnonymousSession))
      'invalidProjectId - test(user2.token, Obfuscated("!"))(-\/(InvalidProjectId))
      'projectNotFound  - test(user2.token, ProjectId(23432))(-\/(ProjectNotFound))
      'accessDenied     - test(user3.token, project1.id)(-\/(AccessDenied))
      'ok               - test(user2.token, project1.id)(\/-((WebSocketStatic(user2.toUser, project1.id), WebSocketState.empty)))
    }
  }
}
