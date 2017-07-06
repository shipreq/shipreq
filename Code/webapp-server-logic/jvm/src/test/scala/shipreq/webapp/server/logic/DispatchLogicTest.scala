package shipreq.webapp.server.logic

import scalaz.Name
import utest._
import shipreq.base.util.Url
import shipreq.base.test.BaseTestUtil._
import DispatchLogic._
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.{MemberUrls, PublicUrls}

object DispatchLogicTest extends TestSuite {

  object Tester extends MockInterpreters {
    val dispatcher = new DispatchLogic[Name]
    db.users ::= user2
    db.users ::= user3
    val pid = ProjectId(9)
    db.addProject(pid, user2.id)()
  }
  import Tester._

  implicit def autoRelUrl(s: String): Url.Relative = Url.Relative(s)

  def run(url: Url.Relative, get: Boolean = true)
         (implicit logIn: MockDb.UserEntry = null): Response = {
    security.loggedIn = Option(logIn)
    dispatcher.all(Request(get, url)).value
  }

  def testRun(expect: Response, u: Url.Relative, get: Boolean = true)
             (implicit logIn: MockDb.UserEntry = null): Unit =
    assertEq(u.relativeUrl, run(u, get)(logIn), expect)

  def testNonGet(url: Url.Relative): Unit =
    for (logIn <- List[MockDb.UserEntry](null, user2)) {
      testRun(Response.MethodNotAllowed, url, get = false)(logIn)
      assertEq("405 shouldn't log user out", security.loggedIn, Option(logIn))
    }

  def testNeedAuth(url: Url.Relative): Unit =
    testRun(Response.redirectToLogin, url)

  val spaSuffixes: List[String] =
    for {
      a <- List("", "/", "/a")
      b <- List("", "#", "#b")
      c <- List("", "/", "/cc")
      d <- List("", "#", "#dd")
    } yield a + b + c + d

  def spaUrls(spaUrl: Url.Relative): List[Url.Relative] =
    spaSuffixes.map(s => Url.Relative(spaUrl.relativeUrlNoHeadOrTailSlash + s))

  val fallbackResponse = Response.Redirect(PublicUrls.home)

  override def tests = TestSuite {

    'publicSpa {
      import PublicUrls.PublicSpaRoute._
      'pages0 - static.foreach(p => testRun(Response.ServePublicSpa, p.url))
      'pages1 - needsToken.foreach(p => testRun(Response.ServePublicSpa, p.url(SecurityToken("x"))))
      'nonGet - static.foreach(p => testNonGet(p.url))
    }

    'memberHomeSpa {
      def urls = spaUrls(MemberUrls.home)
      'auth   - urls.foreach(testRun(Response.ServeHomeSpa, _)(user2))
      'anon   - urls.foreach(testNeedAuth)
      'nonGet - urls.foreach(testNonGet)
    }

    'projectSpa {
      implicit def autoXID(p: ProjectId) = ProjectId.Extern(p)
      'projectExists {
        def urls = spaUrls(MemberUrls.project(pid))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(Response.ProjectSpa.Serve, _)(user2))
        'notOwner - urls.foreach(testRun(Response.ProjectSpa.NotOwner, _)(user3))
        'nonGet   - urls.foreach(testNonGet)
      }
      'noProject {
        def urls = spaUrls(MemberUrls.project(ProjectId(1324675)))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
      'invalidXId {
        def urls = List("@_@", "@").flatMap(x => spaUrls(s"${MemberUrls.project.prefix.relativeUrl}/$x"))
        'anon     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _))
        'auth     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
    }

    'logout {
      def test(logIn: MockDb.UserEntry = null): Unit = {
        testRun(Response.redirectToPublicHome, MemberUrls.logout)(logIn)
        assert(security.loggedIn.isEmpty)
      }
      'anon   - test()
      'auth   - test(user2)
      'nonGet - testNonGet(MemberUrls.logout)
    }

    'fallback {
      val unrecognisedPaths = List[Url.Relative](
        "/fart",
        "/logi",
        "/loginn",
        "/hom",
        "/homeX",
        "/projectX",
        MemberUrls.home.relativeUrlNoHeadOrTailSlash + "x")
      def test(logIn: MockDb.UserEntry = null): Unit = {
        for (u <- unrecognisedPaths) {
          testRun(fallbackResponse, u)(logIn)
          assertEq("404 shouldn't log user out", security.loggedIn, Option(logIn))
        }
      }
      'anon   - test()
      'auth   - test(user2)
      'nonGet - unrecognisedPaths.foreach(testNonGet)
    }

  }
}
