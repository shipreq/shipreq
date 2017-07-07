package shipreq.webapp.server.logic

import scalaz.Name
import utest._
import shipreq.base.util.Url
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.{MemberUrls, PublicUrls}
import DispatchLogic._
import Method._

object DispatchLogicTest extends TestSuite {

  object Tester extends MockInterpreters {
    val dispatcher = new DispatchLogic[Name]
    val dispatch = dispatcher.main.withFallback(dispatcher.fallback)
    db.users ::= user2
    db.users ::= user3
    val pid = ProjectId(9)
    db.addProject(pid, user2.id)()
  }
  import Tester._

  implicit def autoRelUrl(s: String): Url.Relative = Url.Relative(s)

  def run(url: Url.Relative, method: Method = Get)
         (implicit logIn: MockDb.UserEntry = null): Response = {
    security.loggedIn = Option(logIn)
    dispatch(Request(method, url, _ => None)).value
  }

  def testRun(expect: Response, u: Url.Relative, method: Method = Get)
             (implicit logIn: MockDb.UserEntry = null): Unit =
    assertEq(u.relativeUrl, run(u, method)(logIn), expect)

  def testNonGet(url: Url.Relative): Unit =
    for {
      method <- List[Method](Post, Other)
      logIn <- List[MockDb.UserEntry](null, user2)
    } {
        testRun(Response.MethodNotAllowed, url, method)(logIn)
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
      import PublicUrls.SpaRoute._
      'pages0 - static.foreach(p => testRun(Response.ServePublicSpa, p.url))
      'pages1 - needsToken.foreach(p => testRun(Response.ServePublicSpa, p.url(SecurityToken("x"))))
      'nonGet - static.foreach(p => testNonGet(p.url))
    }

    'memberHomeSpa {
      def urls = spaUrls(MemberUrls.home)
      'auth   - urls.foreach(testRun(Response.ServeHomeSpa(user2.toUser), _)(user2))
      'anon   - urls.foreach(testNeedAuth)
      'nonGet - urls.foreach(testNonGet)
    }

    'projectSpa {
      implicit def autoXID(p: ProjectId) = ProjectId.Extern(p)
      'projectExists {
        def urls = spaUrls(MemberUrls.project(pid))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(Response.ProjectSpa.Serve(user2.toUser, pid), _)(user2))
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
