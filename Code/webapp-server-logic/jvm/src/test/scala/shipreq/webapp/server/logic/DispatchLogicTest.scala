package shipreq.webapp.server.logic

import scalaz.{Name, \/-}
import utest._
import shipreq.base.util.{Invalid, Url}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.Urls
import DispatchLogic._
import Method._
import shipreq.webapp.base.user.EmailAddr

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

  val fallbackResponse = Response.Redirect(Urls.publicHome)

  override def tests = TestSuite {

    'publicSpa {
      import Urls.PublicSpaRoute._

      'nullary - static.foreach(p => assertUnprotected(testRun(Response.ServePublicSpa, p.url)))

      'nonGet - static.foreach(p => testNonGet(p.url))

      'loggedIn {
        implicit def login = user2
        'loginRedirects - assertUnprotected(testRun(Response.redirectToMemberHome, Login.url))
        'nonLoginRenders - static.whole.filter(_ !=* Login).foreach(p => assertUnprotected(testRun(Response.ServePublicSpa, p.url)))
      }

      'resetPassword2 {
        svr.run(PublicSpaLogic[Name, Name].initData.value.resetPassword1)(\/-(user2.emailAddr)).needRight

        'invalid - assertProtected(testRun(Response.redirectToPublicHome, ResetPassword.url(SecurityToken("wwwweeeeeeeeeee33333"))))
        'valid   - assertProtected(testRun(Response.ServePublicSpa, ResetPassword.url(db.prevToken())))
        'expired {
          forwardTimeToEndOfPasswordResetWindow(Invalid)
          assertProtected(testRun(Response.redirectToPublicHome, ResetPassword.url(db.prevToken())))
        }
      }

      'register2 {
        svr.run(PublicSpaLogic[Name, Name].initData.value.register1)(EmailAddr("x@x.io")).needRight

        'invalid - assertProtected(testRun(Response.redirectToPublicHome, Register2.url(SecurityToken("wwwweeeeeeeeeee66666"))))
        'valid   - assertProtected(testRun(Response.ServePublicSpa, Register2.url(db.prevToken())))
        'expired {
          forwardTimeToEndOfConfirmationWindow(Invalid)
          assertProtected(testRun(Response.redirectToPublicHome, Register2.url(db.prevToken())))
        }
      }

    }

    'memberHomeSpa {
      def urls = spaUrls(Urls.memberHome)
      'auth   - urls.foreach(testRun(Response.ServeHomeSpa(user2.toUser), _)(user2))
      'anon   - urls.foreach(testNeedAuth)
      'nonGet - urls.foreach(testNonGet)
    }

    'projectSpa {
      implicit def autoXID(p: ProjectId): ProjectId.Public = Obfuscators.projectId.obfuscate(p)
      'projectExists {
        def urls = spaUrls(Urls.project(pid))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(Response.ProjectSpa.Serve(user2.toUser, pid), _)(user2))
        'notOwner - urls.foreach(testRun(Response.ProjectSpa.NotOwner, _)(user3))
        'nonGet   - urls.foreach(testNonGet)
      }
      'noProject {
        def urls = spaUrls(Urls.project(ProjectId(1324675)))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
      'invalidXId {
        def urls = List("@_@", "@").flatMap(x => spaUrls(s"${Urls.project.prefix.relativeUrl}/$x"))
        'anon     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _))
        'auth     - urls.foreach(testRun(Response.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
    }

    'logout {
      def test(logIn: MockDb.UserEntry = null): Unit = {
        testRun(Response.redirectToPublicHome, Urls.logout)(logIn)
        assert(security.loggedIn.isEmpty)
      }
      'anon   - test()
      'auth   - test(user2)
      'nonGet - testNonGet(Urls.logout)
    }

    'fallback {
      val unrecognisedPaths = List[Url.Relative](
        "/fart",
        "/logi",
        "/loginn",
        "/hom",
        "/homeX",
        "/projectX",
        Urls.memberHome.relativeUrlNoHeadOrTailSlash + "x")
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
