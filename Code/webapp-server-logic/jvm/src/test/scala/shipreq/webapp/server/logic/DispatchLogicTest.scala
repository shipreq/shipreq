package shipreq.webapp.server.logic

import upickle.Js
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
    implicit val trace = TraceLogic.off[Name, Request[Unit], Response]
    val dispatcher = new DispatchLogic[Name, Request[Unit], Response](
      r => Request(r.method, r.path, r.param, r), (_, r) => Name(r))
    val dispatch = dispatcher.mainDispatcher(false, false)
    db.users ::= user2
    db.users ::= user3
    val pid = ProjectId(9)
    db.addProject(pid, user2.id)()
  }
  import Tester._

  override def utestBeforeEach(path: Seq[String]): Unit = {
    taskman.reset()
  }

  implicit def autoRelUrl(s: String): Url.Relative = Url.Relative(s)

  def run(url   : Url.Relative,
          method: Method              = Get,
          params: Map[String, String] = Map.empty)
         (implicit logIn: MockDb.UserEntry = null): Response = {
    security.loggedIn = Option(logIn)
    val req = Request(method, url, params.get, ())
    val d = if (dispatcher.Ops.candidate(url))
      dispatcher.Ops.total
    else
      dispatch
    d(req).value
  }

  def testRun(expect: Response,
              url   : Url.Relative,
              method: Method              = Get,
              params: Map[String, String] = Map.empty)
             (implicit logIn: MockDb.UserEntry = null): Unit =
    assertEq(
      s"${method.toString.toUpperCase} ${url.relativeUrl} ${params.mkString(", ")}",
      run(url, method, params)(logIn),
      expect)

  def testNonGet(url: Url.Relative): Unit =
    for {
      method <- List[Method](Post, Other)
      logIn <- List[MockDb.UserEntry](null, user2)
    } {
        testRun(Response.MethodNotAllowed, url, method)(logIn)
        assertEq("405 shouldn't log user out", security.loggedIn, Option(logIn))
      }

  def testNeedAuth(url: Url.Relative): Unit = {
    val expect = if (url ==* Urls.memberHome) "/login" else s"/login/${url.relativeUrlNoHeadSlash}"
    testRun(Response.Redirect(expect), url)
  }

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

  implicit def autoXID(p: ProjectId): ProjectId.Public =
    Obfuscators.projectId.obfuscate(p)

  override def tests = Tests {

    'publicSpa {
      import Urls.PublicSpaRoute._

      'nullary - static.foreach(p => assertUnprotected(testRun(Response.ServePublicSpa, p.url)))

      'nonGet - static.foreach(p => testNonGet(p.url))

      'loggedIn {
        implicit def login = user2
        'loginRedirects - assertUnprotected(testRun(Response.redirectToMemberHome, Login.url))
        'nonLoginRenders - static.whole.filter(_ !=* Login).foreach(p => assertUnprotected(testRun(Response.ServePublicSpa, p.url)))
      }

      'loginToMember - List(Urls.memberHome, Urls.project(ProjectId(1))).foreach(u =>
        testRun(Response.ServePublicSpa, s"/login/${u.relativeUrlNoHeadSlash}"))

      'resetPassword2 {
        svr.run(PublicSpaLogic[Name, Name].initData.value.resetPassword1)(\/-(user2.emailAddr))

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

    'ops {
      'ok - testRun(Response.Text(200, "OK."), "/ops/ok")

      def register1Url = opsRoot / "register1"
      def register1Params = Map(opsSecretKey -> opsSecretValue.value, "email" -> "a@bc.com")

      'register1 - assertProtected(testRun(
        Response.Json(200, Js.Obj("taskId" -> Js.Num(1))),
        register1Url, Post, register1Params))

      // For security reasons, the same response is observed for all failures.
      // This prevents hackers determining:
      // - valid URLs
      // - the secret key param
      // - the secret key value
      def testKO(url: Url.Relative, method: Method, params: Map[String, String] = Map.empty) =
        assertProtected(testRun(Response.Text(404, "Not found."), url, method, params))

      'root      - testKO("/ops", Get)
      'notFound  - testKO("/ops/what", Get)
      'notPost   - testKO(register1Url, Get, register1Params)
      'noSecret  - testKO(register1Url, Post, register1Params - opsSecretKey)
      'badSecret - testKO(register1Url, Post, register1Params + (opsSecretKey -> "admin"))
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
