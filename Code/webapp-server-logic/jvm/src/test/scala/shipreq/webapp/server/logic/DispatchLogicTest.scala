package shipreq.webapp.server.logic

import scalaz.{-\/, Name, Need, \/-}
import upickle.Js
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{BinaryData, Invalid, Url}
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.Protocol
import shipreq.webapp.base.user.{EmailAddr, PersonName}
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.logic.DispatchLogic.Method._
import shipreq.webapp.server.logic.DispatchLogic._

object DispatchLogicTest extends TestSuite {

  final case class TestRequest(method : Method,
                               path   : Url.Relative,
                               body   : Option[BinaryData],
                               params : Map[String, String],
                               cookies: Map[Cookie.Name, String]) {

    def toAbstract: Request[TestRequest] =
      Request(method, path, Need(body), params.get, cookies.get, this)
  }

  def patchCookies(m: Map[Cookie.Name, String], u: Cookie.Update): Map[Cookie.Name, String] =
    m -- u.remove ++ u.add.map(c => c.name -> c.value)

  final case class TestResponse(cmd         : ResponseCmd,
                                cookieUpdate: Cookie.Update,
                                cookies     : Map[Cookie.Name, String]) {
    def authUser(implicit s: Security.Algebra[Name]) =
      s.sessionRestore(cookies.get).value.flatMap(_.authenticatedUser)
  }

  object Tester extends MockInterpreters {
    implicit val traceLogic = TraceLogic.off[Name, TestRequest, TestResponse]

    val dispatcher = new DispatchLogic[Name, TestRequest, TestResponse](
      _.toAbstract,
      (req, res) => Name(TestResponse(res.cmd, res.cookies, patchCookies(req.cookies, res.cookies))))

    val routeDispatcher = dispatcher.all(testMode = false)

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

  def run(url    : Url.Relative,
          method : Method                   = Get,
          body   : Option[BinaryData]       = None,
          params : Map[String, String]      = Map.empty,
          cookies: Map[Cookie.Name, String] = Map.empty)
         (implicit token: Security.SessionToken = null): (TestRequest, TestResponse) = {
    val cookies2 =
      if (token ne null)
        patchCookies(cookies, security.sessionPersist(token).value)
      else
        cookies
    val req = TestRequest(method, url, body, params, cookies2)
    (req, routeDispatcher(req).value)
  }

  def runAjax(p      : Protocol.Ajax[SafePickler])
             (req    : p.prepReq.Type,
              method : Method                   = Post,
              cookies: Map[Cookie.Name, String] = Map.empty)
             (implicit token: Security.SessionToken = null) = {
    val bin = p.prepReq.codec.encode(req)
    run(p.url, method, Some(bin), Map.empty, cookies)(token)
  }

  def testRun(expect: ResponseCmd,
              url   : Url.Relative,
              method: Method              = Get,
              body   : Option[BinaryData] = None,
              params: Map[String, String] = Map.empty)
             (implicit token: Security.SessionToken = null): TestResponse = {
    val actual = run(url, method, body, params)(token)._2
    assertEq(s"${method.toString.toUpperCase} ${url.relativeUrl} ${params.mkString(", ")}", actual.cmd, expect)
    actual
  }

  def testNonGet(url: Url.Relative): Unit =
    for {
      method <- List[Method](Post, Other)
      user   <- List[MockDb.UserEntry](null, user2)
    } {
        val r = testRun(ResponseCmd.StatusOnly.MethodNotAllowed, url, method)(user)
        assertEq("405 shouldn't log user out", r.authUser, Option(user).map(_.toUser))
      }

  def testNeedAuth(url: Url.Relative): Unit = {
    val expect = if (url ==* Urls.memberHome) "/login" else s"/login/${url.relativeUrlNoHeadSlash}"
    testRun(ResponseCmd.Redirect(expect), url)
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

  val fallbackResponse = ResponseCmd.Redirect(Urls.publicHome)

  implicit def autoXID(p: ProjectId): ProjectId.Public =
    Obfuscators.projectId.obfuscate(p)

  implicit def userToToken(u: MockDb.UserEntry): Security.SessionToken =
    if (u eq null) null else u.token

  override def tests = Tests {

    'publicSpa {
      import Urls.PublicSpaRoute._

      'nullary - static.foreach(p => assertUnprotected(testRun(ResponseCmd.ServePublicSpa(None), p.url)))

      'nonGet - static.foreach(p => testNonGet(p.url))

      'logIn - {
        val u = user2
        val req = PublicSpaProtocols.Login.Request(-\/(u.username), user2password)
        val res = runAjax(PublicSpaProtocols.Login.ajax)(req)(Security.SessionToken.anonymous)._2
        val tok = security.sessionRestore(res.cookies.get).value
        assertEq(tok, Some(user2.token))
      }

      'loggedIn {
        implicit def token = user2.token
        'loginRedirects - assertUnprotected(testRun(ResponseCmd.redirectToMemberHome, Login.url))
        'nonLoginRenders - static.whole.filter(_ !=* Login).foreach(p =>
          assertUnprotected(testRun(ResponseCmd.ServePublicSpa(token.authenticatedUser), p.url)))
      }

      'loginToMember - List(Urls.memberHome, Urls.project(ProjectId(1))).foreach(url =>
        testRun(ResponseCmd.ServePublicSpa(None), s"/login/${url.relativeUrlNoHeadSlash}"))

      'resetPassword2 {
        publicSpa.ajaxResetPassword1(\/-(user2.emailAddr)).value

        'invalid - assertProtected(testRun(ResponseCmd.redirectToPublicHome, ResetPassword.url(SecurityToken("wwwweeeeeeeeeee33333"))))
        'valid   - assertProtected(testRun(ResponseCmd.ServePublicSpa(None), ResetPassword.url(db.prevToken())))
        'expired {
          forwardTimeToEndOfPasswordResetWindow(Invalid)
          assertProtected(testRun(ResponseCmd.redirectToPublicHome, ResetPassword.url(db.prevToken())))
        }
      }

      'register2 {
        publicSpa.ajaxRegister1(EmailAddr("x@x.io")).value.needRight

        'invalid - assertProtected(testRun(ResponseCmd.redirectToPublicHome, Register2.url(SecurityToken("wwwweeeeeeeeeee66666"))))
        'valid   - assertProtected(testRun(ResponseCmd.ServePublicSpa(None), Register2.url(db.prevToken())))
        'expired {
          forwardTimeToEndOfConfirmationWindow(Invalid)
          assertProtected(testRun(ResponseCmd.redirectToPublicHome, Register2.url(db.prevToken())))
        }
      }
    }

    'memberHomeSpa {
      def urls = spaUrls(Urls.memberHome)
      'auth   - urls.foreach(testRun(ResponseCmd.ServeHomeSpa(user2.toUser), _)(user2))
      'anon   - urls.foreach(testNeedAuth)
      'nonGet - urls.foreach(testNonGet)
    }

    'projectSpa {
      'projectExists {
        def urls = spaUrls(Urls.project(pid))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(ResponseCmd.ProjectSpa.Serve(user2.toUser, pid), _)(user2))
        'notOwner - urls.foreach(testRun(ResponseCmd.ProjectSpa.NotOwner, _)(user3))
        'nonGet   - urls.foreach(testNonGet)
      }
      'noProject {
        def urls = spaUrls(Urls.project(ProjectId(1324675)))
        'anon     - urls.foreach(testNeedAuth)
        'auth     - urls.foreach(testRun(ResponseCmd.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
      'invalidXId {
        def urls = List("@_@", "@").flatMap(x => spaUrls(s"${Urls.project.prefix.relativeUrl}/$x"))
        'anon     - urls.foreach(testRun(ResponseCmd.ProjectSpa.InvalidId, _))
        'auth     - urls.foreach(testRun(ResponseCmd.ProjectSpa.InvalidId, _)(user2))
        'nonGet   - urls.foreach(testNonGet)
      }
    }

    'logout {
      def test(logIn: MockDb.UserEntry = null): Unit = {
        val r = testRun(ResponseCmd.redirectToPublicHome, Urls.logout)(logIn)
        assert(r.authUser.isEmpty)
      }
      'anon   - test()
      'auth   - test(user2)
      'nonGet - testNonGet(Urls.logout)
    }

    'ajax {
      def test(token: Option[Security.SessionToken])
              (expectFailure: Option[Int],
               expectNewToken: Option[Security.SessionToken]) = {

        val lpReq = PublicSpaProtocols.LandingPage.Request(
          PersonName("Mike"),
          EmailAddr("qwe@jasdkf.com"),
          Some("yo"),
          true)

        val (req, res) = runAjax(PublicSpaProtocols.LandingPage.ajax)(lpReq)(token.orNull)

        expectFailure match {
          case Some(c) => assertEq(res.cmd, ResponseCmd.StatusOnly(c))
          case None => res.cmd match {
            case ResponseCmd.Binary(200, _) => ()
            case x => fail("Expected ResponseCmd.Binary(200, _), got " + x)
          }
        }

        assertEq(res.cookieUpdate.remove, Nil)
        expectNewToken match {
          case None =>
            assertEq(res.cookieUpdate.add, Nil)
          case Some(t) =>
            assertEq(res.cookieUpdate.add.map(_.name), security.cookieName :: Nil)
            val v1 = req.cookies(security.cookieName)
            val v2 = res.cookies(security.cookieName)
            assert(v1 != v2) // JWT expected to change (new expiry)
            assertEq(security.sessionRestore(res.cookies.get).value, Some(t))
        }
      }

      'requireJwt - test(None)(Some(403), None)
      'anon - test(Some(Security.SessionToken.anonymous))(None, Some(Security.SessionToken.anonymous))
      'auth - test(Some(user2.token))(None, Some(user2.token))
    }

    'ops {
      'ok - testRun(ResponseCmd.Text(200, "OK."), "/ops/ok")

      def register1Url = opsRoot / "register1"
      def register1Params = Map(opsSecretKey -> opsSecretValue.value, "email" -> "a@bc.com")

      'register1 - assertProtected(testRun(
        ResponseCmd.Json(200, Js.Obj("taskId" -> Js.Num(1))),
        register1Url, Post, None, register1Params))

      // For security reasons, the same response is observed for all failures.
      // This prevents hackers determining:
      // - valid URLs
      // - the secret key param
      // - the secret key value
      def testKO(url: Url.Relative, method: Method, params: Map[String, String] = Map.empty) =
        assertProtected(testRun(ResponseCmd.StatusOnly(404), url, method, None, params))

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
          val r = testRun(fallbackResponse, u)(logIn)
          assertEq("404 shouldn't log user out", r.authUser, Option(logIn).map(_.toUser))
        }
      }
      'anon   - test()
      'auth   - test(user2)
      'nonGet - unrecognisedPaths.foreach(testNonGet)
    }

  }
}
