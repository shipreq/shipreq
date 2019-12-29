package shipreq.webapp.server.app

import scalaz.-\/
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.{AssetManifest, Urls, WebappConfig}
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.user.{EmailAddr, PersonName}
import shipreq.webapp.client.public.PublicSpaProtocols.LandingPage.Request
import shipreq.webapp.client.public.{PublicSpaEntryPoint, PublicSpaProtocols}
import shipreq.webapp.server.logic.{Obfuscators, Security}
import shipreq.webapp.server.test.LiveTestUtils._
import shipreq.webapp.server.test._

object LiveTest extends TestSuite {

  import userFixture.{user1, user2}

  var pid = Option.empty[ProjectId]

  val prepare = onceUnit {
    LiveTestUtils.init()
    userFixture.setup.unsafeRun()
    pid = Some(xa ! dbAlgebra.createProject(user1.id, Vector.empty, Project.empty))
  }

  implicit def temp[I](c: shipreq.webapp.base.protocol.ClientSideProc[I]): ClientSideProc[I] =
    ClientSideProc[I](c.objectName)(c.pickler)

  implicit def userToToken(u: UserFixture.TestUser): Option[Security.SessionToken[Unit]] =
    Some(Security.SessionToken.anonymous().login(u.toUserDescriptor).withoutExpiry)

  override def tests = Tests {
    prepare()

    'root {
      get("/")
        .assertSpa(AssetManifest.webappClientPublicJs, PublicSpaEntryPoint.proc)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'loginAjax {
      val st = Security.SessionToken.anonymous()
      ajaxPost(CommonProtocols.Login.ajax)(CommonProtocols.Login.Request(-\/(user1.username), user1.password), st)
        .assertOk
        .assertContentType("application/octet-stream")
        .assertJwt(Some(user1.toToken().withSession(st)))
      ()
    }

    'logout {
      val st = user1.toToken()
      get(Urls.logout.relativeUrl, Some(st))
        .assertRedirectTo("/")
        .assertJwt(Some(st.logout))
      ()
    }

    'webappClientPublicJs {
      get(AssetManifest.webappClientPublicJs)
        .assertOk
        .assertContentTypeJs
        .assertBodyContains("function")
        .assertBodyContains("public")
      ()
    }

    'favicon {
      get(AssetManifest.favicon)
        .assertOk
        .assertContentType("image/x-icon")
      ()
    }

    'membersHome {
      get(Urls.memberHome.relativeUrl, user1)
        .assertSpa(AssetManifest.webappClientHomeJs, HomeSpaEntryPoint.proc)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'projectSpa {
      val p = Obfuscators.projectId.obfuscate(pid.get)
      get(Urls.project(p).relativeUrl, user1)
        .assertSpa(AssetManifest.webappClientProjectJs, ProjectSpaEntryPoint.proc)
      ()
    }

    // ensure we don't block these (and other Lift stuff we don't know about)
    'contentSecurityPolicyReport {
      get(s"/${WebappConfig.liftCtxPath}/content-security-policy-report")
        .assertBodyContains("content security policy report")
      ()
    }

    // Lift parses x.y.z as having no extension
    'sourceMaps {
      get("/blah.js.map").assertStatus(404)
      ()
    }

    'opsOk {
      get("/ops/ok").assertOk.assertStatelessLift
      ()
    }

    'metrics {
      def test(token: String = null) =
        get("/ops/metrics", headers = Option(token).map(t => "Authorization" -> s"Bearer $t").toList).assertStatelessLift

      'noAuth - test().assertStatus(404)
      'badAuth - test("xxx").assertStatus(401)
      'goodAuth - test("metric_test_secret").assertStatus(200)
    }

    'templateAccess {
      val templates = List(
        "admin-stats.html",
        "404.html",
        "public.html",
        "members-home.html",
        "templates-hidden/blank.html")
      for (t <- templates)
        get(s"/$t").assertStatus(404)
    }

    'jwtLifespanExtension {
      val Some(s1) = get("/").newJwt()
      assert(s1.authenticatedUser.isEmpty)

      assertEq(get("/", Some(s1)).newJwt(), None)

      val Some(s2) = ajaxPost(CommonProtocols.Login.ajax)(CommonProtocols.Login.Request(-\/(user1.username), user1.password), s1).newJwt()
      assertEq(s2.withoutExpiry, s1.withoutExpiry.login(user1.toUserDescriptor))

      // GETs shouldn't increase session time
      assertEq(get(Urls.memberHome.relativeUrl, Some(s2)).newJwt(), None)
      assertEq(get(AssetManifest.favicon, Some(s2)).newJwt(), None)
      assertEq(get(Urls.project(Obfuscators.projectId.obfuscate(pid.get)).relativeUrl, Some(s2)).newJwt(), None)

      // Non-login AJAX shouldn't increase session time
      val lpReq = Request(PersonName("Mike"), EmailAddr("qwe@jasdkf.com"), Some("yo"), true)
      assertEq(ajaxPost(PublicSpaProtocols.LandingPage.ajax)(lpReq, s2).newJwt(), None)

      val Some(s3) = ajaxPost(CommonProtocols.Login.ajax)(CommonProtocols.Login.Request(-\/(user1.username), user1.password), s2).newJwt()
      assertEq(s3.withoutExpiry, s2.withoutExpiry)

      val Some(s4) = get(Urls.logout.relativeUrl, Some(s3)).newJwt()
      assertEq(s4.withoutExpiry, s3.withoutExpiry.logout)

      ()
    }

    'teardown {
      xa ! DbTable.Event.truncate
      xa ! DbTable.Project.truncate
      userFixture.teardown.unsafeRun()
    }
  }
}
