package shipreq.webapp.server.app

import utest._
import shipreq.webapp.base.{AssetManifest, MemberUrls, WebappConfig}
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.server.test.LiveTestUtils._
import shipreq.webapp.server.test._

object LiveTest extends TestSuite {

  import userFixture.{user1, user2}

  val prepare = once {
    LiveTestUtils.init()
    userFixture.setup.unsafePerformIO()
  }

  override def tests = TestSuite {
    prepare()

    'root {
      get("/")
        .assertOk
        .assertContentTypeHtml
        .assertBodyContains(AssetManifest.webappClientPublicJs)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'liftAjax {
      val root = get("/")
      val ajaxUrl = s"/${WebappConfig.liftPath}/[a-zA-Z0-9_/]+\\.js".r.findFirstIn(root.bodyString) getOrElse fail(s"Lift Ajax not found in: ${root.bodyString}")
      get(ajaxUrl, headers = retainSession(root))
        .assertOk
        .assertContentTypeJs
        .assertBodyContains("lift_settings")
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

    'logout - {
      get(MemberUrls.logout.relativeUrl)
        .assertRedirectTo("/")
      ()
    }

    'membersHome {
      get(MemberUrls.home.relativeUrl, headers = retainSession(login(user1)))
        .assertOk
        .assertContentTypeHtml
        .assertBodyContains(AssetManifest.webappClientHomeJs)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    // ensure we don't block these
    'contentSecurityPolicyReport {
      get(s"/${WebappConfig.liftPath}/content-security-policy-report")
        .assertBodyContains("content security policy report")
      ()
    }

    'teardown - userFixture.teardown.unsafePerformIO()
  }
}
