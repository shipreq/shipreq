package shipreq.webapp.server.app

import scalaz.-\/
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.{AssetManifest, Urls, WebappConfig}
import shipreq.webapp.base.WebappConfig.liftPath1
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol2.HomeSpaProtocols
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.logic.{Obfuscators, Security}
import shipreq.webapp.server.test.LiveTestUtils._
import shipreq.webapp.server.test._

object LiveTest extends TestSuite {

  import userFixture.{user1, user2}

  var pid = Option.empty[ProjectId]

  val prepare = onceUnit {
    LiveTestUtils.init()
    userFixture.setup.unsafeRun()
    pid = Some(xa ! dbAlgebra.createEmptyProject(user1.id, 0))
  }

  implicit def temp[I](c: shipreq.webapp.base.protocol2.ClientSideProc[I]): ClientSideProc[I] =
    ClientSideProc[I](c.objectName)(c.pickler)

  implicit def userToToken(u: UserFixture.TestUser): Option[Security.SessionToken] =
    Some(Security.SessionToken(Some(u.toUserDescriptor)))

  override def tests = Tests {
    prepare()

    'root {
      get("/")
        .assertSpa(AssetManifest.webappClientPublicJs, PublicSpaProtocols.EntryPoint)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'liftAjaxGet {
      val root = get("/")
      val ajaxUrl = s"/$liftPath1/[a-zA-Z0-9_/]+\\.js".r.findFirstIn(root.bodyString) getOrElse fail(s"Lift Ajax not found in: ${root.bodyString}")
      get(ajaxUrl, headers = retainSession(root))
        .assertOk
        .assertContentTypeJs
        .assertBodyContains("lift_settings")
      ()
    }

    'liftAjaxPost {
      post(s"/$liftPath1/ajax/F376706514629MSACC4")
        .assertStatus(404) // Lift responds with 404, DispatcherLogic will respond with 405 if it catches it
      ()
    }

    'loginAjax {
      ajaxPost(PublicSpaProtocols.login)(PublicSpaProtocols.Login.Request(-\/(user1.username), user1.password))
        .assertOk
        .assertContentType("application/octet-stream")
        .assertJwt(Some(user1.toToken))
      ()
    }

    'logout {
      get(Urls.logout.relativeUrl, Some(user1.toToken))
        .assertRedirectTo("/")
        .assertJwt(Some(Security.SessionToken.anonymous))
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
        .assertSpa(AssetManifest.webappClientHomeJs, HomeSpaProtocols.EntryPoint)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'projectSpa {
      val p = Obfuscators.projectId.obfuscate(pid.get)
      get(Urls.project(p).relativeUrl, user1)
        .assertSpa(AssetManifest.webappClientProjectJs, ProjectSpaProtocols.EntryPoint)
      ()
    }

    // ensure we don't block these (and other Lift stuff we don't know about)
    'contentSecurityPolicyReport {
      get(s"/$liftPath1/content-security-policy-report")
        .assertBodyContains("content security policy report")
      ()
    }

    // Lift parses x.y.z as having no extension
    'sourceMaps {
      get("/blah.js.map").assertStatus(404)
      ()
    }

    'opsOk {
      get("/ops/ok").assertOk
      ()
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

    'teardown {
      xa ! DbTable.EventHash.truncate
      xa ! DbTable.Event.truncate
      xa ! DbTable.Project.truncate
      userFixture.teardown.unsafeRun()
    }
  }
}
