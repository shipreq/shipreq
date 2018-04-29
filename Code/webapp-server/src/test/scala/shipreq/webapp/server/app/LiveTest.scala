package shipreq.webapp.server.app

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.{AssetManifest, Urls, WebappConfig}
import shipreq.webapp.base.WebappConfig.liftPath1
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.logic.Obfuscators
import shipreq.webapp.server.test.LiveTestUtils._
import shipreq.webapp.server.test._

object LiveTest extends TestSuite {

  import userFixture.{user1, user2}

  var pid = Option.empty[ProjectId]

  val prepare = onceUnit {
    LiveTestUtils.init()
    userFixture.setup.unsafeRun()
    pid = Some(xa ! dbAlgebra.createEmptyProject(user1.id))
  }

  override def tests = TestSuite {
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
      get(Urls.logout.relativeUrl)
        .assertRedirectTo("/")
      ()
    }

    'membersHome {
      get(Urls.memberHome.relativeUrl, headers = retainSession(login(user1)))
        .assertSpa(AssetManifest.webappClientHomeJs, HomeSpaProtocols.EntryPoint)
        .assertBodyTitle(WebappConfig.makePageTitle())
      ()
    }

    'projectSpa {
      val p = Obfuscators.projectId.obfuscate(pid.get)
      get(Urls.project(p).relativeUrl, headers = retainSession(login(user1)))
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
