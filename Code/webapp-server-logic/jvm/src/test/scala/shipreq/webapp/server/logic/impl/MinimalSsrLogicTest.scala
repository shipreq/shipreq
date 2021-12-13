package shipreq.webapp.server.logic.impl

import shipreq.base.ops.Trace
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FxModule._
import shipreq.base.util.{Allow, Url}
import shipreq.webapp.base.config.{AssetManifest, Urls}
import shipreq.webapp.base.data.Username
import shipreq.webapp.server.logic.test.MockServer
import shipreq.webapp.ssr._
import utest._

object MinimalSsrLogicTest extends TestSuite {
  import SsrAlgebra._
  import SsrSharedData._

  private def getHtml(f: Fx[Output]): String = {
    val result = f.unsafeRun()
    assert(result.isDefined)
    result.get.value
  }

  private implicit def autoUsernameFromString(u: String): Option[Username] =
    Option(u).map(Username(_))

  private def baseUrl = Url.Absolute.Base("https://shipreq.com")

  private implicit val am = AssetManifest(Some(AssetManifest.StaticAssetCdn("https://static.shipreq.com")))

  private lazy val ssr = {
    implicit val trace = Trace.Algebra.off[Fx]
    implicit val svr = new MockServer[Fx]
    new MinimalSsrLogic[Fx]().prepare(baseUrl, Allow).unsafeRun()
  }

  override def tests = Tests {

    "prepare" - {ssr; ()} // so it gets it's own duration in test output

    "public" - {

      "root" - {
        def run(username: String = null): String =
          getHtml(ssr.public(Url.Relative("/"), username))

        "anon" - {
          val html = run()
          assertContains(html, ">Login<")
          assertContains(html, "Get in touch")
          ()
        }

        "loggedIn" - {
          val html = run("gori")
          assertNotContains(html, ">Login<")
          assertContains(html, "@gori")
          assertContains(html, "Get in touch")
          ()
        }
      }

      "login" - {
        def test(username: String = null): Unit = {
          val result = ssr.public(Urls.login, username).unsafeRun()
          assertEq(result, None)
        }
        "anon"     - test()
        "loggedIn" - test("gori")
      }
    }

    "home" - {
      "loader" - {
        val html = getHtml(ssr.homeSpaLoader(HomeSpaLoaderData(Username("gori"), am)))
        assertNotContains(html, "Loading...")
        assertContains(html, "Projects")
        assertContains(html, "@gori")
        ()
      }
    }

    "project" - {
      "loader" - {
        val html = getHtml(ssr.projectSpaLoader(ProjectSpaLoaderData(Username("gori"), "Stuff", am)))
        assertContains(html, "Loading...")
        assertContains(html, "Projects")
        assertContains(html, "Stuff")
        assertContains(html, "@gori")
        ()
      }
    }

  }
}
