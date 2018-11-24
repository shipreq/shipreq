package shipreq.webapp.ssr

import shipreq.base.util.FxModule._
import scala.concurrent.duration._
import utest._
import shipreq.base.util.Url
import shipreq.webapp.base.Urls
import shipreq.webapp.base.user.Username
import shipreq.webapp.ssr.SsrAlgebra.Types._

object SsrInterpreterTest extends TestSuite {

  def cfg = SsrInterpreter.Config(
    enabled = true,
    poolSize = 1,
    timeoutPublic = 10 days,
    timeoutProjectSpaLoader = 10 days)

  lazy val ssr = SsrInterpreter(cfg, false)

  private def assertContent(f: Fx[Option[Html]]): String = {
    val result = f.unsafeRun()
    assert(result.isDefined)
    result.get.value
  }

  override def tests = Tests {

    'public - {

      def run(path: String, username: String = null): String = {
        val url = Url.Absolute("https://shipreq.com" + path)
        val data = SsrInterpreter.samplePublicInitData.copy(loggedInUser = Option(username).map(Username.apply))
        assertContent(ssr.public(url, data))
      }

      'anon - {
        'index - {
          val html = run("")
          assert(html contains ">Login<")
          assert(html contains "Get in touch")
          ()
        }

        'register1 - {
          val html = run(Urls.PublicSpaRoute.Register1.url.relativeUrlNoTailSlash)
          assert(html contains ">Login<")
          ()
        }
      }

      'loggedIn - {
        'index - {
          val html = run("", "gori")
          assert(!html.contains(">Login<"))
          assert(html contains "@gori")
          assert(html contains "Get in touch")
          ()
        }
      }
    }

    'project - {
      'loader - {
        val html = assertContent(ssr.projectSpaLoader(ProjectSpaLoaderData(Username("gori"), "Stuff")))
        assert(html contains "Loading...")
        assert(html contains "Stuff")
        assert(html contains "@gori")
        ()
      }
    }

  }
}
