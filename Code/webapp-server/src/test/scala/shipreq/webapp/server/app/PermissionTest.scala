package shipreq.webapp.server.app

import org.apache.commons.httpclient.{HttpMethodBase, HttpClient}
import org.scalatest.FunSpec
import shipreq.webapp.server.test.LiveTest
import shipreq.webapp.server.test.UserFixture
import AppSiteMap._
import Implicits._
import UserFixture.TestUser

class PermissionTest extends FunSpec with LiveTest {

  lazy val uf = UserFixture(newSession())
  import uf.{user1, user2}

  override def beforeAll() {
    super.beforeAll()
    uf.setup()
  }

  implicit override def responseCapture(fullUrl: String, httpClient: HttpClient, getter: HttpMethodBase) = {
    getter.setFollowRedirects(false)
    super.responseCapture(fullUrl, httpClient, getter)
  }

  def doLogin(user: TestUser) =
    post("/login.api", "user" -> user.username.value, "pass" -> user.password) !@ "Failed to log in"

  def loginShouldBeRequiredFor(url: String) =
    get(url) shouldRedirectTo Login.relativeUrl

  // -------------------------------------------------------------------------------------------------------------------

  lazy val pid = uf.toDbUtil.newProjectId(user1.id)

  describe("/") {
    val member = "client-home.js"
    val anon   = "/login"

    it("anon") {
      val r = get("/") ! 200
      r.responseText should (include(anon) and not include member)
    }

    it("auth") {
      val r = doLogin(user1).get("/") ! 200
      r.responseText should (include(member) and not include anon)
    }
  }

  describe("/project") {
    lazy val url = Project.relativeUrl(pid)

    it("should deny anon") {
      loginShouldBeRequiredFor(url)
    }

    it("should allow owner") {
      val r = doLogin(user1).get(url) ! 200
      r.responseText should include(""" id="tgt"""")
    }

    it("should deny non-owner") {
      doLogin(user2).get(url) shouldRedirect
    }
  }
}
