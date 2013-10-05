package com.beardedlogic.usecase.app

import AppSiteMap._
import Implicits._
import com.beardedlogic.usecase.test.LiveTest
import com.beardedlogic.usecase.test.fixture.UserFixture
import org.apache.commons.httpclient.{HttpMethodBase, HttpClient}
import org.scalatest.FunSpec
import com.beardedlogic.usecase.db.UseCaseHeader

class PermissionTest extends FunSpec with LiveTest with UserFixture {

  override def beforeAll() {
    super.beforeAll()
    initUserFixtureWithoutTransaction()
  }

  implicit override def responseCapture(fullUrl: String, httpClient: HttpClient, getter: HttpMethodBase) = {
    getter.setFollowRedirects(false)
    super.responseCapture(fullUrl, httpClient, getter)
  }

  def doLogin(user: TestUser) =
    post("/login.api", "user" -> user.username, "pass" -> user.password) !@ "Failed to log in"

  def loginShouldBeRequiredFor(url: String) =
    get(url) shouldRedirectTo(Login.relativeUrl)

  // -------------------------------------------------------------------------------------------------------------------

  lazy val pid = newProjectId(user1.id)
  lazy val ucId = withNewTransaction(dao.createUseCaseIdentAndRev1(pid, UseCaseHeader("Hello")))

  describe("/") {
    it("anon") {
      val r = get("/") ! 200
      r.responseText should (include("/login") and not include ("#project-hub"))
    }

    it("auth") {
      val r = doLogin(user1).get("/") ! 200
      r.responseText should (include("project-hub") and not include ("/login"))
    }
  }

  describe("/project") {
    lazy val url = Project.relativeUrl(pid)

    it("should deny anon") {
      loginShouldBeRequiredFor(url)
    }

    it("should allow owner") {
      val r = doLogin(user1).get(url) ! 200
      r.responseText should include("project-title")
    }

    it("should deny non-owner") {
      doLogin(user2).get(url) shouldRedirect
    }
  }

  describe("/usecase") {
    lazy val url = UseCaseEditor.relativeUrl(ucId.identId)

    it("should deny anon") {
      loginShouldBeRequiredFor(url)
    }

    it("should allow owner") {
      val r = doLogin(user1).get(url) ! 200
      r.responseText should include("addTailStep")
    }

    it("should deny non-owner") {
      doLogin(user2).get(url) shouldRedirect
    }
  }
}
