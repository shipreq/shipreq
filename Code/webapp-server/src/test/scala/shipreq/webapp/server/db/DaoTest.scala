package shipreq.webapp.server.db

import org.scalatest.FunSpec
import slick.jdbc.StaticQuery.{queryNA, updateNA}
import shipreq.taskman.api.UserId
import shipreq.webapp.server.data._
import shipreq.webapp.server.lib.Types._
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.server.snippet.ResetPassword
import shipreq.webapp.server.test.TestDatabaseSupport

class DaoTest extends FunSpec with TestDatabaseSupport {

  describe("to_iso8601_str") {

    def test(in: String, out: String): Unit = {
      val q = queryNA[String](s"select to_iso8601_str(timestamptz '$in')")
      val r: String = q.first
      r shouldBe out
    }

    it("should work with typical precision") {
      test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")
    }

    it("should work with more precision") {
      test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")
    }

    it("should work with lesser precision") {
      test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")
    }

    it("should work with NULLs") {
      val q = queryNA[String](s"select to_iso8601_str(NULL)")
      q.first shouldBe null
    }
  }

  // ===================================================================================================================

  describe("Project") {
    import Tables.{Project => TProject}

    def newUserAndProject(projectName: String) = {
      val u = newUserId()
      val p = dao.createProject(u, projectName).gimme
      (u, p)
    }

    describe("create") {
      import CreateProjectResult._

      it("should create a new project") {
        val u = newUserId()
        assertTableDiffs(TProject -> 1) {dao.createProject(u, "Blah")}
      }

      it("should reject duplicate project names") {
        val t = "Yay"
        val (u, _) = newUserAndProject(t)
        val (_, _) = newUserAndProject(t)
        dao.createProject(u, t) ==== NameAlreadyInUse
      }
    }

    describe("update") {
      import UpdateProjectResult._

      it("should update the project name") {
        val (u, p) = newUserAndProject("A")
        assertTableDiffs()(dao.updateProject(p, u, "B")) ==== DbSuccess
        dao.findProject(p).get.name ==== "B"
      }

      it("should reject duplicate names") {
        val (u, p1) = newUserAndProject("A")
        val p2 = dao.createProject(u, "B").gimme
        dao.updateProject(p2, u, "A") ==== NameAlreadyInUse
      }

      it("should fail when project not found") {
        dao.updateProject(ProjectId(0), UserId(0), "A") ==== ProjectNotFound
      }

      it("should fail when project doesnt belong to user") {
        val (u, p) = newUserAndProject("A")
        dao.updateProject(p, newUserId(), "B") ==== ProjectNotFound
      }
    }

    def afterDeletion: (UserId, ProjectId, ProjectId) = {
      val (uid, p1) = newUserAndProject("wow")
      assertTableDiffs()(dao deleteProjectSoft p1)
      val p2 = dao.createProject(uid, "wow").gimme
      assertTableDiffs()(dao deleteProjectSoft p2)
      (uid, p1, p2)
    }

    it("deletion should be soft and hard") {
      val (_, p1, p2) = afterDeletion
      val a = new AsyncDao
      assertTableDiffs(Tables.Project -> -1)(a deleteProject p1)
      assertTableDiffs(Tables.Project -> -1)(a deleteProject p2)
    }

    it("soft deletion should hide the project from view") {
      val (u, p, _) = afterDeletion
      dao.findProject(p) shouldBe None
    }
  }

  // ===================================================================================================================

  describe("User") {

    it("reset password fns") {
      val u = newUserId()
      val username = queryNA[String](s"select username from usr where id=${u.value: Long}").first
      val token = dao.performInstallNewResetPasswordToken(u, () => s"token.$u")

      val date = dao.findResetPasswordTokenIssuedDate(token).get
      ResetPassword.isTokenExpired(date) shouldBe false

      dao.performReuseResetPasswordToken(u)
      val date2 = dao.findResetPasswordTokenIssuedDate(token).get
      ResetPassword.isTokenExpired(date2) shouldBe false

      val p = "hehegreat100"
      val ps = PasswordAndSalt.createWithRandomSalt(p)
      dao.performPasswordReset(ps, token)

      dao.findResetPasswordTokenIssuedDate(token) shouldBe None
      val ps2 = dao.findUserDescAndCredentials(username).get._2
      ps2.matches(p) shouldBe true
    }
  }
}