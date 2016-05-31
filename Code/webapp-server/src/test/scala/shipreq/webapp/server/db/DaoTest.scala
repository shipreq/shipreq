package shipreq.webapp.server.db

import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import org.scalatest.FunSpec
import slick.jdbc.StaticQuery.{queryNA, update, updateNA}
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.UserId
import shipreq.webapp.server.data._
import shipreq.webapp.server.db.EventDao.EventSeq
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

//    def newUserAndProject() = {
//      val u = newUserId()
//      val p = dao.createProject(u)
//      (u, p)
//    }

    describe("create") {
      it("should create a new project") {
        val u = newUserId()
        assertTableDiffs(TProject -> 1) {dao.createProject(u)}
      }
    }

//    describe("rename") {
//      import UpdateProjectResult._
//
//      it("should update the project name") {
//        val (u, p) = newUserAndProject("A")
//        assertTableDiffs()(dao.updateProject(p, u, "B")) ==== DbSuccess
//        dao.findProject(p).get.name ==== "B"
//      }
//
//      it("should reject duplicate names") {
//        val (u, p1) = newUserAndProject("A")
//        val p2 = dao.createProject(u, "B").gimme
//        dao.updateProject(p2, u, "A") ==== NameAlreadyInUse
//      }
//
//      it("should fail when project not found") {
//        dao.updateProject(ProjectId(0), UserId(0), "A") ==== ProjectNotFound
//      }
//
//      it("should fail when project doesnt belong to user") {
//        val (u, p) = newUserAndProject("A")
//        dao.updateProject(p, newUserId(), "B") ==== ProjectNotFound
//      }
//    }

//    def afterDeletion: (UserId, ProjectId, ProjectId) = {
//      val (uid, p1) = newUserAndProject("wow")
//      assertTableDiffs()(dao deleteProjectSoft p1)
//      val p2 = dao.createProject(uid, "wow").gimme
//      assertTableDiffs()(dao deleteProjectSoft p2)
//      (uid, p1, p2)
//    }
//
//    it("deletion should be soft and hard") {
//      val (_, p1, p2) = afterDeletion
//      val a = new AsyncDao
//      assertTableDiffs(Tables.Project -> -1)(a deleteProject p1)
//      assertTableDiffs(Tables.Project -> -1)(a deleteProject p2)
//    }
//
//    it("soft deletion should hide the project from view") {
//      val (u, p, _) = afterDeletion
//      dao.findProject(p) shouldBe None
//    }
  }

  // ===================================================================================================================

  describe("Instant") {
    def assertApproxEqual(a: Instant, e: Instant): Unit =
      Duration.between(a, e).abs.minusSeconds(2).isNegative shouldBe true

    it("should read") {
      val (dbNow, i) = queryNA[(Instant, Int)](s"select now(), 2").first
      i shouldBe 2
      assertApproxEqual(dbNow, Instant.now())
    }

    it("should read Some") {
      val (dbNow, i) = queryNA[(Option[Instant], Int)](s"select now(), 3").first
      i shouldBe 3
      dbNow.isDefined shouldBe true
      assertApproxEqual(dbNow.get, Instant.now())
    }

    it("should read None") {
      val (dbNow, i) = queryNA[(Option[Instant], Int)](s"select null :: timestamptz, 5").first
      i shouldBe 5
      dbNow.isDefined shouldBe false
    }

    it("should write") {
      val u = newUserId()
      val l = LocalDateTime.of(1984, 5, 2, 18, 30, 8)
      val i = l.toInstant(ZoneOffset.of("+11:00"))
      val r = "yay"
      update[(Instant, String, Long)]("UPDATE usr SET confirmed_at=?, roles=? WHERE id=?").apply((i, r, u.value)).execute
      val s = queryNA[String](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").first
      s shouldBe "1984-05-02T07:30:08Z"
      val (ar, ai) = queryNA[(String, Instant)](s"select roles, confirmed_at from usr where id=${u.value: Long}").first
      ai shouldBe i
      ar shouldBe r
    }

    it("should write Option") {
      val u = newUserId()
      val l = LocalDateTime.of(1990, 9, 7, 20, 20, 4)
      val i = l.toInstant(ZoneOffset.of("+15:00"))
      update[(Option[Instant], Option[Instant], Long)]("UPDATE usr SET reset_password_sent_at=?, confirmed_at=? WHERE id=?")
        .apply((None, Some(i), u.value)).execute
      val s1 = queryNA[Option[String]](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").first
      s1 shouldBe Some("1990-09-07T05:20:04Z")
      val s2 = queryNA[Option[String]](s"select to_iso8601_str(reset_password_sent_at) from usr where id=${u.value: Long}").first
      s2 shouldBe None

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