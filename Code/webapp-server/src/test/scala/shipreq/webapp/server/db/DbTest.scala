package shipreq.webapp.server.db

import doobie.imports._
import java.time.{Duration, Instant, LocalDateTime, ZoneOffset}
import sourcecode.Line
import utest._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.user._
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._

object DbTest extends TestSuite {

  def db = PrepareEnv.dbAlgebra
  def dbSec = DbInterpreter.ForSecurity

  override def tests = Tests {

    'to_iso8601_str {
      def test(in: String, out: String): Unit =
        TestDb().runNow { xa =>
          val q = Query0[String](s"select to_iso8601_str(timestamptz '$in')")
          val r: String = xa ! q.unique
          assertEq(r, out)
        }

      'typicalPrecision -
        test("2013-08-16 09:32:48.002474+10", "2013-08-15T23:32:48Z")

      'morePrecision -
        test("2010-10-20 20:32:48.00247489+10", "2010-10-20T10:32:48Z")

      'lesserPrecision -
        test("2012-09-10 09:56:23.2157+11", "2012-09-09T22:56:23Z")

      'null - TestDb().runNow { xa =>
        val q = Query0[Option[String]](s"select to_iso8601_str(NULL)")
        assertEq(xa ! q.unique, None)
      }
    }

    'instant {
      def assertApproxEqual(a: Instant, e: Instant): Unit =
        assertEq(Duration.between(a, e).abs.minusSeconds(2).isNegative, true)

      'read - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Instant, Int)](s"select now(), 2").unique
        assertEq(i, 2)
        assertApproxEqual(dbNow, Instant.now())
      }

      'readSome - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select now(), 3").unique
        assertEq(i, 3)
        assertEq(dbNow.isDefined, true)
        assertApproxEqual(dbNow.get, Instant.now())
      }

      'readNone - TestDb().runNow { xa =>
        val (dbNow, i) = xa ! Query0[(Option[Instant], Int)](s"select null :: timestamptz, 5").unique
        assertEq(i, 5)
        assertEq(dbNow.isDefined, false)
      }

      'write - TestDb().runNow { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2084, 5, 2, 18, 30, 8)
        val i = l.toInstant(ZoneOffset.of("+11:00"))
        xa ! Update[(Instant, Long)]("UPDATE usr SET confirmed_at=? WHERE id=?").toUpdate0((i, u.value)).run
        val s1 = xa ! Query0[String](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, "2084-05-02T07:30:08Z")
        val ai = xa ! Query0[Instant](s"select confirmed_at from usr where id=${u.value: Long}").unique
        assertEq(ai, i)
      }

      'writeOption - TestDb().runNow { xa =>
        val u = DbUtil(xa).newUserId()
        val l = LocalDateTime.of(2030, 9, 7, 20, 20, 4)
        val i = l.toInstant(ZoneOffset.of("+15:00"))
        xa ! Update[(Option[Instant], Option[Instant], Long)]("UPDATE usr SET reset_password_sent_at=?, confirmed_at=? WHERE id=?")
          .toUpdate0((None, Some(i), u.value)).run
        val s1 = xa ! Query0[Option[String]](s"select to_iso8601_str(confirmed_at) from usr where id=${u.value: Long}").unique
        assertEq(s1, Some("2030-09-07T05:20:04Z"))
        val s2 = xa ! Query0[Option[String]](s"select to_iso8601_str(reset_password_sent_at) from usr where id=${u.value: Long}").unique
        assertEq(s2, None)
      }
    }

    'user {
      'resetPasswordFns - TestDb().runNow { xa =>
        val dbu = DbUtil(xa)
        val u = dbu.newUserId()
        val username = xa ! Query0[String](s"select username from usr where id=${u.value: Long}").unique
        val token = xa ! db.createResetPasswordToken(u)

//        val date = xa ! db.getResetPasswordTokenIssueDate(token)
//        assert(!ResetPassword.isTokenExpired(date.get)) TODO

        xa ! db.updateResetPasswordTokenOnReissue(u)
//        val date2 = xa ! db.getResetPasswordTokenIssueDate(token)
//        assert(!ResetPassword.isTokenExpired(date2.get)) TODO

        val p = PlainTextPassword("hehegreat100")
        val ps = Global.security.hashPassword(p).unsafeRun()
        xa ! db.updateUserPassword(token, ps)

        assertEq(xa ! db.getResetPasswordTokenIssueDate(token), None)
//        val ps2 = (xa ! dbSec.getUserAndPassword(username)).get._2
//        assertEq(ps2.matches(p), true)
      }
    }

//  describe("Project") {
//    import Tables.{Project => TProject}
//
////    def newUserAndProject() = {
////      val u = newUserId()
////      val p = dao.createProject(u)
////      (u, p)
////    }
//
//    describe("create") {
//      it("should create a new project") {
//        val u = newUserId()
//        assertTableDiffs(TProject -> 1) {dao.createProject(u)}
//      }
//    }
//
////    describe("rename") {
////      import UpdateProjectResult._
////
////      it("should update the project name") {
////        val (u, p) = newUserAndProject("A")
////        assertTableDiffs()(dao.updateProject(p, u, "B")) ==== DbSuccess
////        dao.findProject(p).get.name ==== "B"
////      }
////
////      it("should reject duplicate names") {
////        val (u, p1) = newUserAndProject("A")
////        val p2 = dao.createProject(u, "B").gimme
////        dao.updateProject(p2, u, "A") ==== NameAlreadyInUse
////      }
////
////      it("should fail when project not found") {
////        dao.updateProject(ProjectId(0), UserId(0), "A") ==== ProjectNotFound
////      }
////
////      it("should fail when project doesnt belong to user") {
////        val (u, p) = newUserAndProject("A")
////        dao.updateProject(p, newUserId(), "B") ==== ProjectNotFound
////      }
////    }
//
////    def afterDeletion: (UserId, ProjectId, ProjectId) = {
////      val (uid, p1) = newUserAndProject("wow")
////      assertTableDiffs()(dao deleteProjectSoft p1)
////      val p2 = dao.createProject(uid, "wow").gimme
////      assertTableDiffs()(dao deleteProjectSoft p2)
////      (uid, p1, p2)
////    }
////
////    it("deletion should be soft and hard") {
////      val (_, p1, p2) = afterDeletion
////      val a = new AsyncDao
////      assertTableDiffs(Tables.Project -> -1)(a deleteProject p1)
////      assertTableDiffs(Tables.Project -> -1)(a deleteProject p2)
////    }
////
////    it("soft deletion should hide the project from view") {
////      val (u, p, _) = afterDeletion
////      dao.findProject(p) shouldBe None
////    }
//  }

    'event {

      'prop - {
        import IgnoreEqualityOfVerifiedEventTimestamps._
        TestDb().runNow { xa =>

          val data  = RandomEventStream.sampleEventStreamWithProjects
          val data1 = data.take(RandomEventStream.InitialEventCount)
          val data2 = data.drop(data1.length)
          val dbu   = DbUtil(xa)
          val uid   = dbu.newUserId()
          val pid   = xa ! db.createProject(uid, data1.map(_._1.event.active), data1.last._2)

          def assertPMD(expect: ProjectMetaData => ProjectMetaData)(implicit l: Line): Unit = {
            val a = (xa ! db.getProjectMetaData(pid)).get
            val e = expect(a)
            assertEq(a, e)
          }

          val read1 = (xa ! db.getAllProjectEvents(pid)).needRight
          assertEq("init event count", read1.size, data1.length)
          assertEq("first ord", read1.head.ord, EventOrd.first)
          assertPMD(a => ProjectMetaData.fromProject(data1.last._2)(
            id            = a.id,
            eventsInit    = data1.length,
            eventsTotal   = data1.length,
            createdAt     = a.createdAt,
            accessedAt    = a.accessedAt,
            lastUpdatedAt = None))

          var ord = read1.last.ord
          for ((e, p) <- data2) {
            ord = EventOrd(ord.value + 1)
            (xa ! db.saveProjectEvent(pid, ord, e.event.active, p, uid)).needRight
          }
          val readAll = (xa ! db.getAllProjectEvents(pid)).needRight
          assertSeq(readAll, data.map(_._1))
          assertPMD(a => ProjectMetaData.fromProject(data.last._2)(
            id            = a.id,
            eventsInit    = data1.length,
            eventsTotal   = data.length,
            createdAt     = a.createdAt,
            accessedAt    = a.lastUpdatedAt.get,
            lastUpdatedAt = Some(a.lastUpdatedAt.get)))
        }
      }

    }

  }
}
