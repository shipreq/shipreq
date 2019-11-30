package shipreq.webapp.server.db

import doobie.imports._
import sourcecode.Line
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db.SingleConnectionXA
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic.IP
import shipreq.webapp.server.test._

object DbTriggerTest extends TestSuite {

  private def considerHourBoundary[A](a: => A): A =
    try a
    catch {
      case _: AssertionError =>
        // We may have crossed an hour boundary - try one more time
        a
    }

  override def tests = Tests {

    'usr_login_log {
      def logLogin(u: UserId, ip: Option[IP])(implicit xa: SingleConnectionXA): Unit =
        xa ! DbInterpreter.ForSecurity.logLoginSuccess(u, ip)

      "insert should update usr table" - TestDb().runNow { implicit xa =>
        def loginCount(userId: UserId)(implicit xa: SingleConnectionXA): Long =
          xa ! Query0[Long](s"SELECT login_count FROM usr WHERE id = ${userId.value}").unique

        val a, b = DbUtil(xa).newUserId()
        def loginCounts = (loginCount(a), loginCount(b))
        def assertLoginCounts(x: Long, y: Long) = assertEq(loginCounts, (x, y))
        assertLoginCounts(0, 0)
        logLogin(a, None); assertLoginCounts(1, 0)
        logLogin(a, None); assertLoginCounts(2, 0)
        logLogin(b, None); assertLoginCounts(2, 1)
      }

      "insert should update usr_logins_per_hour table" - TestDb().runNow { implicit xa =>
        considerHourBoundary {
          xa ! DbTable.UsrLoginsPerHour.truncate
          val a, b = DbUtil(xa).newUserId()
          def stats = (
            xa ! DbTable.UsrLoginsPerHour.count,
            xa ! Query0[Option[Int]](s"SELECT sum(total) FROM usr_logins_per_hour").option.map(_.flatten.getOrElse(0)),
            xa ! Query0[Option[Int]](s"SELECT hll_cardinality(hll_union_agg(users)) FROM usr_logins_per_hour").option.map(_.flatten.getOrElse(0)),
          )
          def assertViewCounts(rows: Int, total: Int, unique: Int) = assertEq(stats, (rows, total, unique))
          assertViewCounts(0, 0, 0)
          logLogin(a, None); assertViewCounts(1, 1, 1)
          logLogin(a, None); assertViewCounts(1, 2, 1)
          logLogin(b, None); assertViewCounts(1, 3, 2)
          logLogin(a, None); assertViewCounts(1, 4, 2)
          logLogin(b, None); assertViewCounts(1, 5, 2)
        }
      }
    }

    'usrd {
      def nameHistory(userId: Long)(implicit xa: SingleConnectionXA) =
        xa ! Query0[String](s"select name from usrh_name where usr_id=$userId order by updated_at").list

      def insert(userId: Long, name: String, newsletter: Boolean)(implicit xa: SingleConnectionXA) =
        xa ! Update[(Long, String, Boolean)]("insert into usrd values(?,?,?)")
          .toUpdate0((userId, name, newsletter)).run

      def update(userId: Long, name: String, newsletter: Boolean)(implicit xa: SingleConnectionXA) =
        xa ! Update[(String, Boolean, Long)]("update usrd set name=?, newsletter=? where usr_id=?")
          .toUpdate0((name, newsletter, userId)).run

      def read(userId: Long)(implicit xa: SingleConnectionXA) =
        xa ! Query0[(String,Boolean)](s"select name, newsletter from usrd where usr_id=$userId").unique

      "should record name changes" - TestDb().runNow { implicit xa =>
        val u = DbUtil(xa).newUserId()
        val (a,b,c) = ("Alice","Bob","Yay")
        insert(u.value, a, true)
        assertEq(nameHistory(u.value), Nil)
        List(b,b,b,c).foreach(update(u.value, _, true))
        assertEq(nameHistory(u.value), List(a, b))
      }

      "should updates without altercation by triggers" - TestDb().runNow { implicit xa =>
        val u = DbUtil(xa).newUserId()
        insert(u.value, "A", true)
        update(u.value, "B", false)
        assertEq(read(u.value), ("B", false))
      }
    }

    'project {
      "should update project_access_per_hour" - TestDb().runNow { implicit xa =>

        val db = new DbInterpreter()(PrepareEnv.global().config.server.security)

        def read(p: ProjectId): Unit =
          xa ! db.projectSpaInitPage(p)

        def writeTo(p: ProjectId): Unit =
          assertEq(xa ! DbInterpreter.SaveProjectEventLogic.updateProjectN.toUpdate0(("A", p)).run, 1)

        def stats = (
          xa ! DbTable.ProjectAccessPerHour.count,
          xa ! Query0[Option[Int]](s"SELECT sum(total) FROM project_access_per_hour WHERE NOT write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT hll_cardinality(hll_union_agg(projects)) FROM project_access_per_hour WHERE NOT write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT sum(total) FROM project_access_per_hour WHERE write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT hll_cardinality(hll_union_agg(projects)) FROM project_access_per_hour WHERE write").option.map(_.flatten.getOrElse(0)),
        )

        def assertState(rows: Int)(totalReads: Int, uniqueReads: Int)(totalWrites: Int, uniqueWrites: Int)(implicit l: Line) =
          assertEq(stats, (rows, totalReads, uniqueReads, totalWrites, uniqueWrites))

        considerHourBoundary {
          xa ! DbTable.ProjectAccessPerHour.truncate
          assertState(0)(0, 0)(0, 0)
          val a = DbUtil(xa).newProjectId()
          assertState(1)(0, 0)(1, 1)
          val b = DbUtil(xa).newProjectId()
          assertState(1)(0, 0)(2, 2)
          writeTo(a); assertState(1)(0, 0)(3, 2)
          read(b)   ; assertState(2)(1, 1)(3, 2)
          read(b)   ; assertState(2)(2, 1)(3, 2)
          writeTo(b); assertState(2)(2, 1)(4, 2)
          read(a)   ; assertState(2)(3, 2)(4, 2)
        }
      }
    }
  }
}
