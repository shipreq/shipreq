package shipreq.webapp.server.db

import doobie._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db._
import shipreq.webapp.base.data._
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{Event, EventOrd}
import shipreq.webapp.server.test._
import sourcecode.Line
import utest._

object DbTriggerTest extends TestSuite {

  private def considerHourBoundary[A](a: => A): A =
    try a
    catch {
      case _: AssertionError =>
        // We may have crossed an hour boundary - try one more time
        a
    }

  override def tests = Tests {
    PrepareEnv.dbOnce()

    "usr_login_log" - {
      def logLogin(u: UserId, ip: Option[IP])(implicit xa: ImperativeXA): Unit =
        xa ! DbInterpreter.ForSecurity.logLoginSuccess(u, ip)

      "insert should update usr table" - TestDb.withImperativeXA { implicit xa =>
        def loginCount(userId: UserId)(implicit xa: ImperativeXA): Long =
          xa ! Query0[Long](s"SELECT login_count FROM usr WHERE id = ${userId.value}").unique

        val a, b = DbUtil(xa).newUserId()
        def loginCounts = (loginCount(a), loginCount(b))
        def assertLoginCounts(x: Long, y: Long) = assertEq(loginCounts, (x, y))
        assertLoginCounts(0, 0)
        logLogin(a, None); assertLoginCounts(1, 0)
        logLogin(a, None); assertLoginCounts(2, 0)
        logLogin(b, None); assertLoginCounts(2, 1)
      }

      "insert should update usr_logins_per_hour table" - TestDb.withImperativeXA { implicit xa =>
        considerHourBoundary {
          xa ! DbTables.UsrLoginsPerHour.truncate
          val a, b = DbUtil(xa).newUserId()
          def stats = (
            xa ! DbTables.UsrLoginsPerHour.count,
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

    "project" - {
      "should update project_access_per_hour" - TestDb.withImperativeXA { implicit xa =>

        val db = new DbInterpreter()(PrepareEnv.global().config.server.security)

        def read(p: ProjectId)(implicit u: UserId): Unit =
          xa ! db.projectSpaInitPage(p, u)

        def writeTo(p: ProjectId, ordIdx: Int)(implicit u: UserId): Unit = {
          val ord = EventOrd.fromIndex(ordIdx)
          val e = Event.ProjectNameSet("A")
          xa ! db.saveProjectEvent(p, ord, e, Project.empty, u)
        }

        def stats = (
          xa ! DbTables.ProjectAccessPerHour.count,
          xa ! Query0[Option[Int]](s"SELECT sum(total) FROM project_access_per_hour WHERE NOT write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT hll_cardinality(hll_union_agg(projects)) FROM project_access_per_hour WHERE NOT write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT sum(total) FROM project_access_per_hour WHERE write").option.map(_.flatten.getOrElse(0)),
          xa ! Query0[Option[Int]](s"SELECT hll_cardinality(hll_union_agg(projects)) FROM project_access_per_hour WHERE write").option.map(_.flatten.getOrElse(0)),
        )

        def assertState(rows: Int)(totalReads: Int, uniqueReads: Int)(totalWrites: Int, uniqueWrites: Int)(implicit l: Line) =
          assertEq(
            "(rows, totalReads, uniqueReads, totalWrites, uniqueWrites)",
            stats,
            (rows, totalReads, uniqueReads, totalWrites, uniqueWrites))

        considerHourBoundary {
          xa ! DbTables.ProjectAccessPerHour.truncate
          assertState(0)(0, 0)(0, 0)
          implicit val u = DbUtil(xa).getOrCreateUserId()
          val a = DbUtil(xa).newProjectId(u)
          assertState(1)(0, 0)(1, 1)
          val b = DbUtil(xa).newProjectId(u)
          assertState(1)(0, 0)(2, 2)
          writeTo(a, 0); assertState(1)(0, 0)(3, 2)
          read(b)      ; assertState(2)(1, 1)(3, 2)
          read(b)      ; assertState(2)(2, 1)(3, 2)
          writeTo(b, 0); assertState(2)(2, 1)(4, 2)
          read(a)      ; assertState(2)(3, 2)(4, 2)
        }
      }
    }
  }
}
