package shipreq.webapp.server.db

import scala.slick.jdbc.JdbcBackend.Session
import scala.slick.jdbc.StaticQuery.{queryNA, query => queryQ, update => updateQ}
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.taskman.api.UserId
import shipreq.webapp.server.test.TestDb.DbUtil

object DbTriggerTest extends TestSuite {

  override def tests = TestSuite {

    'usr_login_log {
      def loginCount(userId: UserId)(implicit s: Session): Long =
        queryNA[Long](s"SELECT login_count FROM usr WHERE id = ${userId.value}").first

      "should update agg view stats by trigger" - DbUtil { dbu =>
        import dbu._
        val a, b = newUserId()
        def viewCounts = (loginCount(a), loginCount(b))
        def assertViewCounts(x: Long, y: Long) = assertEq(viewCounts, (x, y))
        assertViewCounts(0, 0)
        dao.logUserLogin(a, None); assertViewCounts(1, 0)
        dao.logUserLogin(a, None); assertViewCounts(2, 0)
        dao.logUserLogin(b, None); assertViewCounts(2, 1)
      }
    }

    'usrd {
      def nameHistory(userId: Long)(implicit s: Session) =
        queryNA[String](s"select name from usrh_name where usr_id=$userId order by updated_at").list

      def insert(userId: Long, name: String, newsletter: Boolean)(implicit s: Session) =
        updateQ[(Long, String, Boolean)]("insert into usrd values(?,?,?)")
          .apply(userId, name, newsletter).execute

      def update(userId: Long, name: String, newsletter: Boolean)(implicit s: Session) =
        updateQ[(String, Boolean, Long)]("update usrd set name=?, newsletter=? where usr_id=?")
          .apply(name, newsletter, userId).execute

      def read(userId: Long)(implicit s: Session) =
        queryNA[(String,Boolean)](s"select name, newsletter from usrd where usr_id=$userId").first

      "should record name changes" - DbUtil { dbu =>
        import dbu._
        val u = newUserId()
        val (a,b,c) = ("Alice","Bob","Yay")
        insert(u.value, a, true)
        assertEq(nameHistory(u.value), Nil)
        List(b,b,b,c).foreach(update(u.value, _, true))
        assertEq(nameHistory(u.value), List(a, b))
      }

      "should updates without altercation by triggers" - DbUtil { dbu =>
        import dbu._
        val u = newUserId()
        insert(u.value, "A", true)
        update(u.value, "B", false)
        assertEq(read(u.value), ("B", false))
      }
    }

  }
}
