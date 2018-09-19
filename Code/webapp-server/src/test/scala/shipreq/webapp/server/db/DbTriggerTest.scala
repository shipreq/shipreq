package shipreq.webapp.server.db

import doobie.imports._
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db.SingleConnectionXA
import shipreq.webapp.base.user._
import shipreq.webapp.server.test._

object DbTriggerTest extends TestSuite {

  override def tests = Tests {

    /*
    'usr_login_log {
      def loginCount(userId: UserId)(implicit xa: SingleConnectionXA): Long =
        xa ! Query0[Long](s"SELECT login_count FROM usr WHERE id = ${userId.value}").unique

      "should update agg view stats by trigger" - TestDb().runNow { implicit xa =>
        val a, b = DbUtil(xa).newUserId()
        def viewCounts = (loginCount(a), loginCount(b))
        def assertViewCounts(x: Long, y: Long) = assertEq(viewCounts, (x, y))
        assertViewCounts(0, 0)
        xa ! DbLogic.user.logLogin(a, None); assertViewCounts(1, 0)
        xa ! DbLogic.user.logLogin(a, None); assertViewCounts(2, 0)
        xa ! DbLogic.user.logLogin(b, None); assertViewCounts(2, 1)
      }
    }
    */

    'usrd {
      def nameHistory(userId: Long)(implicit xa: SingleConnectionXA) =
        xa ! Query0[String](s"select name from usrh_name where usr_id=$userId order by updated_at").list

      def insert(userId: Long, name: String, newsletter: Boolean)(implicit xa: SingleConnectionXA) =
        xa ! Update[(Long, String, Boolean)]("insert into usrd values(?,?,?)")
          .toUpdate0(userId, name, newsletter).run

      def update(userId: Long, name: String, newsletter: Boolean)(implicit xa: SingleConnectionXA) =
        xa ! Update[(String, Boolean, Long)]("update usrd set name=?, newsletter=? where usr_id=?")
          .toUpdate0(name, newsletter, userId).run

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

  }
}
