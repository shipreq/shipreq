package shipreq.webapp.server.db

import org.scalatest.FunSpec
import scala.slick.jdbc.StaticQuery.{queryNA, query => queryQ, update => updateQ}
import shipreq.taskman.api.UserId
import shipreq.webapp.server.test.TestDatabaseSupport

class DbTriggerTest extends FunSpec with TestDatabaseSupport {

  describe(Tables.UsrLoginLog.name) {
    def loginCount(userId: UserId): Long =
      queryNA[Long](s"SELECT login_count FROM usr WHERE id = ${userId.value}").first

    it("should update agg view stats by trigger") {
      val a, b = newUserId()
      def viewCounts = (loginCount(a), loginCount(b))
      viewCounts shouldBe (0,0)
      dao.logUserLogin(a, None); viewCounts shouldBe (1,0)
      dao.logUserLogin(a, None); viewCounts shouldBe (2,0)
      dao.logUserLogin(b, None); viewCounts shouldBe (2,1)
    }
  }

  describe(Tables.Usrd.name) {
    def nameHistory(userId: Long) =
      queryNA[String](s"select name from usrh_name where usr_id=$userId order by updated_at").list

    def insert(userId: Long, name: String, newsletter: Boolean) =
      updateQ[(Long, String, Boolean)]("insert into usrd values(?,?,?)")
        .apply(userId, name, newsletter).execute

    def update(userId: Long, name: String, newsletter: Boolean) =
      updateQ[(String, Boolean, Long)]("update usrd set name=?, newsletter=? where usr_id=?")
        .apply(name, newsletter, userId).execute

    def read(userId: Long) =
      queryNA[(String,Boolean)](s"select name, newsletter from usrd where usr_id=$userId").first

    it("should record name changes") {
      val u = newUserId()
      val (a,b,c) = ("Alice","Bob","Yay")
      insert(u.value, a, true)
      nameHistory(u.value) shouldBe Nil
      List(b,b,b,c).foreach(update(u.value, _, true))
      nameHistory(u.value) shouldBe List(a, b)
    }

    it("should updates without altercation by triggers") {
      val u = newUserId()
      insert(u.value, "A", true)
      update(u.value, "B", false)
      read(u.value) shouldBe ("B", false)
    }
  }
}
