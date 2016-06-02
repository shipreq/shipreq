package shipreq.webapp.server.test

import com.googlecode.flyway.core.dbsupport.{DbSupportFactory, SqlScript}
import org.apache.commons.io.IOUtils
import org.postgresql.util.PSQLException
import org.scalatest.{Exceptional, Outcome, Suite}
import shipreq.base.util.{AsciiTable, ThreadLocalRes}
import scala.slick.jdbc.StaticQuery.{query, queryNA, update, updateNA}
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import scala.util.Random
import scalaz.Need
import shipreq.base.db.SqlHelpers._
import shipreq.base.util.univeq._
import shipreq.taskman.api.UserId
import shipreq.webapp.base.data.Validators
import shipreq.webapp.server.app.{DI, Defaults}
import shipreq.webapp.server.data._
import shipreq.webapp.server.db
import shipreq.webapp.server.db.{AdminDao, DB, DaoProvider, DaoS, DaoT}
import shipreq.webapp.server.db.SqlHelpers._
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.base.test.BaseTestUtil._

object DbUtil {
  def apply(s: Session) =
    new DbUtil()(s)

  val Random = new Random()
}


class DbUtil()(implicit val session: Session) {

  val dao = shipreq.webapp.server.db.Shim.newDaoT(session)

  private def randomStr: String =
    DbUtil.Random.nextString(32)

  def newProjectId(userId: UserId = getOrCreateUserId()): ProjectId =
    dao.createProject(userId)

  def getOrCreateUserId(): UserId =
    queryNA[UserId]("select id from usr where username is not null").firstOption getOrElse newUserId()

  def newUserId(): UserId =
    query[(String,String),UserId]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,0,0,NOW(),NOW(),NOW()) RETURNING id")
    .apply(randomStr, randomStr).first

  def deleteUser(u: UserId): Unit =
    update[Long]("DELETE FROM usr WHERE id=?").apply(u.value).execute

  def debugSelect(sql: String): Unit = {
    val stmt = session.conn.createStatement()
    val rs = stmt.executeQuery(sql)
    val cols = (1 to rs.getMetaData.getColumnCount).toVector
    var lines = Vector.empty[Vector[String]]
    def readLine(f: Int => String): Unit =
      lines :+= cols.map(f)
    readLine(rs.getMetaData.getColumnName)
    while (rs.next())
      readLine(rs.getString)
    val table = AsciiTable(lines)
    println(s"\n> $sql\n$table\n")
  }

  def debugSelectOnError[A](sql: => String)(f: => A): A =
    try f catch {
      case t: Throwable =>
        debugSelect(sql)
        throw t
    }

  def countRowsIn(table: DbTable): Int =
    queryNA[Int]("select count(*) from " + table.name).first

  def countRowsInAllTables(): Map[DbTable, Int] =
    DbTable.All.whole.map(t => t -> countRowsIn(t)).toMap

  def rowCountChanges[A](fn: => A): (A, Map[DbTable, Int]) = {
    val before = countRowsInAllTables()
    val result = fn
    val after = try {
      countRowsInAllTables()
    } catch {
      case e: PSQLException if e.getMessage.contains("current transaction is aborted") => before
    }
    val diff = after.map {case (t, newCount) => (t, newCount - before(t))}
    (result, diff)
  }

  def assertRowCountChanges[A](expectations: (DbTable, Int)*)(fn: => A) = {
    val (result, diff) = rowCountChanges(fn)
    val emap         = expectations.toMap
    val expectedDiff = DbTable.All.whole.map(t => t -> emap.getOrElse(t, 0)).toMap
    assertMap(diff, expectedDiff)
    result
  }

  def lookupConfirmationToken(email: String): Option[String] =
    query[String, String]("select confirmation_token from usr where email=?").apply(email).firstOption

}
