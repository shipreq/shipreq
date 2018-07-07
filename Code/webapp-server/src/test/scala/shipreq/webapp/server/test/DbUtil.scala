package shipreq.webapp.server.test

import doobie.imports._
import japgolly.microlibs.utils.AsciiTable
import org.postgresql.util.PSQLException
import scala.util.Random
import shipreq.base.db.DoobieHelpers._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db.SingleConnectionXA
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.user.UserId
import shipreq.webapp.server.db.SqlHelpers._
import shipreq.webapp.server.db._

object DbUtil {

  val use = TestDb.mapUsage(_.map(xa => Fx(apply(xa))))

  val Random = new Random()
}

final case class DbUtil(xa: SingleConnectionXA) {
  import PrepareEnv.dbAlgebra

  private def randomStr: String =
    DbUtil.Random.nextString(32)

  def newProjectId(userId: UserId = getOrCreateUserId()): ProjectId =
    xa ! dbAlgebra.createEmptyProject(userId)

  def getOrCreateUserId(): UserId =
    (xa ! Query0[UserId]("select id from usr where username is not null").option) getOrElse newUserId()

  def newUserId(): UserId =
    xa ! Query[(String, String), UserId](
      "INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,0,0,NOW(),NOW(),NOW()) RETURNING id"
    ).toQuery0(randomStr, randomStr).unique

  def deleteUser(u: UserId): Unit =
    xa ! sql"DELETE FROM usr WHERE id=$u".update.execute

  def debugSelect(sql: String): Unit = {
    val stmt = xa.conn.createStatement()
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
    xa ! table.count

  def countRowsInAllTables(): DbTable.Counts =
    xa ! DbTable.countAll

  def rowCountChanges[A](fn: => A): (A, Map[DbTable, Int]) = {
    val before = countRowsInAllTables()
    val result = fn
    val after = try {
      countRowsInAllTables()
    } catch {
      case e: PSQLException if e.getMessage.contains("current transaction is aborted") => before
    }
    val diff = after - before
    (result, diff.table)
  }

  def assertRowCountChanges[A](expectations: (DbTable, Int)*)(fn: => A): A = {
    val (result, diff) = rowCountChanges(fn)
    val emap         = expectations.toMap
    val expectedDiff = DbTable.All.whole.map(t => t -> emap.getOrElse(t, 0)).toMap
    assertMap(diff, expectedDiff)
    result
  }

  def lookupConfirmationToken(email: String): Option[String] =
    xa ! sql"select confirmation_token from usr where email=$email".query[String].option
}
