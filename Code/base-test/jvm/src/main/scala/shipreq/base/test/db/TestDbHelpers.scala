package shipreq.base.test.db

import doobie._
import doobie.free.{connection => C}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.testutil.TestUtil._
import japgolly.microlibs.utils.AsciiTable
import org.postgresql.util.PSQLException
import sourcecode.Line

trait TestDbHelpers {

  protected def tables: Set[DbTable]
  protected def ![A](query: ConnectionIO[A]): A

  // ===================================================================================================================
  // Table counts

  def countRows(tableName: String): Int =
    select[Int](s"select count(1) from $tableName")

  def countRows(tableName: String, where: String): Int =
    select[Int](s"select count(1) from $tableName where $where")

  private lazy val countAllQuery =
    DbTable.countAll(tables)

  def countRowsInAllTables(): DbTable.Counts =
    this ! countAllQuery

  def rowCountChanges[A](fn: => A): (A, DbTable.Counts) = {
    val before = countRowsInAllTables()
    val result = fn
    val after =
      try
        countRowsInAllTables()
      catch {
        case e: PSQLException if e.getMessage.contains("current transaction is aborted") => before
      }
    val diff = after - before
    (result, diff)
  }

  def assertRowCountChanges[A](expectations: (String, Int)*)(fn: => A)(implicit l: Line): A = {
    val (result, diff) = rowCountChanges(fn)
    val emap           = expectations.toMap
    val expectedDiff   = tables.map(t => t.name -> emap.getOrElse(t.name, 0)).toMap
    assertMap(diff.asMap.mapKeysNow(_.name), expectedDiff)
    result
  }

  // ===================================================================================================================
  // Generic usage

  def select[A: Read](sql: String): A =
    this ! Query0[A](sql).unique

  def selectOption[A: Read](sql: String): Option[A] =
    this ! Query0[A](sql).option

  def printTableCounts(): Unit =
    System.err.println(countRowsInAllTables().toTable)

  def debugSelect(sql: String): Unit =
    this ! C.raw { conn =>
      val stmt = conn.createStatement()
      val rs = stmt.executeQuery(sql)
      val cols = (1 to rs.getMetaData.getColumnCount).toVector
      var lines = Vector.empty[Vector[String]]
      def readLine(f: Int => String): Unit = lines :+= cols.map(f)
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

}
