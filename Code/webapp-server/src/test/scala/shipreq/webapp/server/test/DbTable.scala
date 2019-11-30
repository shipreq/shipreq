package shipreq.webapp.server.test

import doobie.imports._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.std.list._
import scalaz.syntax.traverse._
import shipreq.base.db.DoobieHelpers._

sealed abstract class DbTable(val name: String) {
  override def toString = name
  def count: ConnectionIO[Int] = Query0[Int]("select count(*) from " + name).unique
  def truncate: ConnectionIO[Unit] = Update0(s"truncate table $name cascade", None).execute
}

object DbTable {
  case object Event                extends DbTable("event")
  case object Project              extends DbTable("project")
  case object ProjectAccessPerHour extends DbTable("project_access_per_hour")
  case object Usr                  extends DbTable("usr")
  case object UsrLoginLog          extends DbTable("usr_login_log")
  case object UsrLoginsPerHour     extends DbTable("usr_logins_per_hour")
  case object Usrd                 extends DbTable("usrd")
  case object UsrhName             extends DbTable("usrh_name")

  implicit def univEq: UnivEq[DbTable] = UnivEq.derive

  val All = AdtMacros.adtValues[DbTable].toNES

  def find(name: String): Option[DbTable] =
    All.whole.find(_.name ==* name)

  def need(name: String): DbTable =
    find(name).getOrElse(sys error s"Unknown table name: $name")

  def validate(schema: String): ConnectionIO[Unit] =
    Query0[String](s"select tablename from pg_tables where schemaname = '$schema'")
      .list.map { actualList =>
      val actual = actualList.toSet - "flyway_schema_history"
      val got = All.whole.map(_.name)
      if (got ==* actual)
        ()
      else Some {
        val missing = actual -- got
        val deleted = got -- actual
        sys error s"Actual tables (${actual.size}) ≠ DbTable.All (${All.size}): missing: $missing, deleted: $deleted"
      }
    }

  final case class Counts(table: Map[DbTable, Int]) {
    def isEmpty: Boolean =
      table.values.forall(_ ==* 0)

    def nonEmpty: Boolean =
      !isEmpty

    def +(other: Counts): Counts =
      Counts(table.map {case (t, n) => (t, n - other.table(t))})

    def -(before: Counts): Counts =
      Counts(table.map {case (t, n) => (t, n - before.table(t))})
  }

  lazy val countAll: ConnectionIO[Counts] =
    Query0[(String, Int)](
      DbTable.All
        .iterator
        .map(t => s"select '${t.name}',count(1) from ${t.name}")
        .mkString(" union ")
    ).list
      .map(_.iterator.map(_.map1(DbTable.need)).toMap)
      .map(Counts)

  lazy val truncateAll: ConnectionIO[Unit] =
    DbTable.All.whole.toList.map(_.truncate).sequence_
}
