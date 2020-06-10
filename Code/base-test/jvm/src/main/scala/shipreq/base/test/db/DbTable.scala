package shipreq.base.test.db

import cats.instances.list._
import cats.syntax.traverse._
import doobie._
import doobie.implicits._
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.AsciiTable
import japgolly.univeq._
import shipreq.base.db.DoobieHelpers._

final case class DbTable(name: String) {
  override def toString = name

  val count: ConnectionIO[Int] =
    Query0[Int]("select count(*) from " + name).unique

  def truncate: ConnectionIO[Unit] =
    Update0(s"truncate table $name cascade", None).execute
}

object DbTable {
  implicit def univEq: UnivEq[DbTable] = UnivEq.derive

  def all(schema: Option[String]): ConnectionIO[Set[DbTable]] = {
    val s = schema.getOrElse("public")
    sql"select tablename from pg_tables where schemaname = $s"
      .query[String]
      .to[Iterator]
      .map(_
        .filterNot(_.contains("flyway"))
        .map(apply)
        .toSet)
  }

  final case class Counts(asMap: Map[DbTable, Int]) {
    def apply(t: DbTable): Int =
      asMap.getOrElse(t, 0)

    def isEmpty: Boolean =
      asMap.values.forall(_ ==* 0)

    def nonEmpty: Boolean =
      !isEmpty

    def +(other: Counts): Counts =
      Counts(asMap.map {case (t, n) => (t, n - other(t))})

    def -(before: Counts): Counts =
      Counts(asMap.map {case (t, n) => (t, n - before(t))})

    def toTable: String =
      AsciiTable(
        List("TABLE", "ROWS" ) ::
        MutableArray(asMap.iterator.map(r => r._1.name :: r._2.toString :: Nil)).sortBy(_.head).iterator.toList)
  }

  def countAll(tables: IterableOnce[DbTable]): ConnectionIO[Counts] =
    Query0[(String, Int)](
      tables
        .iterator
        .map(t => s"select '${t.name}',count(1) from ${t.name}")
        .mkString(" union ")
    ).to[Iterator].map { it =>
      val m = it.map { case (t, n) => apply(t) -> n }.toMap
      Counts(m)
    }

  def truncateAll(tables: IterableOnce[DbTable]): ConnectionIO[Unit] =
    tables.iterator.map(_.truncate).toList.sequence.void
}
