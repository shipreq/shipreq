package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{StaticQuery => Q, GetResult}
import lib.db._
import DBHelpers._

/**
 * Anything that has a value ID.
 *
 * @tparam T The type of value. Derivable by `value` -> `data` -> `data_type`.
 */
trait Value[T <: DataType] {
  def valueId: Long
}

/**
 * Representation of the `value` table.
 *
 * @tparam T The type of value. Derivable by `value` -> `data` -> `data_type`.
 */
case class PlainValue[T <: DataType](valueId: Long, dataId: Long, rev: Int) extends Value[T]

sealed trait Revision {def querySuffix: String}

case object LatestRev extends Revision {override val querySuffix = "ORDER BY rev DESC LIMIT 1"}

case class ExactRev(rev: Int) extends Revision {override def querySuffix = s"AND rev = $rev"}

// ---------------------------------------------------------------------------------------------------------------------

class LatestRevSubquery {
  private var whereClause = ""
  def where(cond: String) = { whereClause = s" WHERE $cond"; this }
  def toSql = s"SELECT id, row_number() OVER (PARTITION BY data_id ORDER BY rev DESC) rn FROM value${whereClause}"
  private var _withTableAlias = ""
  def withTableAlias = {if (_withTableAlias.isEmpty) throw new IllegalStateException("WITH alias not provided yet."); else _withTableAlias }
  def withTableAlias(tableAlias: String) = {_withTableAlias = tableAlias; this }
  def toWithClause = s"WITH $withTableAlias AS ($toSql)"
  def applyWithTableAsValueIdFilter(valueIdExpr: String) = s"$valueIdExpr in (select id from $withTableAlias where rn = 1)"
}

object ValueAccessor {

  val * = "id, data_id, rev"

  def GetValueResult[T <: DataType] = GetResult { r => PlainValue[T](r.<<, r.<<, r.<<) }

  implicit val GetResultPlainValue = GetValueResult[DataType]

  val InsertWithExactRev = Q.query[(Long, Int), Long]("INSERT INTO value(data_id, rev) VALUES(?,?) RETURNING id")
  val InsertWithLatestRev = Q.query[(Long, Long), (Long, Int)]( """
                              insert into value(data_id,rev)
                              select ?,coalesce(max(rev)+1,1) from value where data_id=?
                              returning id, rev """.sql)
}

trait ValueAccessor extends DatabaseAccessor {
  self: DataAccessor =>

  import ValueAccessor._

  def createInitialValue[T <: DataType](dataType: T): PlainValue[T] =
    createValue(createData(dataType), ExactRev(1))

  def createValue[T <: DataType](data: Data[T], rev: Revision): PlainValue[T] =
    createValueUnchecked(data.id, rev).asInstanceOf[PlainValue[T]]

  def createValue[T <: DataType](value: PlainValue[T], rev: Revision): PlainValue[T] =
    createValueUnchecked(value.dataId, rev).asInstanceOf[PlainValue[T]]

  def createValueUnchecked(dataId: Long, rev: Revision): PlainValue[_ <: DataType] = rev match {
    case ExactRev(revNum) =>
      val newId = InsertWithExactRev.first(dataId, revNum)
      PlainValue(newId, dataId, revNum)

    case LatestRev =>
      val (newId, newRev) = InsertWithLatestRev.first(dataId, dataId)
      PlainValue(newId, dataId, newRev)
  }

  def findValue[T <: DataType](data: Data[T], rev: Revision): Option[PlainValue[T]] = {
    Q.query[Long, PlainValue[DataType]](s"select ${*} from value where data_id=? ${rev.querySuffix}")
    .firstOption(data.id)
    .asInstanceOf[Option[PlainValue[T]]]
  }
}
