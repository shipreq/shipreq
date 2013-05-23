package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
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

object Value extends DBTable {
  override val TableName = "value"

  val * = "id, data_id, rev"
  implicit val GetResultPlainValue = GetResult { r => PlainValue[DataType](r.<<, r.<<, r.<<) }

  val InsertWithExactRev = Q.query[(Long, Int), Long](s"INSERT INTO $TableName(data_id, rev) VALUES(?,?) RETURNING id")
  val InsertWithLatestRev = Q.query[(Long, Long), (Long, Int)]( """
                              insert into value(data_id,rev)
                              select ?,coalesce(max(rev)+1,1) from value where data_id=?
                              returning id, rev """.sql)

  def createWithNewData[T <: DataType](dataType: T)(implicit s: Session): PlainValue[T] =
    create(Data.create(dataType), ExactRev(1))

  def create[T <: DataType](data: Data[T], rev: Revision)(implicit s: Session): PlainValue[T] = rev match {
    case ExactRev(revNum) =>
      val newId = InsertWithExactRev.first(data.id, revNum)
      PlainValue(newId, data.id, revNum)

    case LatestRev =>
      val (newId, newRev) = InsertWithLatestRev.first(data.id, data.id)
      PlainValue(newId, data.id, newRev)
  }

  def find[T <: DataType](data: Data[T], rev: Revision)(implicit s: Session): Option[PlainValue[T]] = {
    Q.query[Long, PlainValue[DataType]](s"select ${*} from $TableName where data_id=? ${rev.querySuffix}")
    .firstOption(data.id)
    .asInstanceOf[Option[PlainValue[T]]]
  }
}

sealed trait Revision {def querySuffix: String}

case object LatestRev extends Revision {override val querySuffix = "ORDER BY rev DESC LIMIT 1"}

case class ExactRev(rev: Int) extends Revision {override def querySuffix = s"AND rev = $rev"}

