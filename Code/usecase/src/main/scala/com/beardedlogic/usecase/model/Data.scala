package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}
import lib.db._
import DBHelpers._

case class Data[T <: DataType](id: Long, dataType: T)

object Data extends DBTable {
  override val TableName = "data"

  def create[T <: DataType](dataType: T, idOpt: Option[Long] = None)(implicit s: Session): Data[T] = idOpt match {
    case Some(id) =>
      Q.update[(Long, Short)](s"INSERT INTO $TableName(id, type_id) VALUES(?,?)").execute(id, dataType)
      Data(id, dataType)

    case None =>
      val id = Q.query[Short, Int](s"INSERT INTO $TableName(type_id) VALUES(?) RETURNING id").first(dataType)
      Data(id, dataType)
  }

  def find(id: Long)(implicit s: Session): Option[Data[_ <: DataType]] = {
    Q.query[Long, Short](s"SELECT type_id FROM $TableName WHERE id=?")
    .firstOption(id)
    .map(Data(id, _))
  }

  def find[T <: DataType](id: Long, dataType: T)(implicit s: Session): Option[Data[T]] = {
    Q.query[(Long, Short), Short](s"SELECT id FROM $TableName WHERE id=? AND type_id=?")
    .firstOption(id, dataType)
    .map(Data(_, dataType))
  }
}
