package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}
import lib.db._
import DBHelpers._

case class Data[T <: DataType](id: Long, dataType: T)

trait DataAccessor extends DatabaseAccessor {

  def createData[T <: DataType](dataType: T, idOpt: Option[Long] = None): Data[T] = idOpt match {
    case Some(id) =>
      Q.update[(Long, Short)]("INSERT INTO data(id, type_id) VALUES(?,?)").execute(id, dataType)
      Data(id, dataType)

    case None =>
      val id = Q.query[Short, Int]("INSERT INTO data(type_id) VALUES(?) RETURNING id").first(dataType)
      Data(id, dataType)
  }

  def findData(id: Long): Option[Data[_ <: DataType]] = {
    Q.query[Long, Short]("SELECT type_id FROM data WHERE id=?")
    .firstOption(id)
    .map(Data(id, _))
  }

  def findData[T <: DataType](id: Long, dataType: T): Option[Data[T]] = {
    Q.query[(Long, Short), Short]("SELECT id FROM data WHERE id=? AND type_id=?")
    .firstOption(id, dataType)
    .map(Data(_, dataType))
  }
}
