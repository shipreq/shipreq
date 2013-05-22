package com.beardedlogic.usecase.model

import scala.slick.driver.PostgresDriver.simple._

case class Data(id: Long,
                data_type: DataType)

object DataTable extends Table[Data]("data") {
  implicit val typeMapper = MappedTypeMapper.base[DataType, Short](_.ordinal, DataType(_))
  def id = column[Long]("id")
  def data_type = column[DataType]("type_id")
  def * = id ~ data_type <>(Data, Data.unapply _)

  def insert(dataType: DataType)(implicit s: Session) = {
    val newId = data_type.returning(id).insert(dataType)
    Data(newId, dataType)
  }

  val QueryByID = for {id <- Parameters[Long]; r <- this if r.id is id} yield r
  def apply(id: Long)(implicit s: Session): Data = QueryByID(id).first
}
