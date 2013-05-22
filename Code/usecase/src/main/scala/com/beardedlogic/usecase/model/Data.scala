package com.beardedlogic.usecase.model

import scala.slick.driver.PostgresDriver.simple._

case class Data(id: Int,
                data_type: DataType)

object DataTable extends Table[Data]("data") {
  implicit val typeMapper = MappedTypeMapper.base[DataType, Int](_.ordinal, DataType(_))
  def id = column[Int]("id")
  def data_type = column[DataType]("type_id")
  def * = id ~ data_type <>(Data, Data.unapply _)

  def insert(dataType: DataType)(implicit s: Session): Int = data_type.returning(id).insert(dataType)

  val QueryByID = for {id <- Parameters[Int]; r <- this if r.id is id} yield r
  def apply(id: Int)(implicit s: Session): Data = QueryByID(id).first
}
