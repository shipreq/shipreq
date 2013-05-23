package com.beardedlogic.usecase
package model

import lib.field._
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}

/**
 * Corresponds to the data type [[com.beardedlogic.usecase.model.DataType.FieldList]], which basically boils down to a
 * single `List[FieldDef]`s.
 */
case class FieldList(value: PlainValue[DataType.FieldList], fieldKeys: List[FieldKey])
  extends Value[DataType.FieldList] {
  def valueId = value.valueId
  def data = Data(value.dataId, DataType.FieldList)
  val fieldDefs = fieldKeys.map(_.fieldDef)
}

object FieldList {

  def createWithNewData(fields: List[FieldDef], idOpt: Option[Long] = None)(implicit s: Session) = {
    val data = Data.create(DataType.FieldList, idOpt)
    create(data, 1, fields)
  }

  def create(data: Data[DataType.FieldList], rev: Int, fields: List[FieldDef])
    (implicit s: Session): FieldList = s.withTransaction {

    val value = Value.create(data, rev)

    var fieldKeys = List.empty[FieldKey]
    var index = 0
    for (f <- fields) {
      val fieldKey = FieldKey.createWithNewData(f.fieldKeyType, f.fieldKeyData)
      Relation.create(value, RelationType.Has, index.toShort, fieldKey)
      fieldKeys :+= fieldKey
      index += 1
    }

    FieldList(value, fieldKeys)
  }

  def find(data: Data[DataType.FieldList], rev: Revision)(implicit s: Session): Option[FieldList] = {
    Value.find(data, rev).map { value =>
      val fieldKeys = FieldKey.listByFieldList(value)
      FieldList(value, fieldKeys)
    }
  }
}
