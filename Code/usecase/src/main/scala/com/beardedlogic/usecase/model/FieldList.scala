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
  def data = Data(dataId, DataType.FieldList)
  def dataId = value.dataId
  val fieldDefs = fieldKeys.map(_.fieldDef)
}

object FieldList {

  def createWithNewData(fields: List[FieldDef], idOpt: Option[Long] = None)(implicit s: Session) = {
    val data = Data.create(DataType.FieldList, idOpt)
    create(data, fields, ExactRev(1), false)
  }

  def create(
    data: Data[DataType.FieldList],
    fields: List[FieldDef],
    rev: Revision = LatestRev,
    reuseFieldKeys: Boolean = true)(implicit s: Session): FieldList = {

    val value = Value.create(data, rev)

    var fieldKeys = List.empty[FieldKey]
    var index = 0
    for (f <- fields) {
      val fieldKey = FieldKey.create(f.fieldKeyType, f.fieldKeyData, reuseFieldKeys)
      Relation.fieldList_has_fieldKey(value, index.toShort, fieldKey)
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

  /**
   * Ensures that a data & value exist in the DB that matches the given field list, and that it is the latest revision.
   *
   * @param id The data ID for the field list.
   * @param fields The field list to save.
   */
  def ensureSavedAndLatest(id: Long, fields: List[FieldDef])
    (implicit s: Session): FieldList = s.withTransaction {
    val dataOp = Data.find(id, DataType.FieldList)
    val latestOp = dataOp.flatMap(find(_, LatestRev))

    (dataOp, latestOp) match {
      case (None, _)                                       => FieldList.createWithNewData(fields, Some(id))
      case (_, Some(latest)) if latest.fieldDefs == fields => latest
      case (Some(data), _)                                 => create(data, fields)
    }
  }
}
