package com.beardedlogic.usecase
package model

import lib.field._

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

trait FieldListAccessor extends DatabaseAccessor {
  self:DataAccessor with ValueAccessor with RelationAccessor with FieldKeyAccessor =>

  def createInitialFieldList(fields: List[FieldDef], idOpt: Option[Long] = None) = db.withTransaction {
    val data = createData(DataType.FieldList, idOpt)
    createFieldList(data, fields, ExactRev(1))
  }

  def createFieldList(data: Data[DataType.FieldList], fields: List[FieldDef], rev: Revision = LatestRev): FieldList = db.withTransaction {
    val value = createValue(data, rev)

    var fieldKeys = List.empty[FieldKey]
    var index = 0
    for (f <- fields) {
      val fieldKey = findOrCreateInitialFieldKey(f.fieldKeyType, f.fieldKeyData)
      relate_fieldList_has_fieldKey(value, index.toShort, fieldKey)
      fieldKeys :+= fieldKey
      index += 1
    }

    FieldList(value, fieldKeys)
  }

  def findFieldList(data: Data[DataType.FieldList], rev: Revision): Option[FieldList] = {
    findValue(data, rev).map { value =>
      val fieldKeys = findAllFieldKeysByFieldList(value)
      FieldList(value, fieldKeys)
    }
  }

  /**
   * Ensures that a data & value exist in the DB that matches the given field list, and that it is the latest revision.
   *
   * @param id The data ID for the field list.
   * @param fields The field list to save.
   */
  def syncFieldList(id: Long, fields: List[FieldDef]): FieldList = db.withTransaction {
    val dataOp = findData(id, DataType.FieldList)
    val latestOp = dataOp.flatMap(findFieldList(_, LatestRev))

    (dataOp, latestOp) match {
      case (None, _)                                       => createInitialFieldList(fields, Some(id))
      case (_, Some(latest)) if latest.fieldDefs == fields => latest
      case (Some(data), _)                                 => createFieldList(data, fields)
    }
  }
}
