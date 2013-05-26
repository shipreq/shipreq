package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.db._
import DBHelpers._
import FieldKey.FieldKeyData

case class FieldKey(valueId: Long, fieldKeyType: FieldKeyType, fieldKeyData: FieldKeyData)
  extends Value[DataType.FieldKey] {
  def fieldDef = fieldKeyType.fieldDef(fieldKeyData)
}

object FieldKey {
  type FieldKeyData = Option[String]
}

object FieldKeyAccessor {

  implicit val GetResultFieldKey = GetResult { r => FieldKey(r.<<, r.<<, r.<<) }

  val SelectIdToReuse = Q.query[(Short, FieldKeyData), Long](
    "SELECT id FROM field_key WHERE type_id=? AND data IS NOT DISTINCT FROM ?")

  val Insert = Q.update[(Long, Short, FieldKeyData)](
    "INSERT INTO field_key(id, type_id, data) VALUES(?,?,?)")

  val SelectByFieldList = Q.query[(Long, Short), FieldKey]( """
      SELECT fk.id, fk.type_id, fk.data
      FROM field_key fk, relation r
      WHERE fk.id = r.to_id
        AND r.from_id = ?
        AND r.type_id = ?
      ORDER BY r.index """.sql)
}

trait FieldKeyAccessor extends DatabaseAccessor {
  self: ValueAccessor =>

  import FieldKeyAccessor._

  def createFieldKey(
    fieldKeyType: FieldKeyType,
    fieldKeyData: FieldKeyData,
    reuseFieldKeys: Boolean = true) = {

    if (reuseFieldKeys)
      SelectIdToReuse.firstOption(fieldKeyType, fieldKeyData)
      .map(FieldKey(_, fieldKeyType, fieldKeyData))
      .getOrElse(createFieldKeyWithNewData(fieldKeyType, fieldKeyData))
    else
      createFieldKeyWithNewData(fieldKeyType, fieldKeyData)
  }

  def createFieldKeyWithNewData(fieldKeyType: FieldKeyType, fieldKeyData: FieldKeyData) = {
    val fkv = createValueWithNewData(DataType.FieldKey)
    Insert.first(fkv.valueId, fieldKeyType, fieldKeyData)
    FieldKey(fkv.valueId, fieldKeyType, fieldKeyData)
  }

  def listFieldKeysByFieldList(fieldList: Value[DataType.FieldList]): List[FieldKey] =
    SelectByFieldList.list(fieldList.valueId, RelationType.Has)
}

