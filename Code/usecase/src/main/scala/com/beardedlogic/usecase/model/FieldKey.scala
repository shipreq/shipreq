package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.db._
import DBHelpers._
import FieldKey.FieldKeyData

case class FieldKey(valueId: Long, fieldKeyType: FieldKeyType, fieldKeyData: FieldKeyData)
  extends Value[DataType.FieldKey] {
  def fieldDef = fieldKeyType.fieldDef(fieldKeyData)
}

object FieldKey extends DBTable {
  override val TableName = "field_key"

  type FieldKeyData = Option[String]

  implicit val GetResultFieldKey = GetResult { r => FieldKey(r.<<, r.<<, r.<<) }

  def createWithNewData(fieldKeyType: FieldKeyType, fieldKeyData: FieldKeyData)(implicit s: Session) = {
    val fkv = Value.createWithNewData(DataType.FieldKey)
    Q.update[(Long, Short, FieldKeyData)](s"INSERT INTO $TableName(id, type_id, data) VALUES(?,?,?)")
    .first(fkv.valueId, fieldKeyType, fieldKeyData)
    FieldKey(fkv.valueId, fieldKeyType, fieldKeyData)
  }

  val SelectByFieldList = Q.query[(Long, Short), FieldKey]( """
      SELECT fk.id, fk.type_id, fk.data
      FROM field_key fk, relation r
      WHERE fk.id = r.to_id
        AND r.from_id = ?
        AND r.type_id = ?
      ORDER BY r.index """.sql)

  def listByFieldList(fieldList: Value[DataType.FieldList])(implicit s: Session): List[FieldKey] =
    SelectByFieldList.list(fieldList.valueId, RelationType.Has)
}

