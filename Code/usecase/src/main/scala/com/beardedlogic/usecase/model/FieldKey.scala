package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q, GetResult}
import lib.db._
import DBHelpers._
import FieldKey.FieldKeyData

case class FieldKey(value: PlainValue[DataType.FieldKey.type], fieldKeyType: FieldKeyType, data: FieldKeyData)
  extends ValueExt[DataType.FieldKey.type]

object FieldKey extends DBTable {
  override val TableName = "field_key"

  type FieldKeyData = Option[String]

  val CreateWithNewData = Q.update[(Long, Short, FieldKeyData)](s"INSERT INTO $TableName(id, type_id, data) VALUES(?,?,?)")

  def createWithNewData(fieldKeyType: FieldKeyType, data: FieldKeyData)(implicit s: Session) = {
    val value = Value.createWithNewData(DataType.FieldKey)
    CreateWithNewData.first(value.id, fieldKeyType, data)
    FieldKey(value, fieldKeyType, data)
  }

  val SelectByFieldList = Q.query[(Long, Short), (FieldKeyType, FieldKeyData)]( """
      SELECT fk.type_id, fk.data
      FROM field_key fk, relation r
      WHERE fk.id = r.to_id
        AND r.from_id = ?
        AND r.type_id = ?
      ORDER BY r.index """.sql)

  def selectByFieldList(fieldListId: Long)(implicit s: Session): List[(FieldKeyType, FieldKeyData)] =
    SelectByFieldList.list(fieldListId, RelationType.Has)
}

