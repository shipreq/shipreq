package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.db._
import DBHelpers._
import com.beardedlogic.usecase.lib.field.Field
import scala.collection.mutable.{Map => MutableMap}

case class FieldValue(
  valueId: Long,
  fieldKey: FieldKey,
  fieldData: FieldValue.FieldValueData
  ) extends Value[DataType.FieldValue]

object FieldValue extends DBTable {

  override val TableName = "field_value"

  type FieldValueData = Option[String]

  private val Insert = Q.update[(Long, Long, FieldValueData)](
    s"INSERT INTO $TableName VALUES(?,?,?)")

  def createWithNewData(fields: List[Field])(implicit s: Session): List[FieldValue] = s.withTransaction {
    val saveCtx = new FieldSaveCtx

    // Pre-Save (data & value tables)
    for (field <- fields if field.save_?) {
      val value = Value.createWithNewData(DataType.FieldValue)
      saveCtx.fieldValues += (field -> value)
      field.presave(saveCtx)
    }

    // Save (value-ext & relation tables)
    var results = List.empty[FieldValue]
    for ((field, value) <- saveCtx.fieldValues) {
      val data = field.save(saveCtx)
      Insert.execute(value.valueId, field.fieldKey.valueId, data)
      results :+= FieldValue(value.valueId, field.fieldKey, data)
    }

    results
  }
}

class FieldSaveCtx {

  /**
   * `Value` instances for all fields that will be saved in the current transaction.
   */
  val fieldValues = MutableMap.empty[Field, Value[DataType.FieldValue]]

  /**
   * Key is the step node ID.
   */
  val stepValues = MutableMap.empty[String, Value[DataType.Step]]
}