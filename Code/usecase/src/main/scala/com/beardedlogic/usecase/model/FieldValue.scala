package com.beardedlogic.usecase
package model

import scala.collection.mutable.{Map => MutableMap}
import scala.slick.jdbc.{StaticQuery => Q}
import lib.field.Field
import FieldValue.FieldValueData

case class FieldValue(
  valueId: Long,
  fieldKey: FieldKey,
  fieldData: FieldValueData
  ) extends Value[DataType.FieldValue]

object FieldValue {
  type FieldValueData = Option[String]
}

object FieldValueAccessor {
  val Insert = Q.update[(Long, Long, FieldValueData)]("INSERT INTO field_value VALUES(?,?,?)")
}

trait FieldValueAccessor extends DatabaseAccessor {
  self: DAO =>

  import FieldValueAccessor._

  def createFieldValue(fields: List[Field]): List[FieldValue] = db.withTransaction {
    val saveCtx = new FieldSaveCtx(this)

    // Pre-Save (data & value tables)
    for (field <- fields if field.save_?) {
      val value = createValueWithNewData(DataType.FieldValue)
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

class FieldSaveCtx(val db: DAO) {

  /**
   * `Value` instances for all fields that will be saved in the current transaction.
   */
  val fieldValues = MutableMap.empty[Field, Value[DataType.FieldValue]]

  /**
   * Key is the step node ID.
   */
  val stepValues = MutableMap.empty[String, Value[DataType.Step]]
}