package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib.db.DBHelpers._
import FieldValue.FieldValueData
import lib.TypeTags._

case class FieldValue(
  valueId: Long,
  fieldKeyId: Long,
  fieldData: FieldValueData
  ) extends Value[DataType.FieldValue] {

  def fieldKeyIdTagged = tag[FieldKeyId](fieldKeyId)
}

class FieldValueFull(
  valueId: Long,
  dataId: Long,
  rev: Short,
  val fieldKeyId: Long,
  val fieldData: FieldValueData
  ) extends PlainValue[DataType.FieldValue](valueId, dataId, rev) {

  def fieldKeyIdTagged = tag[FieldKeyId](fieldKeyId)
}

object FieldValue {
  type FieldValueData = Option[String]
}

// ---------------------------------------------------------------------------------------------------------------------

class MutableFieldSaveCtx {
  //  val fieldValues = MutableMap.empty[FieldKey, PlainValue[DataType.FieldValue]]
  //  val stepValues = MutableMap.empty[LocalIdStr, PlainValue[DataType.Step]]
  val fieldValues = Map.newBuilder[FieldKey, PlainValue[DataType.FieldValue]]
  val stepValues = Map.newBuilder[LocalIdStr, PlainValue[DataType.Step]]
  def immutable = new FieldSaveCtx(fieldValues.result, stepValues.result)
}

case class FieldSaveCtx(
  val fieldValues: Map[FieldKey, PlainValue[DataType.FieldValue]],
  val stepValues: Map[LocalIdStr, PlainValue[DataType.Step]]
  ) {

  /**
   * Merges this and another to form a new instance with all values combined.
   * In the case of conflicts, `this` will override `that`.
   */
  def combineWith(that: FieldSaveCtx) = FieldSaveCtx(
    that.fieldValues ++ this.fieldValues,
    that.stepValues ++ this.stepValues
  )
}

case class FieldLoadCtx(
  val fieldValues: Map[Long_FieldKeyId, FieldValueFull],
  /** For each relation type, a map of from-IDs to to-IDs (in the order specified in the `index` column). */
  val relations: Map[RelationType, Map[Long, List[Long]]],
  val stepData: Map[Long_StepValueId, (PlainValue[DataType.Step], String)]
  )

// ---------------------------------------------------------------------------------------------------------------------

object FieldValueAccessor {

  implicit val GetResultFieldValue = GetResult(r => FieldValue(r.<<, r.<<, r.<<))
  implicit val GetResultFieldValueFull = GetResult(r => new FieldValueFull(r.<<, r.<<, r.<<, r.<<, r.<<))

  implicit object SetParameterFieldValue extends SetParameter[FieldValue] {
    def apply(v: FieldValue, pp: PositionedParameters) {
      pp.setLong(v.valueId)
      pp.setLong(v.fieldKeyId)
      pp.setStringOption(v.fieldData)
    }
  }

  val Insert = Q.update[FieldValue]("INSERT INTO field_value VALUES(?,?,?)")

  val SelectByOwner = Q.query[Long, FieldValueFull]( s"""
      select fv.id, v.data_id, v.rev, fv.field_key_id, fv.text
      from field_value fv, relation r, value v
      where fv.id = r.to_id
      and r.type_id = ${RelationType.Has.ordinal}
      and r.from_id = ?
      """.sql)
}

trait FieldValueAccessor extends DatabaseAccessor {
  self: DAO =>

  import FieldValueAccessor._

  def createFieldValue(value: Value[DataType.FieldValue], fieldKey: FieldKey, data: FieldValueData) = {
    val fv = FieldValue(value.valueId, fieldKey.valueId, data)
    Insert.execute(fv)
    fv
  }

  def getFieldLoadCtxFor(ownerId: Long): FieldLoadCtx = {

    // Load field values
    var fieldValues = Map.empty[Long_FieldKeyId, FieldValueFull]
    SelectByOwner.foreach(ownerId, { fv =>
      fieldValues += (fv.fieldKeyIdTagged -> fv)
    })

    // Load relations and extended field value data (ie. steps)
    val (hasRelations, stepData) =
      withTmpTableOfRecursiveHasRelations(ownerId, { case (tmpTable, hasRelations) =>
        val stepData = findAllStepValuesAndText(s"WHERE s.id IN (SELECT to_id FROM $tmpTable)")
        (hasRelations, stepData)
      })
    val relations = Map((RelationType.Has: RelationType) -> hasRelations)

    // Bundle
    new FieldLoadCtx(fieldValues, relations, stepData)
  }
}