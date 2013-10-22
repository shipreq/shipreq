package com.beardedlogic.usecase
package feature.uc
package field

import db.FieldKeyRec
import lib.Types._
import change.ChangeResponder

/**
 * Represents a field that a use case can have. Eg. "Frequency of Use", "Exception Courses"
 *
 * This does not include the value of the field.
 */
sealed trait Field {

  /** The type of this field's values. */
  type Value

  /** The type of data that encapsulates all records saved by this field. */
  type SavedData

  val defn: FieldDefinition

  /** The DB record used to reference this field. */
  val rec: FieldKeyRec

  @inline final def castV(v: Field#Value) = v.asInstanceOf[Value]
  @inline final def castS(s: Field#SavedData) = s.asInstanceOf[SavedData]
  @inline final def castS2[M[_]](saver: M[Field#SavedData]): M[SavedData] = saver.asInstanceOf[M[SavedData]]

  @inline final def ~>(v: Value): (Field, Field#Value) = this -> v
  @inline final def pairS(sd: SavedData): (Field, Field#SavedData) = this -> sd
  @inline final def pairS2[M[_]](sd: M[SavedData]): (Field, M[Field#SavedData]) = this -> sd.asInstanceOf[M[Field#SavedData]]

  @inline final def apply(fieldValues: FieldValues): Value = castV(fieldValues(this))
  @inline final def get(fieldValues: FieldValues): Option[Value] = fieldValues.get(this).asInstanceOf[Option[Value]]
  @inline final def value(implicit fieldValues: FieldValues) = apply(fieldValues)

  def empty: Value

  def changeResponder(v: Value): ChangeResponder[Value]

  /**
   * Loads a field value from the database.
   *
   * @param loadCtx A big blob of data for all fields, from which this field should find and use its own data.
   */
  def load(loadCtx: FieldLoadCtx): FieldLoadResult[Value, SavedData]

  def saver(v: Value, stepsAndLabels: StepAndLabelBiMap): FieldValueSaver[SavedData]
}

// =====================================================================================================================
// Instances

case class TextField(override val defn: TextFieldDefinition, override val rec: FieldKeyRec) extends Field with TextFieldLike

sealed abstract class StepField extends Field with StepFieldLike
case class NormalCourseField(override val rec: FieldKeyRec) extends StepField with NormalCourseFieldLike
case class ExceptionCourseField(override val rec: FieldKeyRec) extends StepField with ExceptionCourseFieldLike
