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

  def defn: FieldDefinition

  /** The DB record used to reference this field. */
  val rec: FieldKeyRec

  final override def hashCode = rec.id.toInt
  final override def equals(o: Any) = o match {
    case f: Field => rec.id == f.rec.id
    case _        => false
  }

  @inline final def castV(v: Field#Value) = v.asInstanceOf[Value]
  @inline final def ~>(v: Value): (Field, Field#Value) = this -> v

  @inline final def apply(fieldValues: FieldValues): Value = castV(fieldValues(this))
  @inline final def get(fieldValues: FieldValues): Option[Value] = fieldValues.get(this).asInstanceOf[Option[Value]]
  @inline final def value(implicit fieldValues: FieldValues) = apply(fieldValues)

  def empty: Value

  def changeResponder: ChangeResponder[Value]
}

// =====================================================================================================================
// Instances

case class TextField(override val defn: TextFieldDefinition, override val rec: FieldKeyRec) extends Field with TextFieldLike

sealed abstract class StepField extends Field with StepFieldLike
case class NormalCourseField(override val rec: FieldKeyRec) extends StepField with NormalCourseFieldLike
case class ExceptionCourseField(override val rec: FieldKeyRec) extends StepField with ExceptionCourseFieldLike

case class FlowGraphField(override val rec: FieldKeyRec) extends Field with FlowGraphFieldLike
