package com.beardedlogic.usecase.lib
package field

import com.beardedlogic.usecase.model._
import change.ChangeResponder
import Types._

trait FieldDefinition {

  /** The type (enum) of this field. */
  val fieldKeyType: FieldKeyType

  /** Arbitrary data (to store in the database) that comprises this field key's state. */
  val fieldKeyData: FieldKeyRecData

  def field(rec: FieldKeyRec): Field
}

/**
 * Represents a field that a use case can have. Eg. "Frequency of Use", "Exception Courses"
 *
 * This does not include the value of the field.
 */
trait Field extends UcChangeDomain {

  /** The type of this field's values. */
  type Value <: ChangeResponder[Value]

  /** The type of data that encapsulates all records saved by this field. */
  type SavedData

  val defn: FieldDefinition

  /** The DB record used to reference this field. */
  val rec: FieldKeyRec

  @inline final def castValue(v: Field#Value) = v.asInstanceOf[Value]

  @inline final def castSavedData(s: Field#SavedData) = s.asInstanceOf[SavedData]

  @inline final def saver(savers: Map[Field, FieldValueSaver[_]]) = savers(this).asInstanceOf[FieldValueSaver[SavedData]]

  @inline final def apply(fieldValues: FieldValues): Value = castValue(fieldValues(this))

  @inline final def get(fieldValues: FieldValues): Option[Value] = fieldValues.get(this).asInstanceOf[Option[Value]]

  @inline final def ~>(fieldValue: Value): (Field, Field#Value) = this -> fieldValue

  @inline final def value(implicit fieldValues: FieldValues) = apply(fieldValues)

  def empty: Value

  /**
   * Loads a field value from the database.
   *
   * @param loadCtx A big blob of data for all fields, from which this field should find and use its own data.
   */
  def load(loadCtx: FieldLoadCtx): FieldLoadResult[Value, SavedData]

  def valueSaver(v: Value, stepsAndLabels: StepAndLabelBiMap): FieldValueSaver[SavedData]
}

trait FieldValueSaver[SavedData] {

  /**
   * Gives a field a chance to opt-out of storing a value in the database.
   * If a field is blank, then there's no point saving it.
   *
   * Note: This is ignored if the field was saved previously. To do otherwise would be to lose audit trail.
   */
  def record_required_? : Boolean

  /**
   * Compares the current field value to the previous saved data.
   *
   * @return Whether the field value has changed.
   */
  def differsFromPrevSave_?(prev: SavedData)(implicit savedSteps: SavedSteps): Boolean

  /**
   * Creates identity rows (`text.id`) for steps.
   *
   * @return A map of new saved steps.
   */
  def presave(dao: DAO, ucId: UseCaseIdentId, prevSavedSteps: Option[SavedSteps]): Map[LocalIdStr, TextIdentId]

  /**
   * Saves field value(s) to the database and links them to the provided UC.
   *
   * @return Data that will be passed back in on subsequent saves to facilitate data reuse (in the DB).
   */
  def save(dao: DAO, ucId: UseCaseIdentId, ucRevId: UseCaseRevId, prevSave: Option[SavedData])(implicit savedSteps: SavedSteps): SavedData
}
