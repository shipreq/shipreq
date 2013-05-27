package com.beardedlogic.usecase
package lib
package field

import scala.xml.NodeSeq
import model.{FieldKey, FieldValue, FieldKeyType, FieldLoadCtx, FieldSaveCtx}
import FieldKey.FieldKeyData
import FieldValue.FieldValueData

trait FieldDef {

  def newFieldInstance(state: UCEditorState, fieldKey: FieldKey): Field

  def fieldKeyType: FieldKeyType

  /**
   * The arbitrary data stored in the database that comprises this field key's state.
   */
  def fieldKeyData: FieldKeyData
}

/**
 * Stateful instance of a Use Case Editor field (or fields).
 */
trait Field {

  val state: UCEditorState

  val fieldKey: FieldKey

  @inline final def msgCentre = state.msgCentre

  /**
   * Called once after all fields have been created. Invocation is synchronous and must complete before the first
   * render is performed.
   */
  def init(): Unit

  def render(): NodeSeq

  /**
   * Sets this object's state to a previously saved state, as provided by the load context.
   *
   * @param ctx A big blob of data for all fields, from which this field should find and use its own data.
   */
  def load(ctx: FieldLoadCtx): Unit

  /**
   * Gives a field a chance to opt-out of storing a value in the database.
   * If a field is blank, then there's no point saving it.
   */
  def save_? : Boolean

  /**
   * Saves `Data` and `Value` rows for any additional data required.
   */
  def presave(ctx: FieldSaveCtx): Unit

  /**
   * Continues saving state to database.
   *
   * Once this is called, the `Data` and `Value` rows for all fields will have been saved, the IDs known.
   *
   * @return A single, arbitrary data string that will be stored in `field_value.data`. The format and mechanism of this
   *         value can be decided by the field type.
   */
  def save(ctx: FieldSaveCtx): FieldValueData
}
