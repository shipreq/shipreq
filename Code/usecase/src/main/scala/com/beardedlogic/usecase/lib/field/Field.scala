package com.beardedlogic.usecase
package lib
package field

import scala.xml.NodeSeq
import model._
import FieldKey.FieldKeyData
import FieldValue.FieldValueData

/**
 * @tparam S Field State type.
 */
trait FieldDef[S] {

  def newFieldInstance(ucCtx: UseCaseCtx, fieldKey: FieldKey): Field[S]

  def fieldKeyType: FieldKeyType

  /**
   * The arbitrary data stored in the database that comprises this field key's state.
   */
  def fieldKeyData: FieldKeyData

  def stateLoader(fieldKey: FieldKey): FieldStateLoader[S]
}

/**
 * Stateful instance of a Use Case Editor field (or fields).
 *
 * @tparam S Field State type.
 */
trait Field[S] {

  val ucCtx: UseCaseCtx

  val fieldKey: FieldKey

  @inline final def msgCentre = ucCtx.msgCentre

  /**
   * Called once after all fields have been created. Invocation is synchronous and must complete before the first
   * render is performed.
   */
  def init(): Unit

  def render(): NodeSeq

  /**
   * Restores internal state to a previous state. Usually called when loading from DB.
   *
   * @return A function to be invoked after all fields have had their states similarly set.
   */
  def setState(newState: S): () => Unit

  // TODO update doco here

  /**
   * Gives a field a chance to opt-out of storing a value in the database.
   * If a field is blank, then there's no point saving it.
   */
  def save_? : Boolean

  /**
   * Saves `Data` and `Value` rows for any additional data required.
   */
  def presave(
    lastSave: Option[(FieldSaveCtx, S)],
    saveCtx: MutableFieldSaveCtx,
    dao: DAO
  ): Boolean


  /**
   * Continues saving state to database.
   *
   * Once this is called, the `Data` and `Value` rows for all fields will have been saved, the IDs known.
   *
   * @return A single, arbitrary data string that will be stored in `field_value.data`. The format and mechanism of this
   *         value can be decided by the field type.
   */
  def save(
    combinedSaveCtx: FieldSaveCtx,
    newSaveCtx: FieldSaveCtx,
    dao: DAO
  ): (FieldValueData, S)
}