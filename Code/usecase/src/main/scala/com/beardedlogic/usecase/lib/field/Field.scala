package com.beardedlogic.usecase
package lib
package field

import scala.xml.NodeSeq
import model.{FieldLoadCtx, FieldKey, FieldKeyType}
import FieldKey.FieldKeyData

trait FieldDef {

  def newFieldInstance(state: UCEditorState, fieldKey: FieldKey): Field[_]

  def fieldKeyType: FieldKeyType

  /**
   * The arbitrary data stored in the database that comprises this field key's state.
   */
  def fieldKeyData: FieldKeyData
}

/**
 * Stateful instance of a Use Case Editor field (or fields).
 *
 * @tparam S Type of the field state object.
 */
trait Field[S] {

  val uceState: UCEditorState

  val fieldKey: FieldKey

  @inline final def msgCentre = uceState.msgCentre

  /**
   * Called once after all fields have been created. Invocation is synchronous and must complete before the first
   * render is performed.
   */
  def init(): Unit

  def render(): NodeSeq

  def state: S
  def state_=(newState: S): Unit
  def stateDao: FieldStateMiniDao[S]

  // TODO remove?
  def load(ctx: FieldLoadCtx) { state = stateDao.load(ctx) }
}