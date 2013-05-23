package com.beardedlogic.usecase
package lib
package field

import scala.xml.NodeSeq
import model.FieldKeyType
import model.FieldKey.FieldKeyData

trait FieldDef {

  def newFieldInstance(state: UCEditorState): Field

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

  @inline final def msgCentre = state.msgCentre

  /**
   * Called once after all fields have been created. Invocation is synchronous and must complete before the first
   * render is performed.
   */
  def init() : Unit

  def render(): NodeSeq
}