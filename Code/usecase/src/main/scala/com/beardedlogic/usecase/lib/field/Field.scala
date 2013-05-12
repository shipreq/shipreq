package com.beardedlogic.usecase.lib
package field

import scala.xml.NodeSeq
import net.liftweb.actor.LiftActor

trait FieldDef {
  def newFieldInstance(state: UCEditorState): Field
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