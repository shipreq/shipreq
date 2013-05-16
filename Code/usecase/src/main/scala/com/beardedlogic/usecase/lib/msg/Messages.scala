package com.beardedlogic.usecase.lib.msg

import net.liftweb.http.js.JsCmd

/** Marks a message that is meant for a [[net.liftweb.http.CometActor]] only. */
trait CometMessage

/**
 * Registry of available messages that actors can choose to respond to.
 *
 * @since 16/05/2013
 */
object Messages {

  /** Push a Javascript command to the client. */
  case class PushToClient(cmd: JsCmd) extends CometMessage

  /** Indicates that one or more steps have changed. */
  case object StepChangeMsg

  /**
   * Indicates that a step's flow-from list has changed.
   *
   * Example: If the text of step 1.7 changes from `"Blah"` or `"Blah ⬅ 1.0.2"` to `"Blah ⬅ 1.3, 1.4"`
   * then this message will be broadcast:
   * {{{
   * FlowToChangeMsg( [1.3, 1.4], 1.7 )
   * }}}
   *
   * @param fromIds The IDs of all steps that now flow to the target.
   * @param toId The ID of the step that issued the change, the step to which the from-steps now flow.
   */
  case class FlowFromChangeMsg(fromIds: Set[String], toId: String)

  /**
   * Indicates that a step's flow-to list has changed.
   *
   * Example: If the text of step 1.7 changes from `"Blah"` or `"Blah ➡ 1.0.2"` to `"Blah ➡ 1.3, 1.4"`
   * then this message will be broadcast:
   * {{{
   * FlowToChangeMsg( 1.7, [1.3, 1.4] )
   * }}}
   *
   * @param fromId The ID of the step that issued the change, the step from which steps now flow out.
   * @param toIds The IDs of all steps that the source step now flows to.
   */
  case class FlowToChangeMsg(fromId: String, toIds: Set[String])
}