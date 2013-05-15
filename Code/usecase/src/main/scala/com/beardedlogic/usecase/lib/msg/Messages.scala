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

}