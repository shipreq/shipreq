package com.beardedlogic.usecase.lib.msg

import net.liftweb.http.js.JsCmd

/**
 * Marks a message that is meant for a CometActor only. (ie. rather than standard LiftActors.)
 */
trait CometMsg

/**
 * Push a Javascript command to the client.
 */
case class PushToClient(cmd: JsCmd) extends CometMsg