package com.beardedlogic.usecase.lib.msg

import net.liftweb.actor.LiftActor
import net.liftweb.http.{CometActor, AddAListener, ListenerManager}
import net.liftweb.common.SimpleActor

/**
 * A simple hub for message traffic that sits between publishers and subscribers, thus decoupling them.
 *
 * This is initially disabled, during which messages are discarded rather than broadcast.
 *
 * Also baked-in, is a special feature that routes all CometMsgs to a provided CometActor. This could have easily be
 * done with existing functionality but the volume of CometMsgs and the assurance that only one listener will be
 * interested warranted the minor performance enhancement.
 *
 * @since 11/05/2013
 */
class MessageCentre(val cometActor: CometActor) extends LiftActor {
  type Subscriber = SimpleActor[Any]

  private[this] var subscribers: List[Subscriber] = Nil
  @volatile var enabled: Boolean = false

  /**
   * Simply routes received messages to a subscribed listeners.
   */
  override def messageHandler = {
    case msg if enabled => broadcast(msg)
  }

  @inline final protected def broadcast(msg: Any) {
    subscribers foreach (_ ! msg)
  }

  /**
   * Registers an actor so that it receives a copy of all messages that pass through.
   */
  def register(subscriber: Subscriber) {
    subscribers ::= subscriber
  }

  /**
   * Removes a subscribed actor so that it no longer receives messages from here.
   */
  def unregister(subscriber: Subscriber) {
    subscribers = subscribers.filter(_ ne subscriber)
  }

  @inline final def !(msg: CometMsg): Unit = {
    cometActor ! msg
  }
}
