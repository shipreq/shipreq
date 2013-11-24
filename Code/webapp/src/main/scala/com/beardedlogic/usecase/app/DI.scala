package com.beardedlogic.usecase
package app

import net.liftweb.util.SimpleInjector
import db.{DB, DaoProvider, DaoS, DaoT}
import security.{SecurityProvider, Oshiro}
import lib.{StatLoggerActor, StatLogger}

// TODO Use DI for Mailer testing

/**
 * Houses and provides access to global resources. Not exactly "dependency injection" but serves a similar enough
 * purpose.
 *
 * The big bonus with that using the `doWith` methods, resources defined here can be manipulated by tests.
 */
object DI extends SimpleInjector {

  final val DaoProvider = new Inject[DaoProvider](DB.DaoProvider) with DaoProvider {
    override def withSession[T](block: DaoS => T): T = vend.withSession(block)
    override def withTransaction[T](block: DaoT => T): T = vend.withTransaction(block)
  }

  final val SecurityProvider = new Inject[SecurityProvider](Oshiro) {}

  final val StatLogger = new Inject[StatLogger](StatLoggerActor) {}
}

/**
 * Mixes in accessors to DI resources.
 */
trait DI {
  def daoProvider = DI.DaoProvider.vend
  def securityProvider = DI.SecurityProvider.vend
  def statLogger = DI.StatLogger.vend
}