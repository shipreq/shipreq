package shipreq.webapp
package app

import db.{AsyncDbImpl, AsyncDb, DB, DaoProvider}
import lib.{StatLoggerImpl, StatLogger}
import net.liftweb.util.SimpleInjector
import net.liftweb.util.{Mailer => LiftMailer}
import security.{SecurityProvider, Oshiro}

/**
 * Houses and provides access to global resources. Not exactly "dependency injection" but serves a similar enough
 * purpose.
 *
 * The big bonus with that using the `doWith` methods, resources defined here can be manipulated by tests.
 */
object DI extends SimpleInjector {

  final val DaoProvider = new Inject[DaoProvider](DB.DaoProvider) {}

  final val SecurityProvider = new Inject[SecurityProvider](Oshiro) {}

  final val StatLogger = new Inject[StatLogger](StatLoggerImpl) {}

  final val Mailer = new Inject[LiftMailer](LiftMailer) {}

  final val AsyncDb = new Inject[AsyncDb](AsyncDbImpl) {}
}

/**
 * Mixes in accessors to DI resources.
 */
trait DI {
  def daoProvider = DI.DaoProvider.vend
  def securityProvider = DI.SecurityProvider.vend
  def statLogger = DI.StatLogger.vend
  def mailer = DI.Mailer.vend
  def asyncDb = DI.AsyncDb.vend
}