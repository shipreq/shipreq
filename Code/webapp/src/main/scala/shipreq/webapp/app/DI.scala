package shipreq.webapp
package app

import net.liftweb.util.SimpleInjector
import scala.slick.jdbc.JdbcBackend.Session
import security.{SecurityProvider, Oshiro}
import db.{AsyncDbImpl, AsyncDb, DB, DaoProvider}
import lib.{TaskmanImpl, TaskmanInterface, StatLoggerImpl, StatLogger}

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

  final val Taskman = new Inject[TaskmanInterface](TaskmanImpl) {}

  final val AsyncDb = new Inject[AsyncDb](AsyncDbImpl) {}
}

/**
 * Mixes in accessors to DI resources.
 */
trait DI {
  final def daoProvider = DI.DaoProvider.vend
  final def securityProvider = DI.SecurityProvider.vend
  final def statLogger = DI.StatLogger.vend
  final def taskman = DI.Taskman.vend
  final def asyncDb = DI.AsyncDb.vend

  /** One-shot taskman job. Uses a new DB connection. */
  final def taskman1[A](f: TaskmanInterface => Session => A): A =
    daoProvider.withRawSession(f(taskman))
}
