package com.beardedlogic.usecase.lib

import net.liftweb.util.SimpleInjector
import com.beardedlogic.usecase.db.{DB, DaoProvider, Dao}

// TODO Use DI for Mailer testing

/**
 * Houses and provides access to global resources. Not exactly "dependency injection" but serves a similar enough
 * purpose.
 *
 * The big bonus with that using the `doWith` methods, resources defined here can be manipulated by tests.
 */
object DI extends SimpleInjector {

  final val DaoProvider = new Inject[DaoProvider](DB.DaoProvider) with DaoProvider {
    override def get = vend.get
    override def withSession[T](block: Dao => T): T = vend.withSession(block)
    override def withTransaction[T](block: Dao => T): T = vend.withTransaction(block)
  }
}

/**
 * Mixes in accessors to DI resources.
 */
trait DI {
  def daoProvider = DI.DaoProvider.vend
}