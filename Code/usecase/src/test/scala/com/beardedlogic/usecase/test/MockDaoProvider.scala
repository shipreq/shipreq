package com.beardedlogic.usecase
package test

import org.scalatest.mock.MockitoSugar
import com.beardedlogic.usecase.db.{DaoS, DaoT, DaoProvider}
import com.beardedlogic.usecase.lib.DI

/**
 * [[com.beardedlogic.usecase.db.DaoProvider]] that creates and uses a mock DAO.
 *
 * Usage:
 *
 * {{{
 *   MockDaoProvider(dao => when(dao.xxxx).thenReturn(xxx)).install {
 *     new MySnippet().render
 *   }
 * }}}
 */
class MockDaoProvider extends DaoProvider with MockitoSugar {
  val dao = mock[DaoT]
  override def withSession[T](block: DaoS => T): T = block(dao)
  override def withTransaction[T](block: DaoT => T): T = block(dao)

  def install[R](fn: => R): R = DI.DaoProvider.doWith(this)(fn)
}

object MockDaoProvider {
  def apply(setup: (DaoT => Unit) = identity) = {
    val p = new MockDaoProvider
    setup(p.dao)
    p
  }
}
