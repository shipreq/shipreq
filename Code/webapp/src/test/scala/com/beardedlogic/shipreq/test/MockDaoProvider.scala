package com.beardedlogic.shipreq
package test

import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import slick.session.Session
import app.DI
import db.{AdminDao, DaoS, DaoT, DaoProvider}

/**
 * [[com.beardedlogic.shipreq.db.DaoProvider]] that creates and uses a mock DAO.
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
  val session = mock[Session]
  val dao = mock[DaoT]
  val adminDao = mock[AdminDao]
  for (d <- List(dao, adminDao)) when(d.session).thenReturn(session)

  override protected def createSession(): DaoS = dao
  override def withSession[T](block: DaoS => T): T = block(dao)
  override def withTransaction[T](block: DaoT => T): T = block(dao)
  override def withAdminDao[T](block: AdminDao => T): T = block(adminDao)

  def install[R](fn: => R): R = DI.DaoProvider.doWith(this)(fn)
}

object MockDaoProvider {
  def apply(setup: (DaoT => Unit) = identity) = {
    val p = new MockDaoProvider
    setup(p.dao)
    p
  }
}
