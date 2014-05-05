package shipreq.webapp
package test

import java.sql.Connection
import org.mockito.Mockito.when
import org.scalatest.mock.MockitoSugar
import scala.slick.jdbc.JdbcBackend.Session
import scalaz.Need
import app.DI
import db.{AdminDao, DaoS, DaoT, DaoProvider}

/**
 * [[shipreq.webapp.db.DaoProvider]] that creates and uses a mock DAO.
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
  val conn     = mock[Connection]
  val session  = mock[Session]
  val dao      = mock[DaoT]
  val adminDao = mock[AdminDao]

  when(session.conn).thenReturn(conn)
  for (d <- List(dao, adminDao)) when(d.session).thenReturn(session)

  override protected def rawSession(): Session                            = session
  override def withRawSession [T](f: Session  => T): T                    = f(session)
  override protected def inTransaction[T](s: Session)(f: Session => T): T = f(s)

  override protected def newDaoS    (s: Session): DaoS     = dao
  override protected def newDaoT    (s: Session): DaoT     = dao
  override protected def newAdminDao(s: Session): AdminDao = adminDao

  override def withLazySession[T](f: Need[DaoS] => T): T = f(Need(dao))

  def install[R](fn: => R): R = DI.DaoProvider.doWith(this)(fn)
}

object MockDaoProvider {
  def apply(setup: (DaoT => Unit) = identity) = {
    val p = new MockDaoProvider
    setup(p.dao)
    p
  }
}
