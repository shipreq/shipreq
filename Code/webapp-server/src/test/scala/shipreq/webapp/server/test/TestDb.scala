package shipreq.webapp.server.test

import shipreq.webapp.server.app.{DI, Defaults}
import shipreq.webapp.server.db.{DB, DaoProvider, DaoT, Shim}
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import shipreq.base.util.ThreadLocalRes
import shipreq.webapp.server.test.{DbUtil => DBUtil}

object TestDb {
  @volatile private[this] var initialised = false
  private[this] val lock = new AnyRef

  def init(): Unit =
    if (!initialised)
      lock.synchronized(
        if (!initialised) {
          initialised = true
          PrepareEnv()
          DB.wipe_!()
          Defaults.uninit()
          (new bootstrap.liftweb.Boot).initDatabase()
        }
      )

  def reinitOnNextUse(): Unit =
    initialised = false

  val Slick = Database.forDataSource(DB.DataSource)

  val Session: ThreadLocalRes[Session] =
    ThreadLocalRes {
      init()
      Slick.createSession()
    }
      .wrapUse(TestDb.UseInDI) // TODO This will slow down DB prop tests...

  object UseInDI extends ThreadLocalRes.Around[Session] {
    override def apply[A](s: Session)(a: => A): A =
      DI.DaoProvider.doWith(new TestDaoProvider()(s))(a)
  }

  val Transaction: ThreadLocalRes[Session] =
    Session
      .onLend(_.conn.setAutoCommit(false))
      .onReturn(_.conn.rollback())

  val DbUtil: ThreadLocalRes[DBUtil] =
    Transaction.xmap(DBUtil(_))(_.session)

  val Dao: ThreadLocalRes[DaoT] =
    Transaction.xmap(Shim.newDaoT)(_.session)
}

class TestDaoProvider()(implicit session: Session) extends DaoProvider {
  override def withRawSession[T](f: Session => T): T =
    f(session)

  override protected def rawSession(): Session =
    session

  override protected def inTransaction[T](s: Session)(f: Session => T): T = {
    if (s.conn.getAutoCommit)
      super.inTransaction(s)(f)
    else
      f(s)
  }
}
