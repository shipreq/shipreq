package shipreq.base.test.specs2.db

import java.util.concurrent.locks.Lock
import java.util.Properties
import org.specs2.mutable.Specification
import org.specs2.execute.{Result, AsResult}
import org.specs2.specification.AroundExample
import scala.slick.session.{Database, Session}
import scala.slick.jdbc.SQLInterpolation
import shipreq.base.util._
import shipreq.base.db.{SingleConnDatabase, DatabaseConnection, DbTemplate}

object TestDb extends DbTemplate {
  val runMode = RunMode.Test
  val props = JPropertiesValueReader(Props.loadUsingStandardStrategy(runMode)(new Properties))
  import props._
  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick

  override protected def preInit() = wipe_!()
}

trait DatabaseTest extends AroundExample {
  this: Specification =>

  private lazy val dbLog = Logger.forClass(getClass)

  isolated

  private[this] var _session: Option[Session] = None
  implicit def session: Session = _session.getOrElse(throw new RuntimeException("No session available."))

  def db: Database = new SingleConnDatabase(session)

  def mutex: Option[Lock] = None

  def wrapTestsInTransaction = true

  override def around[T: AsResult](t: => T): Result = {
    def inMutex[A](f: => A): A = mutex match {
      case None => f
      case Some(lock) =>
        lock.lockInterruptibly()
        try f finally lock.unlock()
    }

    def go(s: Session)(rollback: => Unit): Result = {
      _session = Some(s)
      try inMutex(AsResult(t))
      finally {
        rollback
        _session = None
      }
    }
    TestDb.init()
    wrapTestsInTransaction match {
      case true =>
        TestDb.slick.withTransaction(s =>
          go(s)(try s.rollback() catch { case e: Throwable => dbLog.warn("Rollback failed.", e) })
        )
      case false =>
        TestDb.slick.withSession(s => go(s)())
    }
  }

  implicit def sqlInterpolation(s: StringContext) = new SQLInterpolation(s)
}
