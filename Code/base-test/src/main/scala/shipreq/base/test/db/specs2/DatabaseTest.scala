package shipreq.base.test.db.specs2

import org.specs2.mutable.Specification
import org.specs2.execute.{Result, AsResult}
import org.specs2.specification.AroundExample
import java.util.Properties
import scala.slick.session.{Database, Session}
import scala.slick.jdbc.SQLInterpolation
import shipreq.base.util._
import shipreq.base.db.{DatabaseConnection, DbTemplate}

object TestDb extends DbTemplate {
  val runMode = RunMode.Test
  val props = JPropertiesValueReader(Props.loadUsingStandardStrategy(runMode)(new Properties))
  import props._
  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick

  override protected def preInit() = wipe_!()
}

class SingleConnDatabase(s: Session) extends Database {
  override def createConnection() = s.conn
  override def withSession[T](f: Session => T): T = f(s)
}

trait DatabaseTest extends AroundExample {
  this: Specification =>

  private lazy val dbLog = Logger.forClass(getClass)

  isolated

  private[this] var _session: Option[Session] = None
  implicit def session: Session = _session.getOrElse(throw new RuntimeException("No session available."))

  def db: Database = new SingleConnDatabase(session)

  override def around[T: AsResult](t: => T): Result = {
    TestDb.init()
    TestDb.slick.withTransaction(s => {
      _session = Some(s)
      try AsResult(t)
      finally {
        try s.rollback() catch {case e: Throwable => dbLog.warn("Rollback failed.", e)}
        _session = None
      }
    })
  }

  implicit def sqlInterpolation(s: StringContext) = new SQLInterpolation(s)
}
