package shipreq.base.test.db

import doobie.free.connection.{ConnectionOp, rollback, setAutoCommit, setSavepoint}
import doobie.imports._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.io.{PrintWriter, StringWriter}
import java.sql.Connection
import scalaz.effect.IO
import scalaz.syntax.apply._
import scalaz.syntax.catchable._
import scalaz.{-\/, Catchable, Free, Monad, \/, \/-}
import shipreq.base.db.DbAccess
import shipreq.base.db.DbAccess.AbstractTransactor
import shipreq.base.test.db.SingleConnectionXA._

object SingleConnectionXA {
  val DoNothing = Free.pure[ConnectionOp, Unit](())
  val BeginTran = setAutoCommit(false) *> setSavepoint
}

final case class SingleConnectionXA(realConn: Connection) extends Transactor[IO] {

  val conn: Connection =
    new DelegateConnection(realConn) {
      override def close(): Unit = ()
    }

  override val connect = IO(conn)
  override def before = DoNothing
  override def after = DoNothing
  override def oops = DoNothing
  override def always = DoNothing

  private def abstractTransactor: AbstractTransactor =
    new AbstractTransactor {
      override def get[M[_] : Catchable : Capture : Monad] = new Transactor[M] {
        override def before = DoNothing
        override def after = DoNothing
        override def oops = DoNothing
        override def always = DoNothing
        override val connect = Monad[M] point conn
      }
    }

  def dbAccess(other: DbAccess): DbAccess =
    DbAccess(
      other.cfg,
      other.ds, // hmmm...
      abstractTransactor,
      other.migrator)

  def useWithAutoCommit[A](use: IO[A]): IO[A] =
    for {
      _ <- setAutoCommit(true).transact(this)
      r <- use
    } yield r

  def useAndRollback[A](use: IO[A]): IO[A] =
    for {
      p <- BeginTran.transact(this)
      r <- use ensuring rollback(p).transact(this)
    } yield r

  def close: IO[Unit] =
    IO {
      \/.fromTryCatchNonFatal(realConn.close()).leftMap(_.printStackTrace)
      ()
    }

  def !![A](c: ConnectionIO[A]): A =
    c.transact(this).unsafePerformIO()

  def ![A](c: ConnectionIO[A]): A =
    c.transact(this).attempt.unsafePerformIO() match {
      case \/-(a) => a
      case -\/(t) =>
        val stackTrace = t.stackTraceAsStringWithLineMod {
          case s if s contains "shipreq" => Console.BOLD + Console.MAGENTA + s + Console.RESET
        }
        System.err.println(stackTrace)
        throw t
    }
}
