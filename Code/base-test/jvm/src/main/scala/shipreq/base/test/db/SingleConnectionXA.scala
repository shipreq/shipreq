package shipreq.base.test.db

import doobie.free.connection.{ConnectionOp, rollback, setAutoCommit, setSavepoint}
import doobie.imports._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.sql.Connection
import scalaz.syntax.apply._
import scalaz.{-\/, Catchable, Free, Monad, \/, \/-}
import shipreq.base.db.DbAccess
import shipreq.base.db.DbAccess.AbstractTransactor
import shipreq.base.util.FxModule._
import shipreq.base.test.db.SingleConnectionXA._
import DbAccess.fxCapture

object SingleConnectionXA {
  val DoNothing = Free.pure[ConnectionOp, Unit](())
  val BeginTran = setAutoCommit(false) *> setSavepoint
}

final case class SingleConnectionXA(realConn: Connection) extends Transactor[Fx] {

  val conn: Connection =
    new DelegateConnection(realConn) {
      override def close(): Unit = ()
    }

  override val connect = Fx(conn)
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

  def useWithAutoCommit[A](use: Fx[A]): Fx[A] =
    for {
      _ <- setAutoCommit(true).transact(this)
      r <- use
    } yield r

  def useAndRollback[A](use: Fx[A]): Fx[A] =
    for {
      p <- BeginTran.transact(this)
      r <- use ensuring rollback(p).transact(this)
    } yield r

  def close: Fx[Unit] =
    Fx {
      \/.fromTryCatchNonFatal(realConn.close()).leftMap(_.printStackTrace)
      ()
    }

  def !![A](c: ConnectionIO[A]): A =
    c.transact(this).unsafeRun()

  def ![A](c: ConnectionIO[A]): A =
    c.transact(this).attempt.unsafeRun() match {
      case Right(a) => a
      case Left(t) =>
        val stackTrace = t.stackTraceAsStringWithLineMod {
          case s if s contains "shipreq" => Console.BOLD + Console.MAGENTA + s + Console.RESET
        }
        System.err.println(stackTrace)
        throw t
    }
}
