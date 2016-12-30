package shipreq.base.test.db

import java.util.Properties
import java.util.concurrent.locks.Lock
import scalaz.effect.IO
import scalaz.syntax.apply._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util._

object TestDb extends TestDb

trait TestDb extends DbTemplate with TestDbUsageDefaults[Usable[SingleConnectionXA]] {
  lazy val props = JPropertiesValueReader(Props.loadUsingStandardStrategy(RunMode.Test)(new Properties))
  lazy val dbCfg = ErrorOr require_! DbConfig.read(props)
  lazy val dbAccess = DbAccess.fromCfgWithoutPool(dbCfg)

  @volatile var cleanRequired = false

  override protected def unsafeInit(): Unit = {
    dropSchema()
    cleanRequired = false
    super.unsafeInit()
  }

  protected final val cleanIfRequired: IO[Unit] =
    IO {
      if (cleanRequired) {
        log.debug("Cleaning DB.")
        unsafeClean()
        cleanRequired = false
      }
    }

  /** Clean the DB after a non-transactional test. Usually this means truncation. */
  protected def unsafeClean(): Unit =
    ()

  private val initIO = IO(init())
  private val requireClean = IO(cleanRequired = true)

  override def apply(inTransaction: Boolean = true, mutex: Option[Lock] = None): Usable[SingleConnectionXA] =
    new Usable[SingleConnectionXA] {
      override def apply[X](block: SingleConnectionXA => IO[X]): IO[X] =
        initIO *> cleanIfRequired *> LockUtils.maybeInMutexIO(mutex) {
          val xa = SingleConnectionXA(dbAccess.ds.getConnection)
          val io =
            if (inTransaction)
              xa.useAndRollback(wrapTransaction(xa, block(xa)))
            else
              xa.useWithAutoCommit(block(xa)) ensuring requireClean
          io ensuring xa.close
        }
    }

  /** Close the connection yourself */
  def newConnection(): SingleConnectionXA = {
    init()
    SingleConnectionXA(dbAccess.ds.getConnection)
  }

  def wrapTransaction[A](xa: SingleConnectionXA, io: IO[A]): IO[A] =
    io
}

trait TestDbUsageDefaults[+A] {
  def apply(inTransaction: Boolean = true, mutex: Option[Lock] = None): A

  def mapUsage[B](f: A => B): TestDbUsageDefaults[B] = {
    val self = this
    new TestDbUsageDefaults[B] {
      override def apply(inTransaction: Boolean = true, mutex: Option[Lock] = None) =
        f(self(inTransaction, mutex))
    }
  }
}

trait Usable[+A] {
  def apply[X](f: A => IO[X]): IO[X]

  def runNow[X](f: A => X): X =
    apply(a => IO(f(a))).unsafePerformIO()

  def map[B](f1: A => IO[B]): Usable[B] = {
    val self = this
    new Usable[B] {
      override def apply[X](f: B => IO[X]): IO[X] =
        self(f1(_).flatMap(f))
    }
  }

  def before[B](f: A => IO[B]): Usable[A] =
    map[A](a => f(a).map(_ => a))

  def after[B](f2: A => IO[B]): Usable[A] = {
    val self = this
    new Usable[A] {
      override def apply[X](f: A => IO[X]): IO[X] =
        self(a => for {
          x <- f(a)
          _ <- f2(a)
        } yield x)
    }
  }
}
