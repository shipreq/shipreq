package shipreq.base.test.db

import java.util.concurrent.locks.Lock
import scalaz.syntax.apply._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.FxModule._
import shipreq.base.util._

object TestDb extends TestDb

trait TestDb extends DbTemplate with TestDbUsageDefaults[Usable[SingleConnectionXA]] {
  lazy val (dbCfg, dbCfgReport) = DbConfig.config.withReport.run(Props.sources).unsafeRun().getOrDie()
  // println(dbCfgReport.reportUsed)
  lazy val dbAccess = DbAccess.fromCfgWithoutPool(dbCfg)

  @volatile var cleanRequired = false

  override protected def unsafeInit(): Unit = {
    dropSchema()
    cleanRequired = false
    super.unsafeInit()
  }

  protected final val cleanIfRequired: Fx[Unit] =
    Fx {
      if (cleanRequired) {
        log.debug("Cleaning DB.")
        unsafeClean()
        cleanRequired = false
      }
    }

  /** Clean the DB after a non-transactional test. Usually this means truncation. */
  protected def unsafeClean(): Unit =
    ()

  private val initFx = Fx(init())
  private val requireClean = Fx{cleanRequired = true}

  override def apply(inTransaction: Boolean = true, mutex: Option[Lock] = None): Usable[SingleConnectionXA] =
    new Usable[SingleConnectionXA] {
      override def apply[X](block: SingleConnectionXA => Fx[X]): Fx[X] =
        initFx *> cleanIfRequired *> LockUtils.maybeInMutexFx(mutex) {
          val xa = SingleConnectionXA(dbAccess.ds.getConnection)
          val fx =
            if (inTransaction)
              xa.useAndRollback(wrapTransaction(xa, block(xa)))
            else
              xa.useWithAutoCommit(block(xa)) andFinally requireClean
          fx andFinally xa.close
        }
    }

  /** Close the connection yourself */
  def newConnection(): SingleConnectionXA = {
    init()
    SingleConnectionXA(dbAccess.ds.getConnection)
  }

  def wrapTransaction[A](xa: SingleConnectionXA, io: Fx[A]): Fx[A] =
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
  def apply[X](f: A => Fx[X]): Fx[X]

  def runNow[X](f: A => X): X =
    apply(a => Fx(f(a))).unsafeRun()

  def map[B](f1: A => Fx[B]): Usable[B] = {
    val self = this
    new Usable[B] {
      override def apply[X](f: B => Fx[X]): Fx[X] =
        self(f1(_).flatMap(f))
    }
  }

  def before[B](f: A => Fx[B]): Usable[A] =
    map[A](a => f(a).map(_ => a))

  def after[B](f2: A => Fx[B]): Usable[A] = {
    val self = this
    new Usable[A] {
      override def apply[X](f: A => Fx[X]): Fx[X] =
        self(a => for {
          x <- f(a)
          _ <- f2(a)
        } yield x)
    }
  }
}
