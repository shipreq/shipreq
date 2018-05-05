package shipreq.base.db

import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.hikaritransactor._
import doobie.imports._
import doobie.japgolly.DoobieHack
import javax.sql.DataSource
import scalaz.Scalaz._
import scalaz._
import shipreq.base.db.DbAccess.AbstractTransactor
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import DbAccess.fxCapture

final case class DbAccess(cfg               : DbConfig,
                          ds                : DataSource,
                          abstractTransactor: AbstractTransactor,
                          migrator          : SchemaMigrator) extends HasLogger {

  def host: String =
    cfg.pgDataSource.getServerName

  def databaseName: String =
    cfg.pgDataSource.getDatabaseName

  def schema: Option[String] =
    cfg.schema

  def absoluteSchema: String =
    cfg.schema getOrElse "public"

  def schemaAsPrefix: String =
    cfg.schema.map(_ + ".") getOrElse ""

  def desc: String =
    s"$host/$databaseName" + schema.map(":" + _).getOrElse("")

  val fx = abstractTransactor[Fx]

  def verifyConnectivity(): Unit = {
    log.info(s"Connecting to database: $desc")
    ds.getConnection().close()
  }

  def shutdown(): Unit =
    ArticulateError.attempt(
      ds match {
        case h: HikariDataSource => h.close()
        case _ => ()
      }
    ).swap.foreach(log.error("Error closing database connections.", _))

  def setupRunShutdown[M[_] : Catchable : Capture : Monad, A](app: => M[A]): M[A] =
    for {
      _ ← Monad[M] point verifyConnectivity()
      a ← (migrator.migrate[M] *> app) ensuring Monad[M].point(shutdown())
    } yield a
}

object DbAccess extends HasLogger {

  trait AbstractTransactor {
    final def apply[M[_]: Catchable : Capture : Monad]: Transactor[M] =
      DoobieHack.disableAutoTransactions(get[M])

    protected def get[M[_]: Catchable : Capture : Monad]: Transactor[M]
  }
  object AbstractTransactor {
    def hikari(ds: HikariDataSource): AbstractTransactor =
      new AbstractTransactor {
        override def get[M[_]: Catchable : Capture : Monad] = HikariTransactor[M](ds)
      }
    def dataSource(ds: DataSource): AbstractTransactor =
      new AbstractTransactor {
        override def get[M[_]: Catchable : Capture : Monad] = DataSourceTransactor[M].apply(ds)
      }
  }

  /** When using docker-compose, sometimes the DB image needs more time to initialise. This adds a small retry. */
  def fromCfg(cfg: DbConfig): Fx[DbAccess] = {
    val delay = Fx {
      log.info("DbAccess initialisation failed. Retrying...")
      Thread.sleep(2000)
    }
    fromCfgWithoutRetry(cfg).retryOnException((n, _) => if (n < 4) Some(delay) else None)
  }

  // This is in Fx because HikariDataSource connects to the DB (and throws when unable) on construction.
  def fromCfgWithoutRetry(cfg: DbConfig): Fx[DbAccess] =
    Fx {
      val ds = new HikariDataSource(cfg.hikariConfig)
      val xa = AbstractTransactor.hikari(ds)
      val migrator = SchemaMigrator(ds, cfg.schema)
      DbAccess(cfg, ds, xa, migrator)
    }

  def fromCfgWithoutPool(cfg: DbConfig): DbAccess = {
    val ds = cfg.pgDataSource
    val xa = AbstractTransactor.dataSource(ds)
    val migrator = SchemaMigrator(ds, cfg.schema)
    DbAccess(cfg, ds, xa, migrator)
  }

  implicit val fxCapture: Capture[Fx] =
    new Capture[Fx] {
      override def apply[A](a: => A) = Fx(a)
    }
}
