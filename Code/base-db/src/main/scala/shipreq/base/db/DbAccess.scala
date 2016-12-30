package shipreq.base.db

import com.zaxxer.hikari.HikariDataSource
import doobie.hikari.hikaritransactor._
import doobie.imports._
import doobie.japgolly.DoobieHack
import java.sql.Connection
import javax.sql.DataSource
import scalaz.Scalaz._
import scalaz._
import scalaz.effect.IO
import shipreq.base.util.ErrorOr
import shipreq.base.util.log.HasLogger
import DbAccess.AbstractTransactor

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

  val io = abstractTransactor[IO]

  def verifyConnectivity(): Unit = {
    log.info.z(s"Connecting to database: $desc")
    ds.getConnection().close()
  }

  def shutdown(): Unit = {
    ErrorOr.safe(ds match {
      case h: HikariDataSource => h.close()
      case _ => ()
    }).leftMap(e => log.error(e, "Error closing database connections."))
    ()
  }

  def setupRunShutdown[M[_] : Catchable : Capture : Monad, A](app: => M[A]): M[A] =
    for {
      _ ← Monad[M] point verifyConnectivity()
      a ← (migrator.migrate[M] *> app) ensuring Monad[M].point(shutdown())
    } yield a
}

object DbAccess {

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

  def fromCfg(cfg: DbConfig): DbAccess = {
    val ds = new HikariDataSource(cfg.hikariCfg)
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
}
