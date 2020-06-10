package shipreq.base.db

import cats.effect.{Blocker, ContextShift, IO, Resource}
import com.zaxxer.hikari.HikariDataSource
import doobie._
import doobie.hikari.HikariTransactor
import javax.sql.DataSource
import scala.concurrent.ExecutionContext
import shipreq.base.util.FxModule._
import shipreq.base.util.ThreadUtils
import shipreq.base.util.log.HasLogger

final case class DbAccessor(config    : DbConfig,
                            dataSource: DataSource,
                            xa        : Resource[Fx, XA],
                            migrator  : SchemaMigrator) extends HasLogger {

  def host: String =
    config.pgDataSource.getServerNames()(0)

  def databaseName: String =
    config.pgDataSource.getDatabaseName

  def schema: Option[String] =
    config.schema

  def absoluteSchema: String =
    config.schema getOrElse "public"

  def schemaAsPrefix: String =
    config.schema.map(_ + ".") getOrElse ""

  def desc: String =
    s"$host/$databaseName" + schema.map(":" + _).getOrElse("")

  val verifyConnectivity: Fx[Unit] =
    Fx {
      logger.info(s"Connecting to database: $desc")
      dataSource.getConnection().close()
    }
}

object DbAccessor extends HasLogger {

  /** When using docker-compose, sometimes the DB image needs more time to initialise. This adds a small retry. */
  def fromCfg(cfg: DbConfig): Fx[DbAccessor] = {
    val delay = Fx {
      logger.warn("DbAccess initialisation failed. Retrying...")
      Thread.sleep(2000)
    }
    fromCfgWithoutRetry(cfg).retryOnException((n, _) => if (n < 4) Some(delay) else None)
  }

  // This is in Fx because HikariDataSource connects to the DB (and throws when unable) on construction.
  def fromCfgWithoutRetry(cfg: DbConfig): Fx[DbAccessor] =
    Fx {
      val poolSize = cfg.poolSize
      assert(poolSize >= 1, s"DB pool size = $poolSize ?!")

      val ds = new HikariDataSource(cfg.hikariConfig)

      implicit val ec: ExecutionContext =
        ThreadUtils.newThreadPool("HikariCP", logger).withThreads(poolSize).executionContext

      implicit val cs: ContextShift[Fx] =
        IO.contextShift(ec)

      val xaRes: Resource[Fx, XA] =
        for {
          ce <- ExecutionContexts.fixedThreadPool[Fx](poolSize)
          be <- Blocker[Fx]
        } yield new XA(HikariTransactor[Fx](ds, ce, be))

      val migrator = SchemaMigrator(ds, cfg.schema)

      DbAccessor(cfg, ds, xaRes, migrator)
    }
}
