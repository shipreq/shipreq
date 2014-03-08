package com.beardedlogic.shipreq.db

import com.googlecode.flyway.core.Flyway
import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import org.slf4j.LoggerFactory
import javax.sql.DataSource

class DbMigrator(ds: DataSource, cfg: Flyway => Flyway = identity) {

  final class FlyWayLogger(clazz: Class[_]) extends Log {
    private[this] val log = LoggerFactory.getLogger(clazz)
    override def debug(message: String) = log.debug(message)
    override def info(message: String) = log.info(message)
    override def warn(message: String) = log.warn(message)
    override def error(message: String) = log.error(message)
    override def error(message: String, e: Exception) = log.error(message,e)
  }

  private[this] val flyway = {
    LogFactory.setLogCreator(new LogCreator {
      override def createLogger(clazz: Class[_]): Log = new FlyWayLogger(clazz)
    })
    val flyway = {
      val flyway = new Flyway
      flyway.setLocations("db_migrations")
      flyway.setSqlMigrationPrefix("v")
      cfg(flyway)
    }
    flyway.setDataSource(ds)
    flyway
  }

  def performPendingMigrations() = flyway.migrate()

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  def wipe_!() = flyway.clean()
}
