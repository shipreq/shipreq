package shipreq.base.db

import com.googlecode.flyway.core.Flyway
import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import org.slf4j.LoggerFactory
import javax.sql.DataSource
import scalaz.Monad

object SchemaMigrator {

  final class FlyWayLogger(clazz: Class[_]) extends Log {
    private[this] val log = LoggerFactory.getLogger(clazz)
    override def debug(message: String) = log.debug(message)
    override def info(message: String) = log.info(message)
    override def warn(message: String) = log.warn(message)
    override def error(message: String) = log.error(message)
    override def error(message: String, e: Exception) = log.error(message,e)
  }

  def apply(ds: DataSource, schema: Option[String]): SchemaMigrator = {
    LogFactory.setLogCreator(new LogCreator {
      override def createLogger(clazz: Class[_]): Log = new FlyWayLogger(clazz)
    })
    val flyway = new Flyway
    flyway.setLocations("db_migrations")
    flyway.setSqlMigrationPrefix("v")
    schema.foreach(flyway.setSchemas(_))
    flyway.setDataSource(ds)
    new SchemaMigrator(flyway)
  }
}

final class SchemaMigrator(private val flyway: Flyway) extends AnyVal {

  def migrate[M[_]](implicit M: Monad[M]): M[Unit] =
    M point flyway.migrate()

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  def drop[M[_]](implicit M: Monad[M]): M[Unit] =
    M point flyway.clean()
}
