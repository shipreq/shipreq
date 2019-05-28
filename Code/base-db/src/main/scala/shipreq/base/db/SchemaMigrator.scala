package shipreq.base.db

import javax.sql.DataSource
import org.flywaydb.core.Flyway
import scalaz.Monad

object SchemaMigrator {

  def apply(ds: DataSource, schema: Option[String]): SchemaMigrator = {
    var cfg = Flyway
      .configure()
      .locations("db_migrations")
      .sqlMigrationPrefix("v")
      .dataSource(ds)

    schema.foreach(s => cfg = cfg.schemas(s))

    val flyway = cfg.load()

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
