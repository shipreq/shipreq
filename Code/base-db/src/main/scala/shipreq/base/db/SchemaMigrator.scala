package shipreq.base.db

import cats.Monad
import javax.sql.DataSource
import org.flywaydb.core.Flyway
import shipreq.base.util.CatsExtra.ApplicativeDelay

object SchemaMigrator {

  def apply(ds: DataSource, schema: Option[String]): SchemaMigrator = {
    var cfg = Flyway
      .configure()
      .locations("db_migrations", "shipreq/**/db/migration")
      .sqlMigrationPrefix("v")
      .dataSource(ds)

    schema.foreach(s => cfg = cfg.schemas(s))

    val flyway = cfg.load()

    new SchemaMigrator(flyway)
  }
}

final class SchemaMigrator(private val flyway: Flyway) extends AnyVal {

  def migrate[M[_]](implicit M: Monad[M]): M[Unit] =
    M delay flyway.migrate()

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  def drop[M[_]](implicit M: Monad[M]): M[Unit] =
    M delay flyway.clean()
}
