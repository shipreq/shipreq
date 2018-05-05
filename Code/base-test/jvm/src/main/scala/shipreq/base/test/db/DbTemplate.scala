package shipreq.base.test.db

import java.sql.Connection
import scalaz.Scalaz.Id
import shipreq.base.db._
import shipreq.base.util.log.HasLogger

/**
 * Template/mixin for database singletons.
 */
trait DbTemplate extends HasLogger {

  def dbCfg: DbConfig

  protected def unsafeInit(): Unit = {
    migrator.migrate[Id]
  }

  protected def unsafeShutdown(): Unit = {
    connection.close()
  }

  // ===================================================================================================================

  final def databaseName = dbCfg.pgDataSource.getDatabaseName

  @volatile private[this] var _connection: Connection = null
  protected final def connection = _connection

  private[this] val initLock = new Object
  final def initialised = _connection ne null
  final def initPending = !initialised

  private final def migrator = SchemaMigrator(dbCfg.pgDataSource, dbCfg.schema)

  final def init(): Unit =
    if (initPending)
      initLock.synchronized(
        if (initPending) {
          log.debug("Database initialising...")
          _connection = dbCfg.pgDataSource.getConnection()
          unsafeInit()
          log.debug("Database initialised.")
        }
      )

  /** Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas. */
  final def dropSchema(): Unit =
    initLock.synchronized {
      val allowed = "shipreq_test"
      if (databaseName != allowed)
        sys.error(s"You're trying to wipe $databaseName. Only $allowed is allowed to be wiped.")
      log.info(s"Wiping database: $databaseName")
      migrator.drop[Id]
    }

  final def shutdown(): Unit =
    if (initialised)
      initLock.synchronized(
        if (initialised) {
          log.debug("Database shutting down...")
          unsafeShutdown()
          _connection = null
          log.debug("Database shut down.")
        }
      )
}
