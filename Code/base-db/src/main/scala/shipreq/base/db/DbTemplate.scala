package shipreq.base.db

import com.googlecode.flyway.core.Flyway
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import shipreq.base.util.log.HasLogger

/**
 * Template/mixin for database singletons.
 */
trait DbTemplate extends HasLogger {

  protected def newConnection: DatabaseConnection
  protected final lazy val connection = newConnection

  protected def flywayCfg: Flyway => Flyway = identity

  protected def preInit(): Unit = ()

  protected def onInit(implicit s: Session): Unit = ()

  // ===================================================================================================================

  final def DatabaseName = connection.name

  protected lazy val _slick = Database.forDataSource(connection.ds)

  private[this] lazy val migrator = new DbMigrator(connection, flywayCfg)

  private[this] val initLock = new Object
  @volatile private[this] var initPending = true

  def init(): Unit = initLock.synchronized {
    if (initPending) {
      preInit()
      migrator.performPendingMigrations()
      _slick.withTransaction((s: Session) => onInit(s))
      initPending = false
      log.debug("Database initialised successfully.")
    }
  }

  /**
   * Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas.
   */
  protected def wipe_!(): Unit = initLock.synchronized {
    log.warn.z(s"Wiping database: $DatabaseName")
    migrator.wipe_!()
    initPending = true
  }

  def shutdown(): Unit = initLock.synchronized {
    if (!initPending) {
      log.debug("Database shutting down...")
      initPending = true
      connection.ds.close()
    }
  }
}
