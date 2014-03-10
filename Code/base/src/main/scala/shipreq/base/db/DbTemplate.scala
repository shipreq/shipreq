package shipreq.base.db

import com.googlecode.flyway.core.Flyway
import com.jolbox.bonecp.{ConnectionHandle, BoneCPDataSource}
import com.jolbox.bonecp.hooks.{AbstractConnectionHook, ConnectionHook}
import org.slf4j.LoggerFactory
import scala.slick.session.Session

object DbTemplate {
  type C = BoneCPDataSource => BoneCPDataSource

  def setSearchPath(path: String): C =
    runOnConnectionAcquire(s"SET search_path TO $path")

  def runOnConnectionAcquire(sql: String): C =
    ds => {
      val hook: ConnectionHook = new AbstractConnectionHook {
        override def onAcquire(conn: ConnectionHandle): Unit = {
          val s = conn.createStatement()
          try s.execute(sql)
          finally s.close()
        }
      }
      ds.setConnectionHook(hook)
      ds
    }

  type F = Flyway => Flyway

  def setSchema(schema: String): F =
    f => {f.setSchemas(schema); f}
}

/**
 * Template/mixin for database singletons.
 */
trait DbTemplate {

  protected val log = LoggerFactory.getLogger(getClass)

  // ===================================================================================================================
  // Connection

  protected def establishConnection(): BaseDbConnection

  protected val baseConn = establishConnection()
  import baseConn.{DataSource, Slick}

  @inline final def DatabaseName = baseConn.DatabaseName

  // ===================================================================================================================
  // Initialisation

  protected def flywayCfg: Flyway => Flyway = identity
  private[this] val migrator = new DbMigrator(DataSource, flywayCfg)
  private[this] def initLock: AnyRef = migrator

  @volatile private[this] var initPending = true

  def init(): Unit = initLock.synchronized {
    if (initPending) {
      migrator.performPendingMigrations()
      Slick.withTransaction((s: Session) => onInit(s))
      initPending = false
      log.debug("Database initialised successfully.")
    }
  }

  protected def onInit(implicit s: Session): Unit = ()

  /**
   * Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas.
   */
  def wipe_!() = initLock.synchronized {
    log.warn("Wiping database: " + DatabaseName)
    migrator.wipe_!()
    initPending = true
    this
  }
}
