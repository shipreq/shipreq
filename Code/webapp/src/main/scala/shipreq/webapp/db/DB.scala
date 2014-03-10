package shipreq.webapp
package db

import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import net.liftweb.common.Logger
import org.slf4j.LoggerFactory
import scala.slick.session.Session

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends Logger {

  import util.PropsRetrievers._
  private val base = new BaseDbConnection()

  @inline def DataSource = base.DataSource
  @inline def DatabaseName = base.DatabaseName

  // ===================================================================================================================
  // Schema

  private[this] class FlyWayLogger(clazz: Class[_]) extends com.googlecode.flyway.core.util.logging.Log {
    val log = LoggerFactory.getLogger(Logger.loggerNameFor(clazz))
    def debug(message: String) {log.debug(message)}
    def info(message: String) {log.info(message)}
    def warn(message: String) {log.warn(message)}
    def error(message: String) {log.error(message)}
    def error(message: String, e: Exception) {log.error(message,e)}
  }

  private val Flyway = {
    LogFactory.setLogCreator(new LogCreator {
      override def createLogger(clazz: Class[_]): Log = new FlyWayLogger(clazz)
    })
    val flyway = new com.googlecode.flyway.core.Flyway
    flyway.setLocations("db_migrations")
    flyway.setDataSource(DataSource)
    flyway.setSqlMigrationPrefix("v")
    flyway
  }

  private def performPendingMigrations() = Flyway.migrate()

  @volatile private var initPending = true

  def init(): Unit = synchronized {
    if (initPending) {
      init_!()
      debug("Database initialised successfully.")
      initPending = false
    }
  }

  private def init_!(): Unit = {
    performPendingMigrations()
    Slick.withTransaction { implicit s: Session =>
      FieldKeyType.init
    }
  }

  /**
   * Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas.
   * @return this
   */
  def wipe_!() = synchronized {
    warn("Wiping database: " + DatabaseName)
    Flyway.clean
    initPending = true
    this
  }

  // ===================================================================================================================
  // Access

  @inline private def Slick = base.Slick

  object DaoProvider extends DaoProvider {
    override def withRawSession[T](f: Session => T): T = Slick.withSession(f)
    override protected def rawSession(): Session       = Slick.createSession()
  }
}
