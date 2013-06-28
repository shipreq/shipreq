package com.beardedlogic.usecase.lib
package db

import ch.qos.logback.classic.Level
import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import net.liftweb.common.Logger
import net.liftweb.util.Props
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory
import scala.slick.session.{Session, Database}
import com.beardedlogic.usecase.model.{RelationType, FieldKeyType, DataType}

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends Logger {

  @inline private def n(name: String) = s"db.$name"
  @inline private def prop(name: String) = Props.get(n(name)) ?~ s"Property not found: ${n(name)}"
  @inline private def setIfDefinedS(name: String, setFn: String => Unit) = Props.get(n(name)).foreach(setFn(_))
  @inline private def setIfDefinedI(name: String, setFn: Int => Unit) = Props.getInt(n(name)).foreach(setFn(_))
  @inline private def setIfDefinedB(name: String, setFn: Boolean => Unit) = Props.getBool(n(name)).foreach(setFn(_))

  private def loadDataSource() = for {
    database <- prop("database")
    username <- prop("username")
    password <- prop("password")
  } yield {
    val ds = new PGSimpleDataSource
    ds.setDatabaseName(database)
    ds.setUser(username)
    ds.setPassword(password)
    setIfDefinedS("appname", ds.setApplicationName(_))
    setIfDefinedB("binary_transfer", ds.setBinaryTransfer(_))
    setIfDefinedS("binary_transfer_disable", ds.setBinaryTransferDisable(_))
    setIfDefinedS("binary_transfer_enable", ds.setBinaryTransferEnable(_))
    setIfDefinedS("compatible", ds.setCompatible(_))
    setIfDefinedI("login_timeout", ds.setLoginTimeout(_))
    setIfDefinedI("port", ds.setPortNumber(_))
    setIfDefinedI("prepare_threshold", ds.setPrepareThreshold(_))
    setIfDefinedI("protocol_ver", ds.setProtocolVersion(_))
    setIfDefinedI("recv_buffer_size", ds.setReceiveBufferSize(_))
    setIfDefinedI("send_buffer_size", ds.setSendBufferSize(_))
    setIfDefinedS("host", ds.setServerName(_))
    setIfDefinedI("socket_timeout", ds.setSocketTimeout(_))
    setIfDefinedB("ssl", ds.setSsl(_))
    setIfDefinedS("ssl_factory", ds.setSslfactory(_))
    setIfDefinedB("tcp_keep_alive", ds.setTcpKeepAlive(_))
    setIfDefinedI("unknown_length", ds.setUnknownLength(_))
    prop("log_level").orElse(prop("loglevel")).foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))
    ds
  }

  val DataSource = {
    val ds = loadDataSource().openOrThrowException("A database connection is mandatory.")
    info("Connecting to database: " + ds.getDatabaseName)
    ds.getConnection.close() // test the data source validity
    ds
  }

  def DatabaseName = DataSource.getDatabaseName

  private val Flyway = {
    LogFactory.setLogCreator(new LogCreator {
      def createLogger(clazz: Class[_]): Log = new FlyWayLogger(clazz)
    })
    val flyway = new com.googlecode.flyway.core.Flyway
    flyway.setLocations("db_migrations")
    flyway.setDataSource(DataSource)
    flyway.setSqlMigrationPrefix("v")
    flyway
  }

  val Slick = Database.forDataSource(DataSource)

  def withInstance[T](transaction: Boolean)(block: Session => T): T = {
    if (transaction) Slick.withTransaction(block) else Slick.withSession(block)
  }

  def performPendingMigrations() = Flyway.migrate()

  def syncEnums(implicit s: Session) = DatabaseEnum.init(DataType, FieldKeyType, RelationType)

  @volatile private var initCalled = false
  def init() {
    synchronized {
      if (!initCalled) {
        performPendingMigrations()
        Slick.withTransaction { implicit s: Session =>
          syncEnums
        }
        debug("Database initialised successfully.")
        initCalled = true
      }
    }
  }

  /**
   * Drops all objects (tables, views, procedures, triggers, ...) in the configured schemas.
   *
   * @return this
   */
  def wipe_!() = {
    synchronized {
      warn("Wiping database: " + DatabaseName)
      Flyway.clean
      initCalled = false
    }
    this
  }
}

class FlyWayLogger(clazz: Class[_]) extends com.googlecode.flyway.core.util.logging.Log {
  val log = LoggerFactory.getLogger(Logger.loggerNameFor(clazz))
  def debug(message: String) {log.debug(message)}
  def info(message: String) {log.info(message)}
  def warn(message: String) {log.warn(message)}
  def error(message: String) {log.error(message)}
  def error(message: String, e: Exception) {log.error(message,e)}
}
