package com.beardedlogic.shipreq
package db

import ch.qos.logback.classic.Level
import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import com.jolbox.bonecp.BoneCPDataSource
import net.liftweb.common.Logger
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory
import scala.slick.session.{Session, Database}

/**
 * Database connectivity.
 *
 * @since 21/05/2013
 */
object DB extends Logger {

  // ===================================================================================================================
  // Connection

  private def loadDataSource() = {
    import util.RuntimeProps._
    implicit val scope = PropScope(name => s"db.$name")
    for {
      database <- wantS("database")
      username <- wantS("username")
      password <- wantS("password")
    } yield {
      val ds = new PGSimpleDataSource
      ds.setDatabaseName(database)
      ds.setUser(username)
//      ds.setPassword(password)
      useIfDefinedS("appname",                 ds.setApplicationName)
      useIfDefinedB("binary_transfer",         ds.setBinaryTransfer)
      useIfDefinedS("binary_transfer_disable", ds.setBinaryTransferDisable)
      useIfDefinedS("binary_transfer_enable",  ds.setBinaryTransferEnable)
      useIfDefinedS("compatible",              ds.setCompatible)
      useIfDefinedI("login_timeout",           ds.setLoginTimeout)
      useIfDefinedI("port",                    ds.setPortNumber)
      useIfDefinedI("prepare_threshold",       ds.setPrepareThreshold)
      useIfDefinedI("protocol_ver",            ds.setProtocolVersion)
      useIfDefinedI("recv_buffer_size",        ds.setReceiveBufferSize)
      useIfDefinedI("send_buffer_size",        ds.setSendBufferSize)
      useIfDefinedS("host",                    ds.setServerName)
      useIfDefinedI("socket_timeout",          ds.setSocketTimeout)
      useIfDefinedB("ssl",                     ds.setSsl)
      useIfDefinedS("ssl_factory",             ds.setSslfactory)
      useIfDefinedB("tcp_keep_alive",          ds.setTcpKeepAlive)
      useIfDefinedI("unknown_length",          ds.setUnknownLength)
      wantS("log_level").orElse(wantS("loglevel")).foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      {
        implicit val scope = PropScope(name => s"db.pool.$name")
        val pool = new BoneCPDataSource()
        pool.setJdbcUrl(ds.getUrl)
        pool.setUsername(username)
        pool.setPassword(password)
        pool.setDefaultTransactionIsolation("READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
        pool.setDefaultAutoCommit(true)
        pool.setLogStatementsEnabled(false)

        useIfDefinedI("acquireIncrement",                  pool.setAcquireIncrement)
        useIfDefinedI("acquireRetryAttempts",              pool.setAcquireRetryAttempts)
        useIfDefinedL("acquireRetryDelayInMs",             pool.setAcquireRetryDelayInMs)
        useIfDefinedB("closeConnectionWatch",              pool.setCloseConnectionWatch)
        useIfDefinedL("closeConnectionWatchTimeoutInMs",   pool.setCloseConnectionWatchTimeoutInMs)
        useIfDefinedS("connectionHookClassName",           pool.setConnectionHookClassName)
        useIfDefinedS("connectionTestStatement",           pool.setConnectionTestStatement)
        useIfDefinedL("connectionTimeoutInMs",             pool.setConnectionTimeoutInMs)
        useIfDefinedS("defaultCatalog",                    pool.setDefaultCatalog)
        useIfDefinedB("disableConnectionTracking",         pool.setDisableConnectionTracking)
        useIfDefinedB("disableJMX",                        pool.setDisableJMX)
        useIfDefinedB("externalAuth",                      pool.setExternalAuth)
        useIfDefinedL("idleConnectionTestPeriodInMinutes", pool.setIdleConnectionTestPeriodInMinutes)
        useIfDefinedL("idleConnectionTestPeriodInSeconds", pool.setIdleConnectionTestPeriodInSeconds)
        useIfDefinedL("idleMaxAgeInMinutes",               pool.setIdleMaxAgeInMinutes)
        useIfDefinedL("idleMaxAgeInSeconds",               pool.setIdleMaxAgeInSeconds)
        useIfDefinedS("initSQL",                           pool.setInitSQL)
        useIfDefinedB("lazyInit",                          pool.setLazyInit)
        useIfDefinedB("logStatementsEnabled",              pool.setLogStatementsEnabled)
        useIfDefinedL("maxConnectionAgeInSeconds",         pool.setMaxConnectionAgeInSeconds)
        useIfDefinedI("maxConnectionsPerPartition",        pool.setMaxConnectionsPerPartition)
        useIfDefinedI("minConnectionsPerPartition",        pool.setMinConnectionsPerPartition)
        useIfDefinedI("partitionCount",                    pool.setPartitionCount)
        useIfDefinedI("poolAvailabilityThreshold",         pool.setPoolAvailabilityThreshold)
        useIfDefinedS("poolName",                          pool.setPoolName)
        useIfDefinedL("queryExecuteTimeLimitInMs",         pool.setQueryExecuteTimeLimitInMs)
        useIfDefinedS("serviceOrder",                      pool.setServiceOrder)
        useIfDefinedI("statementsCacheSize",               pool.setStatementsCacheSize)
        useIfDefinedB("statisticsEnabled",                 pool.setStatisticsEnabled)
        useIfDefinedB("transactionRecoveryEnabled",        pool.setTransactionRecoveryEnabled)

        pool
      }
    }
  }

  val DataSource = {
    val ds = loadDataSource().openOrThrowException("A database connection is mandatory.")
    info("Connecting to database: " + getDatabaseName(ds))
    debug("Database pool config: " + ds.getConfig)
    ds.getConfig
    ds.getConnection().close() // test the data source validity
    ds
  }

  private def getDatabaseName(ds: BoneCPDataSource) = ds.getJdbcUrl.replaceFirst("\\?.*", "").replaceFirst("^.+//", "")
  val DatabaseName = getDatabaseName(DataSource)

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

  // ===================================================================================================================
  // Access

  private val Slick = Database.forDataSource(DataSource)

  object DaoProvider extends DaoProvider {
    override def withSession[T](block: DaoS => T): T = Slick.withSession(initConnAndExec(_, block))
    override def withTransaction[T](block: DaoT => T): T = Slick.withTransaction(initConnAndExec(_, block))
    @inline private def initConnAndExec[T](s: Session, block: Dao => T): T = {
      block(new Dao(s))
    }
  }

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

}
