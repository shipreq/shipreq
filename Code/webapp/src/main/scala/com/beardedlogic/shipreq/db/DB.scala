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
    import util.ExternalValueReader._
    import util.RuntimePropReaders._
    implicit val scope = scopeByNS("db")
    for {
      database <- get[String]("database")
      username <- get[String]("username")
      password <- get[String]("password")
    } yield {
      val ds = new PGSimpleDataSource
      ds.setDatabaseName(database)
      ds.setUser(username)
//      ds.setPassword(password)
      ds.setDisableColumnSanitiser(true)
      useIfAvailable("appname"                )(ds.setApplicationName)
      useIfAvailable("binary_transfer"        )(ds.setBinaryTransfer)
      useIfAvailable("binary_transfer_disable")(ds.setBinaryTransferDisable)
      useIfAvailable("binary_transfer_enable" )(ds.setBinaryTransferEnable)
      useIfAvailable("compatible"             )(ds.setCompatible)
      useIfAvailable("disableColumnSanitiser" )(ds.setDisableColumnSanitiser)
      useIfAvailable("login_timeout"          )(ds.setLoginTimeout)
      useIfAvailable("port"                   )(ds.setPortNumber)
      useIfAvailable("prepare_threshold"      )(ds.setPrepareThreshold)
      useIfAvailable("protocol_ver"           )(ds.setProtocolVersion)
      useIfAvailable("recv_buffer_size"       )(ds.setReceiveBufferSize)
      useIfAvailable("send_buffer_size"       )(ds.setSendBufferSize)
      useIfAvailable("host"                   )(ds.setServerName)
      useIfAvailable("socket_timeout"         )(ds.setSocketTimeout)
      useIfAvailable("ssl"                    )(ds.setSsl)
      useIfAvailable("ssl_factory"            )(ds.setSslfactory)
      useIfAvailable("tcp_keep_alive"         )(ds.setTcpKeepAlive)
      useIfAvailable("unknown_length"         )(ds.setUnknownLength)
      getVar[String]("log_level", "loglevel").foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      {
        implicit val scope = scopeByNS("db.pool")
        val pool = new BoneCPDataSource()
        pool.setJdbcUrl(ds.getUrl)
        pool.setUsername(username)
        pool.setPassword(password)
        pool.setDefaultTransactionIsolation("READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
        pool.setDefaultAutoCommit(true)
        pool.setLogStatementsEnabled(false)

        useIfAvailable("acquireIncrement"                 )(pool.setAcquireIncrement)
        useIfAvailable("acquireRetryAttempts"             )(pool.setAcquireRetryAttempts)
        useIfAvailable("acquireRetryDelayInMs"            )(pool.setAcquireRetryDelayInMs)
        useIfAvailable("closeConnectionWatch"             )(pool.setCloseConnectionWatch)
        useIfAvailable("closeConnectionWatchTimeoutInMs"  )(pool.setCloseConnectionWatchTimeoutInMs)
        useIfAvailable("connectionHookClassName"          )(pool.setConnectionHookClassName)
        useIfAvailable("connectionTestStatement"          )(pool.setConnectionTestStatement)
        useIfAvailable("connectionTimeoutInMs"            )(pool.setConnectionTimeoutInMs)
        useIfAvailable("defaultCatalog"                   )(pool.setDefaultCatalog)
        useIfAvailable("disableConnectionTracking"        )(pool.setDisableConnectionTracking)
        useIfAvailable("disableJMX"                       )(pool.setDisableJMX)
        useIfAvailable("externalAuth"                     )(pool.setExternalAuth)
        useIfAvailable("idleConnectionTestPeriodInMinutes")(pool.setIdleConnectionTestPeriodInMinutes)
        useIfAvailable("idleConnectionTestPeriodInSeconds")(pool.setIdleConnectionTestPeriodInSeconds)
        useIfAvailable("idleMaxAgeInMinutes"              )(pool.setIdleMaxAgeInMinutes)
        useIfAvailable("idleMaxAgeInSeconds"              )(pool.setIdleMaxAgeInSeconds)
        useIfAvailable("initSQL"                          )(pool.setInitSQL)
        useIfAvailable("lazyInit"                         )(pool.setLazyInit)
        useIfAvailable("logStatementsEnabled"             )(pool.setLogStatementsEnabled)
        useIfAvailable("maxConnectionAgeInSeconds"        )(pool.setMaxConnectionAgeInSeconds)
        useIfAvailable("maxConnectionsPerPartition"       )(pool.setMaxConnectionsPerPartition)
        useIfAvailable("minConnectionsPerPartition"       )(pool.setMinConnectionsPerPartition)
        useIfAvailable("partitionCount"                   )(pool.setPartitionCount)
        useIfAvailable("poolAvailabilityThreshold"        )(pool.setPoolAvailabilityThreshold)
        useIfAvailable("poolName"                         )(pool.setPoolName)
        useIfAvailable("queryExecuteTimeLimitInMs"        )(pool.setQueryExecuteTimeLimitInMs)
        useIfAvailable("serviceOrder"                     )(pool.setServiceOrder)
        useIfAvailable("statementsCacheSize"              )(pool.setStatementsCacheSize)
        useIfAvailable("statisticsEnabled"                )(pool.setStatisticsEnabled)
        useIfAvailable("transactionRecoveryEnabled"       )(pool.setTransactionRecoveryEnabled)

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
    override def withRawSession[T](f: Session => T): T = Slick.withSession(f)
    override protected def rawSession(): Session       = Slick.createSession()
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
