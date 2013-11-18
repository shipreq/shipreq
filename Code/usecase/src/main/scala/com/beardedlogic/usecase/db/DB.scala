package com.beardedlogic.usecase
package db

import ch.qos.logback.classic.Level
import com.googlecode.flyway.core.util.logging.{Log, LogCreator, LogFactory}
import com.jolbox.bonecp.BoneCPDataSource
import net.liftweb.common.Logger
import net.liftweb.util.Props
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

  private case class PropScope(run: String => String)
  @inline private def prop(name: String)(implicit n: PropScope) = Props.get(n.run(name)) ?~ s"Property not found: ${n.run(name)}"
  @inline private def setIfDefinedS(name: String, f: String  => Unit)(implicit n: PropScope) = Props get n.run(name) foreach f
  @inline private def setIfDefinedI(name: String, f: Int     => Unit)(implicit n: PropScope) = Props getInt n.run(name) foreach f
  @inline private def setIfDefinedL(name: String, f: Long    => Unit)(implicit n: PropScope) = Props getLong n.run(name) foreach f
  @inline private def setIfDefinedB(name: String, f: Boolean => Unit)(implicit n: PropScope) = Props getBool n.run(name) foreach f

  private def loadDataSource() = {
    implicit val scope = PropScope(name => s"db.$name")
    for {
      database <- prop("database")
      username <- prop("username")
      password <- prop("password")
    } yield {
      val ds = new PGSimpleDataSource
      ds.setDatabaseName(database)
      ds.setUser(username)
//      ds.setPassword(password)
      setIfDefinedS("appname",                 ds.setApplicationName)
      setIfDefinedB("binary_transfer",         ds.setBinaryTransfer)
      setIfDefinedS("binary_transfer_disable", ds.setBinaryTransferDisable)
      setIfDefinedS("binary_transfer_enable",  ds.setBinaryTransferEnable)
      setIfDefinedS("compatible",              ds.setCompatible)
      setIfDefinedI("login_timeout",           ds.setLoginTimeout)
      setIfDefinedI("port",                    ds.setPortNumber)
      setIfDefinedI("prepare_threshold",       ds.setPrepareThreshold)
      setIfDefinedI("protocol_ver",            ds.setProtocolVersion)
      setIfDefinedI("recv_buffer_size",        ds.setReceiveBufferSize)
      setIfDefinedI("send_buffer_size",        ds.setSendBufferSize)
      setIfDefinedS("host",                    ds.setServerName)
      setIfDefinedI("socket_timeout",          ds.setSocketTimeout)
      setIfDefinedB("ssl",                     ds.setSsl)
      setIfDefinedS("ssl_factory",             ds.setSslfactory)
      setIfDefinedB("tcp_keep_alive",          ds.setTcpKeepAlive)
      setIfDefinedI("unknown_length",          ds.setUnknownLength)
      prop("log_level").orElse(prop("loglevel")).foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      {
        implicit val scope = PropScope(name => s"db.pool.$name")
        val pool = new BoneCPDataSource()
        pool.setJdbcUrl(ds.getUrl)
        pool.setUsername(username)
        pool.setPassword(password)
        pool.setDefaultTransactionIsolation("SERIALIZABLE")
        pool.setDefaultAutoCommit(true)
        pool.setLogStatementsEnabled(false)

        setIfDefinedI("acquireIncrement",                  pool.setAcquireIncrement)
        setIfDefinedI("acquireRetryAttempts",              pool.setAcquireRetryAttempts)
        setIfDefinedL("acquireRetryDelayInMs",             pool.setAcquireRetryDelayInMs)
        setIfDefinedB("closeConnectionWatch",              pool.setCloseConnectionWatch)
        setIfDefinedL("closeConnectionWatchTimeoutInMs",   pool.setCloseConnectionWatchTimeoutInMs)
        setIfDefinedS("connectionHookClassName",           pool.setConnectionHookClassName)
        setIfDefinedS("connectionTestStatement",           pool.setConnectionTestStatement)
        setIfDefinedL("connectionTimeoutInMs",             pool.setConnectionTimeoutInMs)
        setIfDefinedS("defaultCatalog",                    pool.setDefaultCatalog)
        setIfDefinedB("disableConnectionTracking",         pool.setDisableConnectionTracking)
        setIfDefinedB("disableJMX",                        pool.setDisableJMX)
        setIfDefinedB("externalAuth",                      pool.setExternalAuth)
        setIfDefinedL("idleConnectionTestPeriodInMinutes", pool.setIdleConnectionTestPeriodInMinutes)
        setIfDefinedL("idleConnectionTestPeriodInSeconds", pool.setIdleConnectionTestPeriodInSeconds)
        setIfDefinedL("idleMaxAgeInMinutes",               pool.setIdleMaxAgeInMinutes)
        setIfDefinedL("idleMaxAgeInSeconds",               pool.setIdleMaxAgeInSeconds)
        setIfDefinedS("initSQL",                           pool.setInitSQL)
        setIfDefinedB("lazyInit",                          pool.setLazyInit)
        setIfDefinedB("logStatementsEnabled",              pool.setLogStatementsEnabled)
        setIfDefinedL("maxConnectionAgeInSeconds",         pool.setMaxConnectionAgeInSeconds)
        setIfDefinedI("maxConnectionsPerPartition",        pool.setMaxConnectionsPerPartition)
        setIfDefinedI("minConnectionsPerPartition",        pool.setMinConnectionsPerPartition)
        setIfDefinedI("partitionCount",                    pool.setPartitionCount)
        setIfDefinedI("poolAvailabilityThreshold",         pool.setPoolAvailabilityThreshold)
        setIfDefinedS("poolName",                          pool.setPoolName)
        setIfDefinedL("queryExecuteTimeLimitInMs",         pool.setQueryExecuteTimeLimitInMs)
        setIfDefinedS("serviceOrder",                      pool.setServiceOrder)
        setIfDefinedI("statementsCacheSize",               pool.setStatementsCacheSize)
        setIfDefinedB("statisticsEnabled",                 pool.setStatisticsEnabled)
        setIfDefinedB("transactionRecoveryEnabled",        pool.setTransactionRecoveryEnabled)

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
