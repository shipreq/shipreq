package shipreq.base
package db

import ch.qos.logback.classic.Level
import com.jolbox.bonecp.BoneCPDataSource
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory
import scala.slick.session.Database

import util.ExternalValueReader._

class BaseDbConnection(setDsDefaults: BoneCPDataSource => BoneCPDataSource = identity)(
  implicit _s: Retriever[String], _i: Retriever[Int], _l: Retriever[Long], _b: Retriever[Boolean]) {

  protected val log = LoggerFactory.getLogger(getClass)

  // ===================================================================================================================
  // Connection

  protected def loadDataSource(): Either[String, BoneCPDataSource] = {
    implicit val scope = scopeByNS("db")
    for {
      database <- get[String]("database").right
      username <- get[String]("username").right
      password <- get[String]("password").right
    } yield {
      val ds = new PGSimpleDataSource
      ds.setDatabaseName(database)
      ds.setUser(username)
//      ds.setPassword(password)
      ds.setDisableColumnSanitiser(true)
      tryUse("appname"                )(ds.setApplicationName)
      tryUse("binary_transfer"        )(ds.setBinaryTransfer)
      tryUse("binary_transfer_disable")(ds.setBinaryTransferDisable)
      tryUse("binary_transfer_enable" )(ds.setBinaryTransferEnable)
      tryUse("compatible"             )(ds.setCompatible)
      tryUse("disableColumnSanitiser" )(ds.setDisableColumnSanitiser)
      tryUse("login_timeout"          )(ds.setLoginTimeout)
      tryUse("port"                   )(ds.setPortNumber)
      tryUse("prepare_threshold"      )(ds.setPrepareThreshold)
      tryUse("protocol_ver"           )(ds.setProtocolVersion)
      tryUse("recv_buffer_size"       )(ds.setReceiveBufferSize)
      tryUse("send_buffer_size"       )(ds.setSendBufferSize)
      tryUse("host"                   )(ds.setServerName)
      tryUse("socket_timeout"         )(ds.setSocketTimeout)
      tryUse("ssl"                    )(ds.setSsl)
      tryUse("ssl_factory"            )(ds.setSslfactory)
      tryUse("tcp_keep_alive"         )(ds.setTcpKeepAlive)
      tryUse("unknown_length"         )(ds.setUnknownLength)
      tryGet[String]("log_level", "loglevel").right.foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      {
        implicit val scope = scopeByNS("db.pool")
        val pool = {
          val pool = new BoneCPDataSource()
          pool.setDefaultTransactionIsolation("READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
          pool.setDefaultAutoCommit(true)
          pool.setLogStatementsEnabled(false)
          setDsDefaults(pool)
        }
        pool.setJdbcUrl(ds.getUrl)
        pool.setUsername(username)
        pool.setPassword(password)

        tryUse("acquireIncrement"                 )(pool.setAcquireIncrement)
        tryUse("acquireRetryAttempts"             )(pool.setAcquireRetryAttempts)
        tryUse("acquireRetryDelayInMs"            )(pool.setAcquireRetryDelayInMs)
        tryUse("closeConnectionWatch"             )(pool.setCloseConnectionWatch)
        tryUse("closeConnectionWatchTimeoutInMs"  )(pool.setCloseConnectionWatchTimeoutInMs)
        tryUse("connectionHookClassName"          )(pool.setConnectionHookClassName)
        tryUse("connectionTestStatement"          )(pool.setConnectionTestStatement)
        tryUse("connectionTimeoutInMs"            )(pool.setConnectionTimeoutInMs)
        tryUse("defaultCatalog"                   )(pool.setDefaultCatalog)
        tryUse("disableConnectionTracking"        )(pool.setDisableConnectionTracking)
        tryUse("disableJMX"                       )(pool.setDisableJMX)
        tryUse("externalAuth"                     )(pool.setExternalAuth)
        tryUse("idleConnectionTestPeriodInMinutes")(pool.setIdleConnectionTestPeriodInMinutes)
        tryUse("idleConnectionTestPeriodInSeconds")(pool.setIdleConnectionTestPeriodInSeconds)
        tryUse("idleMaxAgeInMinutes"              )(pool.setIdleMaxAgeInMinutes)
        tryUse("idleMaxAgeInSeconds"              )(pool.setIdleMaxAgeInSeconds)
        tryUse("initSQL"                          )(pool.setInitSQL)
        tryUse("lazyInit"                         )(pool.setLazyInit)
        tryUse("logStatementsEnabled"             )(pool.setLogStatementsEnabled)
        tryUse("maxConnectionAgeInSeconds"        )(pool.setMaxConnectionAgeInSeconds)
        tryUse("maxConnectionsPerPartition"       )(pool.setMaxConnectionsPerPartition)
        tryUse("minConnectionsPerPartition"       )(pool.setMinConnectionsPerPartition)
        tryUse("partitionCount"                   )(pool.setPartitionCount)
        tryUse("poolAvailabilityThreshold"        )(pool.setPoolAvailabilityThreshold)
        tryUse("poolName"                         )(pool.setPoolName)
        tryUse("queryExecuteTimeLimitInMs"        )(pool.setQueryExecuteTimeLimitInMs)
        tryUse("serviceOrder"                     )(pool.setServiceOrder)
        tryUse("statementsCacheSize"              )(pool.setStatementsCacheSize)
        tryUse("statisticsEnabled"                )(pool.setStatisticsEnabled)
        tryUse("transactionRecoveryEnabled"       )(pool.setTransactionRecoveryEnabled)

        pool
      }
    }
  }

  private def getDatabaseName(ds: BoneCPDataSource): String =
    ds.getJdbcUrl.replaceFirst("\\?.*", "").replaceFirst("^.+//", "")

  val DataSource = {
    val ds = loadDataSource() match {
      case Right(v) => v
      case Left(e)  => throw new RuntimeException(e)
    }
    log.info("Connecting to database: " + getDatabaseName(ds))
    log.debug("Database pool config: " + ds.getConfig)
    ds.getConfig
    ds.getConnection().close() // test the data source validity
    ds
  }

  val DatabaseName = getDatabaseName(DataSource)

  // ===================================================================================================================
  // Access

  val Slick = Database.forDataSource(DataSource)
}
