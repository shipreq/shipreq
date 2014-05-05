package shipreq.base.db

import ch.qos.logback.classic.Level
import com.jolbox.bonecp.hooks.{AbstractConnectionHook, ConnectionHook}
import com.jolbox.bonecp.{ConnectionHandle, BoneCPDataSource}
import org.postgresql.ds.PGSimpleDataSource
import shipreq.base.util.ErrorOr
import shipreq.base.util.ExternalValueReader.{get => getEV, Retriever => R, _}
import shipreq.base.util.log.HasLogger

case class DatabaseConnection(host: String, name: String, schema: Option[String], ds: BoneCPDataSource) {
  def desc = s"$host/$name" + schema.map(":" + _).getOrElse("")
}

object DatabaseConnection extends HasLogger {

  type C = BoneCPDataSource => BoneCPDataSource

  def PropertyScope = "db"

  def establish_!(defaults: C = identity)(implicit _s: R[String], _i: R[Int], _l: R[Long], _b: R[Boolean]): DatabaseConnection = {
    val c = ErrorOr.require_!(get(defaults))
    verify_!(c)
    c
  }

  def get(defaults: C = identity)(implicit _s: R[String], _i: R[Int], _l: R[Long], _b: R[Boolean]): ErrorOr[DatabaseConnection] =
    ErrorOr.catchException {
      dataSource(defaults).map {
        case (schema, pg, ds) =>
          DatabaseConnection(pg.getServerName, pg.getDatabaseName, schema, ds)
    }
  }

  protected def dataSource(defaults: C = identity)(implicit _s: R[String], _i: R[Int], _l: R[Long], _b: R[Boolean])
  : ErrorOr[(Option[String], PGSimpleDataSource, BoneCPDataSource)] = {

    implicit val scope = scopeByNS(PropertyScope)
    for {
      database <- getEV[String]("database")
      username <- getEV[String]("username")
      password <- getEV[String]("password")
    } yield {
      val ds = new PGSimpleDataSource
      ds.setDatabaseName(database)
      ds.setUser(username)
      // ds.setPassword(password)
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
      tryGet[String]("log_level", "loglevel").foreach(l => ds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      val schema = getO[String]("schema")
      val searchPath= getO[String]("search_path")

      {
        implicit val scope = scopeByNS(PropertyScope, "pool")
        val pool = {
          val pool = new BoneCPDataSource()
          pool.setDefaultTransactionIsolation("READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
          pool.setDefaultAutoCommit(true)
          pool.setLogStatementsEnabled(false)
          defaults(pool)
        }
        pool.setJdbcUrl(ds.getUrl)
        pool.setUsername(username)
        pool.setPassword(password)

        (schema, searchPath) match {
          case (_,          Some(path)) => setSearchPath(path)(pool)
          case (Some(path), None)       => setSearchPath(path)(pool)
          case (None      , None)       =>
        }

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

        (schema, ds, pool)
      }
    }
  }

  def verify_!(c: DatabaseConnection): Unit = {
    log.info.z(s"Connecting to database: ${c.desc}")
    log.debug.z(s"Database pool config: ${c.ds.getConfig}")
    c.ds.getConnection().close() // test the data source validity
  }

  def setSearchPath(path: String) = {
    if (path.contains("-")) throw new IllegalArgumentException("PostgreSQL doesn't allow dashes in schema names.")
    runOnConnectionAcquire(s"SET search_path TO $path")
  }

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
}
