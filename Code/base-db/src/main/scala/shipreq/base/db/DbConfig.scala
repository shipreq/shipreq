package shipreq.base.db

import ch.qos.logback.classic.Level
import com.zaxxer.hikari.HikariConfig
import org.postgresql.ds.PGSimpleDataSource
import shipreq.base.util.{ErrorOr, StringBasedValueReader}
import shipreq.base.util.ExternalValueReader.{get => getEV, Retriever => R, _}

case class DbConfig(hikariCfg: HikariConfig, pgDataSource: PGSimpleDataSource, schema: Option[String])

object DbConfig {

  def PropertyScope = "db"

  def read(s: StringBasedValueReader): ErrorOr[DbConfig] = {
    import s._
    read2
  }

  def read2(implicit _s: R[String], _i: R[Int], _l: R[Long], _b: R[Boolean]): ErrorOr[DbConfig] =
    ErrorOr catchException dataSource

  protected def dataSource(implicit _s: R[String], _i: R[Int], _l: R[Long], _b: R[Boolean]): ErrorOr[DbConfig] = {

    implicit val scope = scopeByNS(PropertyScope)
    for {
      database <- getEV[String]("database")
      username <- getEV[String]("username")
      password <- getEV[String]("password")
    } yield {
      val pgds = new PGSimpleDataSource
      pgds.setDatabaseName(database)
      pgds.setUser(username)
      pgds.setPassword(password)
      pgds.setDisableColumnSanitiser(true)
      tryUse("appname"                )(pgds.setApplicationName)
      tryUse("binary_transfer"        )(pgds.setBinaryTransfer)
      tryUse("binary_transfer_disable")(pgds.setBinaryTransferDisable)
      tryUse("binary_transfer_enable" )(pgds.setBinaryTransferEnable)
      tryUse("compatible"             )(pgds.setCompatible)
      tryUse("disableColumnSanitiser" )(pgds.setDisableColumnSanitiser)
      tryUse("login_timeout"          )(pgds.setLoginTimeout)
      tryUse("port"                   )(pgds.setPortNumber)
      tryUse("prepare_threshold"      )(pgds.setPrepareThreshold)
      tryUse("protocol_ver"           )(pgds.setProtocolVersion)
      tryUse("recv_buffer_size"       )(pgds.setReceiveBufferSize)
      tryUse("send_buffer_size"       )(pgds.setSendBufferSize)
      tryUse("host"                   )(pgds.setServerName)
      tryUse("socket_timeout"         )(pgds.setSocketTimeout)
      tryUse("ssl"                    )(pgds.setSsl)
      tryUse("ssl_factory"            )(pgds.setSslfactory)
      tryUse("tcp_keep_alive"         )(pgds.setTcpKeepAlive)
      tryUse("unknown_length"         )(pgds.setUnknownLength)
      tryGet[String]("log_level", "loglevel").foreach(l => pgds.setLogLevel(Level.valueOf(l.toUpperCase).toInt))

      val schema     = getO[String]("schema")
      val searchPath = getO[String]("search_path")

      // If schema only, set here in Postgres
      (schema, searchPath) match {
        case (Some(s), None) => pgds.setCurrentSchema(s)
        case _               => ()
      }

      {
        implicit val scope = scopeByNS(PropertyScope, "pool")

        val hcfg = new HikariConfig
        hcfg.setTransactionIsolation("TRANSACTION_READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
        hcfg.setAutoCommit(true)
        hcfg.setDataSource(pgds)
        hcfg.setUsername(username)
        hcfg.setPassword(password)

        // If search path, set here in Hikari
        (schema, searchPath) match {
          case (None,    Some(s)) => setSearchPath(s)(hcfg)
          case (Some(_), Some(_)) => sys error "You can't set both the DB schema and search_path."
          case (_,       None)    => ()
        }

        tryUse("allowPoolSuspension"     )(hcfg.setAllowPoolSuspension)
        tryUse("catalog"                 )(hcfg.setCatalog)
        tryUse("connectionInitSql"       )(hcfg.setConnectionInitSql)
        tryUse("connectionTestQuery"     )(hcfg.setConnectionTestQuery)
        tryUse("connectionTimeout"       )(hcfg.setConnectionTimeout)
        tryUse("idleTimeout"             )(hcfg.setIdleTimeout)
        tryUse("initializationFailFast"  )(hcfg.setInitializationFailFast)
        tryUse("isolateInternalQueries"  )(hcfg.setIsolateInternalQueries)
        tryUse("leakDetectionThreshold"  )(hcfg.setLeakDetectionThreshold)
        tryUse("maxLifetime"             )(hcfg.setMaxLifetime)
        tryUse("maximumPoolSize"         )(hcfg.setMaximumPoolSize)
        tryUse("minimumIdle"             )(hcfg.setMinimumIdle)
        tryUse("poolName"                )(hcfg.setPoolName)
        tryUse("registerMbeans"          )(hcfg.setRegisterMbeans)
        tryUse("transactionIsolation"    )(hcfg.setTransactionIsolation)
        tryUse("validationTimeout"       )(hcfg.setValidationTimeout)

        /*
        tryUse("autoCommit"              )(hcfg.setAutoCommit)
        tryUse("dataSource"              )(hcfg.setDataSource)
        tryUse("dataSourceClassName"     )(hcfg.setDataSourceClassName)
        tryUse("dataSourceJNDI"          )(hcfg.setDataSourceJNDI)
        tryUse("dataSourceProperties"    )(hcfg.setDataSourceProperties)
        tryUse("driverClassName"         )(hcfg.setDriverClassName)
        tryUse("jdbc4ConnectionTest"     )(hcfg.setJdbc4ConnectionTest)
        tryUse("jdbcUrl"                 )(hcfg.setJdbcUrl)
        tryUse("metricsTrackerFactory"   )(hcfg.setMetricsTrackerFactory)
        tryUse("metricRegistry"          )(hcfg.setMetricRegistry)
        tryUse("healthCheckRegistry"     )(hcfg.setHealthCheckRegistry)
        tryUse("healthCheckProperties"   )(hcfg.setHealthCheckProperties)
        tryUse("password"                )(hcfg.setPassword)
        tryUse("readOnly"                )(hcfg.setReadOnly)
        tryUse("scheduledExecutorService")(hcfg.setScheduledExecutorService)
        tryUse("threadFactory"           )(hcfg.setThreadFactory)
        tryUse("username"                )(hcfg.setUsername)
        */

        DbConfig(hcfg, pgds, schema)
      }
    }
  }

  private def setSearchPath(path: String): HikariConfig => Unit = {
    if (path.contains("-"))
      throw new IllegalArgumentException("PostgreSQL doesn't allow dashes in schema names.")
    runOnConnectionAcquire(s"SET search_path TO $path")
  }

  private def runOnConnectionAcquire(sql: String): HikariConfig => Unit =
    _.setConnectionInitSql(sql)
}
