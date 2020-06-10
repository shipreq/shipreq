package shipreq.base.db

import com.zaxxer.hikari.HikariConfig
import japgolly.clearconfig._
import javax.sql.DataSource
import org.postgresql.ds.PGSimpleDataSource
import scalaz.syntax.apply._
import scalaz.{-\/, \/, \/-}

final case class DbConfig(
  pgDataSource: PGSimpleDataSource,
  hikariConfig: HikariConfig,
  schema      : Option[String]) {

  def poolSize: Int =
    hikariConfig.getMaximumPoolSize

  def modifyHikariDataSource(f: DataSource => DataSource): Unit =
    hikariConfig.setDataSource(f(hikariConfig.getDataSource))
}

object DbConfig {

  def config: ConfigDef[DbConfig] = {

    val schemaCfg: ConfigDef[Option[String]] =
      ConfigDef.get[String]("schema")

    val schemaSearchPathCfg: ConfigDef[(Option[String], Option[String])] =
      schemaCfg tuple ConfigDef.get[String]("search_path")

    def ifSchemaValid[A](s: String, a: => A): String \/ A =
      if (s contains "-") -\/("PostgreSQL doesn't allow dashes in schema names.") else \/-(a)

    val pgCurrentSchema: ConfigDef[PGSimpleDataSource => Unit] =
      schemaSearchPathCfg mapAttempt {
        case (Some(s), None) => ifSchemaValid(s, _.setCurrentSchema(s))
        case _               => \/-(_ => ())
      }

    // If search path, set here in Hikari
    val hikariSearchPath: ConfigDef[HikariConfig => Unit] =
      schemaSearchPathCfg mapAttempt {
        case (Some(_), Some(_)) => -\/("You can't set both the DB schema and search_path.")
        case (None,    Some(s)) => ifSchemaValid(s, _.setConnectionInitSql(s"SET search_path TO $s"))
        case (Some(_), None)
           | (None,    None)    => \/-(_ => ())
      }

    val pgdsCfg: ConfigDef[PGSimpleDataSource] =
      (
        ConfigDef.need[String]("database") |@|
        ConfigDef.need[String]("username") |@|
        ConfigDef.need[String]("password") |@|
        ConfigDef.consumerFn[PGSimpleDataSource](
          _ => pgCurrentSchema,
          _.getOrUse("host", x => (s: String) => x.setServerNames(Array(s)))("localhost"),
          _.get("appname"               , _.setApplicationName),
          _.get("binaryTransfer"        , _.setBinaryTransfer),
          _.get("binaryTransferDisable" , _.setBinaryTransferDisable),
          _.get("binaryTransferEnable"  , _.setBinaryTransferEnable),
          _.get("disableColumnSanitiser", _.setDisableColumnSanitiser),
          _.get("loginTimeout"          , _.setLoginTimeout),
          _.get("port"                  , x => (p: Int) => x.setPortNumbers(Array(p))),
          _.get("prepareThreshold"      , _.setPrepareThreshold),
          _.get("protocolVersion"       , _.setProtocolVersion),
          _.get("receiveBufferSize"     , _.setReceiveBufferSize),
          _.get("sendBufferSize"        , _.setSendBufferSize),
          _.get("socketTimeout"         , _.setSocketTimeout),
          _.get("ssl"                   , _.setSsl),
          _.get("sslfactory"            , _.setSslfactory),
          _.get("tcpKeepAlive"          , _.setTcpKeepAlive),
          _.get("unknownLength"         , _.setUnknownLength))
        ) { (database, username, password, fn) =>
        val pgds = new PGSimpleDataSource
        pgds.setDatabaseName(database)
        pgds.setUser(username)
        pgds.setPassword(password)
        pgds.setDisableColumnSanitiser(true)
        fn(pgds)
        pgds
      }

    val hikariCfg: ConfigDef[HikariConfig] =
      ConfigDef.consumerFn[HikariConfig](
        _ => hikariSearchPath,
        _.get("allowPoolSuspension"      , _.setAllowPoolSuspension),
        _.get("catalog"                  , _.setCatalog),
        _.get("connectionInitSql"        , _.setConnectionInitSql),
        _.get("connectionTestQuery"      , _.setConnectionTestQuery),
        _.get("connectionTimeout"        , _.setConnectionTimeout),
        _.get("idleTimeout"              , _.setIdleTimeout),
        _.get("initializationFailTimeout", _.setInitializationFailTimeout),
        _.get("isolateInternalQueries"   , _.setIsolateInternalQueries),
        _.get("leakDetectionThreshold"   , _.setLeakDetectionThreshold),
        _.get("maxLifetime"              , _.setMaxLifetime),
        _.get("maximumPoolSize"          , _.setMaximumPoolSize),
        _.get("minimumIdle"              , _.setMinimumIdle),
        _.get("poolName"                 , _.setPoolName),
        _.get("registerMbeans"           , _.setRegisterMbeans),
        _.get("transactionIsolation"     , _.setTransactionIsolation),
        _.get("validationTimeout"        , _.setValidationTimeout))
        .withPrefix("pool.")
        .map { fn =>
          val hcfg = new HikariConfig
          hcfg.setTransactionIsolation("TRANSACTION_READ_COMMITTED") // Shouldn't be doing repeated-reads anyway
          hcfg.setAutoCommit(true)
          fn(hcfg)
          hcfg
        }

    (pgdsCfg |@| hikariCfg |@| schemaCfg) ((pgds, hcfg, schema) => {
      hcfg.setDataSource(pgds)
      hcfg.setUsername(pgds.getUser)
      hcfg.setPassword(pgds.getPassword)
      DbConfig(pgds, hcfg, schema)
    }).withPrefix("db.")
  }

  // _.get("autoCommit"              , _.setAutoCommit),
  // _.get("dataSource"              , _.setDataSource),
  // _.get("dataSourceClassName"     , _.setDataSourceClassName),
  // _.get("dataSourceJNDI"          , _.setDataSourceJNDI),
  // _.get("dataSourceProperties"    , _.setDataSourceProperties),
  // _.get("driverClassName"         , _.setDriverClassName),
  // _.get("jdbc4ConnectionTest"     , _.setJdbc4ConnectionTest),
  // _.get("jdbcUrl"                 , _.setJdbcUrl),
  // _.get("metricsTrackerFactory"   , _.setMetricsTrackerFactory),
  // _.get("metricRegistry"          , _.setMetricRegistry),
  // _.get("healthCheckRegistry"     , _.setHealthCheckRegistry),
  // _.get("healthCheckProperties"   , _.setHealthCheckProperties),
  // _.get("password"                , _.setPassword),
  // _.get("readOnly"                , _.setReadOnly),
  // _.get("scheduledExecutorService", _.setScheduledExecutorService),
  // _.get("threadFactory"           , _.setThreadFactory),
  // _.get("username"                , _.setUsername),
}
