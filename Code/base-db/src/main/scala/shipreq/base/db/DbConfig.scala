package shipreq.base.db

import ch.qos.logback.classic.Level
import com.zaxxer.hikari.HikariConfig
import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config._
import org.postgresql.ds.PGSimpleDataSource
import scalaz.syntax.apply._
import scalaz.{-\/, \/, \/-}

final case class DbConfig(
  pgDataSource: PGSimpleDataSource,
  hikariConfig: HikariConfig,
  schema      : Option[String])

object DbConfig {

  def config: Config[DbConfig] = {

    val schemaCfg: Config[Option[String]] =
      Config.get[String]("schema")

    val schemaSearchPathCfg: Config[(Option[String], Option[String])] =
      schemaCfg tuple Config.get[String]("search_path")

    def ifSchemaValid[A](s: String, a: => A): String \/ A =
      if (s contains "-") -\/("PostgreSQL doesn't allow dashes in schema names.") else \/-(a)

    val pgCurrentSchema: Config[PGSimpleDataSource => Unit] =
      schemaSearchPathCfg mapAttempt {
        case (Some(s), None) => ifSchemaValid(s, _.setCurrentSchema(s))
        case _               => \/-(_ => ())
      }

    // If search path, set here in Hikari
    val hikariSearchPath: Config[HikariConfig => Unit] =
      schemaSearchPathCfg mapAttempt {
        case (Some(_), Some(_)) => -\/("You can't set both the DB schema and search_path.")
        case (None,    Some(s)) => ifSchemaValid(s, _.setConnectionInitSql(s"SET search_path TO $s"))
        case (Some(_), None)
           | (None,    None)    => \/-(_ => ())
      }

    val pgdsCfg: Config[PGSimpleDataSource] =
      (
        Config.need[String]("database") |@|
        Config.need[String]("username") |@|
        Config.need[String]("password") |@|
        Config.consumerFn[PGSimpleDataSource](
          _ => pgCurrentSchema,
          _.getOrUse("host", _.setServerName)("localhost"),
          _.get("appname"               , _.setApplicationName),
          _.get("binaryTransfer"        , _.setBinaryTransfer),
          _.get("binaryTransferDisable" , _.setBinaryTransferDisable),
          _.get("binaryTransferEnable"  , _.setBinaryTransferEnable),
          _.get("compatible"            , _.setCompatible),
          _.get("disableColumnSanitiser", _.setDisableColumnSanitiser),
          _.get("loginTimeout"          , _.setLoginTimeout),
          _.get("logLevel"              , p => (l: String) => p.setLogLevel(Level.valueOf(l.toUpperCase).toInt)),
          _.get("port"                  , _.setPortNumber),
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

    val hikariCfg: Config[HikariConfig] =
      Config.consumerFn[HikariConfig](
        _ => hikariSearchPath,
        _.get("allowPoolSuspension"   , _.setAllowPoolSuspension),
        _.get("catalog"               , _.setCatalog),
        _.get("connectionInitSql"     , _.setConnectionInitSql),
        _.get("connectionTestQuery"   , _.setConnectionTestQuery),
        _.get("connectionTimeout"     , _.setConnectionTimeout),
        _.get("idleTimeout"           , _.setIdleTimeout),
        _.get("initializationFailFast", _.setInitializationFailFast),
        _.get("isolateInternalQueries", _.setIsolateInternalQueries),
        _.get("leakDetectionThreshold", _.setLeakDetectionThreshold),
        _.get("maxLifetime"           , _.setMaxLifetime),
        _.get("maximumPoolSize"       , _.setMaximumPoolSize),
        _.get("minimumIdle"           , _.setMinimumIdle),
        _.get("poolName"              , _.setPoolName),
        _.get("registerMbeans"        , _.setRegisterMbeans),
        _.get("transactionIsolation"  , _.setTransactionIsolation),
        _.get("validationTimeout"     , _.setValidationTimeout))
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
