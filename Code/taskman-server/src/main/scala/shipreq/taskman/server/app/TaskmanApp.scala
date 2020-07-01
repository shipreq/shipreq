package shipreq.taskman.server.app

import cats.effect.IOApp
import scalaz.syntax.applicative._
import shipreq.base.db._
import shipreq.base.util.FxModule._
import shipreq.base.util.Props
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.{TaskmanConfig, TaskmanCtx}

private[app] trait TaskmanApp extends IOApp with HasLogger {

  protected def initDb(dbCfg: DbConfig): Fx[DbAccessor] = {
//    import shipreq.base.ops.{JdbcLogging, SqlTracer}
//    val sqlTracer: SqlTracer = JdbcLogging
//    if (cfg.server.prometheus.enabled && cfg.server.prometheus.jdbc)
//      sqlTracer = sqlTracer compose JdbcMetrics.sqlTracer("webapp")
//    dbCfg.modifyHikariDataSource(sqlTracer.inject)
    DbAccessor.fromCfg(dbCfg)
  }

  protected def withDatabase[A](f: (DbAccessor, XA) => Fx[A]): Fx[A] = {
    for {
      (cfg, report) <- DbConfig.config.withReport.run(Props.sources).map(_.getOrDie())
      _             <- Fx(logger.info(s"Config report:\n${report.full}"))
      db            <- initDb(cfg)
      _             <- db.verifyConnectivity
      _             <- db.migrator.migrate[Fx]
      a             <- db.xa.use(f(db, _))
    } yield a
  }

  protected def withTaskmanCtx[A](f: TaskmanCtx => Fx[A]): Fx[A] = {
    val readConfig = (DbConfig.config tuple TaskmanConfig.config).withReport.run(Props.sources)

    for {
      ((dbCfg, taskmanCfg), report) <- readConfig.map(_.getOrDie())
      _                             <- Fx(logger.info(s"Config report:\n${report.full}"))
      db                            <- initDb(dbCfg)
      _                             <- db.verifyConnectivity
      _                             <- db.migrator.migrate[Fx]
      a                             <- db.xa.flatMap(TaskmanCtx(db, taskmanCfg, _)).use(f)
    } yield a
  }

}
