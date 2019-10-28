package shipreq.taskman.server.app

import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.FxModule._
import shipreq.base.util.Props
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.{TaskmanConfig, TaskmanCtx}
import DbAccess.fxCapture

private[app] trait MainTemplate extends HasLogger {

  def withDatabase[A](f: DbAccess => Fx[A]): Fx[A] =
    for {
      tmp          <- DbConfig.config.withReport.run(Props.sources).map(_.getOrDie)
      (cfg, report) = tmp
      _            <- Fx(logger.info(s"Config report:\n${report.full}"))
      dbAccess     <- DbAccess.fromCfg(cfg)
      a            <- dbAccess.setupRunShutdown(f(dbAccess))
    } yield a

  def withTaskmanCtx[A](f: TaskmanCtx => Fx[A]): Fx[A] = {
    val readConfig = (DbConfig.config tuple TaskmanConfig.config).withReport.run(Props.sources)

    def onShutDown(dbAccess: DbAccess, taskmanCfg: TaskmanConfig) =
      for {
      ctx <- Fx(TaskmanCtx(dbAccess, taskmanCfg))
      a <- f(ctx) andFinally ctx.shutdown
    } yield a

    for {
      tmp                          <- readConfig.map(_.getOrDie)
      ((dbCfg, taskmanCfg), report) = tmp
      _                            <- Fx(logger.info(report.full))
      dbAccess                     <- DbAccess.fromCfg(dbCfg)
      a                            <- dbAccess.setupRunShutdown(onShutDown(dbAccess, taskmanCfg))
    } yield a
  }
}
