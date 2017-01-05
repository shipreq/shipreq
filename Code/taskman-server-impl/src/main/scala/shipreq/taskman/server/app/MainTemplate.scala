package shipreq.taskman.server.app

import scalaz.effect.IO
import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.RunMode
import shipreq.base.util.log.{HasLogger, LogCfg}
import shipreq.taskman.server.{TaskmanConfig, TaskmanCtx}

private[app] trait MainTemplate extends HasLogger {

  lazy val runMode: RunMode = {
    val runMode = RunMode.Current
    LogCfg.Logback.init(runMode)
    log.info.z(s"Run mode: $runMode")
    runMode
  }

  def withDatabase[A](f: DbAccess => IO[A]): IO[A] =
    for {
      tmp <- IO(DbConfig.config.withReport.run(runMode.configSources).getOrDie())
      (cfg, report) = tmp
      _ <- IO.putStrLn(report.report)
      dbAccess = DbAccess.fromCfg(cfg)
      a <- dbAccess.setupRunShutdown(f(dbAccess))
    } yield a

  def withTaskmanCtx[A](f: TaskmanCtx => IO[A]): IO[A] =
    for {
      tmp <- IO((DbConfig.config tuple TaskmanConfig.config).withReport.run(runMode.configSources).getOrDie())
      ((dbCfg, taskmanCfg), report) = tmp
      dbAccess = DbAccess.fromCfg(dbCfg)
      a <- dbAccess.setupRunShutdown(
        for {
          ctx <- IO(TaskmanCtx(dbAccess, taskmanCfg, report))
          a <- f(ctx) ensuring ctx.shutdown
        } yield a
      )
    } yield a
}
