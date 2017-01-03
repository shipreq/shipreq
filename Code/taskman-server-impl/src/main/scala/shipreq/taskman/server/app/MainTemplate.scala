package shipreq.taskman.server.app

import scalaz.effect.IO
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
      x <- IO(DbConfig.config.withReport.run(runMode.configSources).getOrDie())
      (cfg, report) = x
      _ <- IO.putStrLn(report.report)
      dbAccess = DbAccess.fromCfg(cfg)
      a <- dbAccess.setupRunShutdown(f(dbAccess))
    } yield a

  def withTaskmanCtx[A](f: TaskmanCtx => IO[A]): IO[A] =
    withDatabase[A](db =>
      for {
        cfg <- IO(TaskmanConfig.config.run(runMode.configSources).getOrDie())
        ctx <- IO(TaskmanCtx(db, cfg))
        a <- f(ctx) ensuring ctx.shutdown
      } yield a
    )
}
