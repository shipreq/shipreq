package shipreq.taskman.server.app

import java.util.Properties
import scalaz.effect.IO
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.log.{HasLogger, LogCfg}
import shipreq.base.util.{JPropertiesValueReader, Props, RunMode}
import shipreq.taskman.server.TaskmanCtx

private[app] trait MainTemplate extends HasLogger {

  implicit def scope = GlobalScope

  lazy val runMode: RunMode = {
    val runMode = RunMode.Current
    LogCfg.Logback.init(runMode)
    log.info.z(s"Run mode: $runMode")
    runMode
  }

  lazy val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
  lazy val propsR = JPropertiesValueReader(props)

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
        ctx <- IO(new TaskmanCtx(db.io, props, propsR))
        a <- f(ctx) ensuring ctx.shutdown()
      } yield a
    )
}
