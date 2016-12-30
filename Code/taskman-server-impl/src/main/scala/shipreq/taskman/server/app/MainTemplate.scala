package shipreq.taskman.server.app

import java.util.Properties
import scalaz.effect.IO
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.log.{HasLogger, LogCfg}
import shipreq.base.util.{ErrorOr, JPropertiesValueReader, Props, RunMode}
import shipreq.taskman.server.TaskmanCtx

private[app] trait MainTemplate extends HasLogger {

  implicit def scope = GlobalScope

  lazy val runMode: RunMode = {
    implicit val rmr: Retriever[RunMode] = RunMode.retrieverFromSysProps
    val runMode: RunMode = tryNeed("run.mode", RunMode.detectFromStackTrace())
    LogCfg.Logback.init(runMode)
    log.info.z(s"Run mode: $runMode")
    runMode
  }

  lazy val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
  lazy val propsR = JPropertiesValueReader(props)

//  def withDatabase[M[_] : Catchable : Capture : Monad, A](f: DbAccess[M] => M[A]): M[A] =
//    for {
//      cfg <- Monad[M] point ErrorOr.require_!(DbConfig.read(propsR))
//      _ <- DbAccess.run(cfg)(f)
//    } yield ()

  def withDatabase[A](f: DbAccess => IO[A]): IO[A] =
    for {
      cfg <- IO(ErrorOr.require_!(DbConfig.read(propsR)))
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
