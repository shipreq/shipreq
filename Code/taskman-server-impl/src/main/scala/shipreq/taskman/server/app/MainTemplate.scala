package shipreq.taskman.server.app

import java.util.Properties
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.{ErrorOr, JPropertiesValueReader, Props, RunMode}
import shipreq.taskman.server.{TaskmanCtx, Db}
import shipreq.base.util.log.{LogCfg, HasLogger}

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

  def withDatabase[A](f: Db => A): A = {
    val db = new Db(propsR)
    db.init()
    try
      f(db)
    finally
      ErrorOr.safe(db.shutdown()).leftMap(e => log.error(e, "Error closing database connections."))
  }

  def withTaskmanCtx[A](f: TaskmanCtx => A): A =
    withDatabase { db =>
      val ctx = new TaskmanCtx(db.slick, props, propsR)
      try
        f(ctx)
      finally
        ErrorOr.safe(ctx.shutdown()).leftMap(e => log.error(e, "Error shutting down ctx."))
    }
}
