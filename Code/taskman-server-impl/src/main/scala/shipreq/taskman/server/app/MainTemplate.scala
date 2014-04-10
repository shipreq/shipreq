package shipreq.taskman.server.app

import java.util.Properties
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.{ErrorOr, JPropertiesValueReader, Props, RunMode, Logger}
import shipreq.taskman.server.{TaskmanCtx, Db}

private[app] trait MainTemplate extends Logger {

  implicit def scope = GlobalScope

  lazy val runMode: RunMode = {
    implicit val rmr: Retriever[RunMode] = RunMode.retrieverFromSysProps
    val runMode: RunMode = tryNeed("run.mode", RunMode.detectFromStackTrace())
    log.info("Run mode: {}", runMode)
    runMode
  }

  lazy val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
  lazy val propsR = JPropertiesValueReader(props)

  def withDatabase[A](f: Db => A): A = {
    val db = new Db(propsR)
    db.init()
    try {
      f(db)
    } finally {
      ErrorOr.safe(db.shutdown()).leftMap(e => log.error("Error closing database connections.", e.throwable))
    }
  }

  def withTaskmanCtx[A](f: TaskmanCtx => A): A =
    withDatabase(db =>
      f(new TaskmanCtx(db.slick, props, propsR))
    )
}
