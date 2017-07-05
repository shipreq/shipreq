package shipreq.webapp.server.test

import bootstrap.liftweb.AppConfig
import java.time.Duration
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.db.DbInterpreter

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private lazy val cfg = {
    var (appConfig, runMode) = boot.readConfig()
    runMode foreach boot.setRunMode
    appConfig = (AppConfig.server ^|-> ServerConfig.attackFrustrationDelay).set(Duration.ZERO)(appConfig)
    println("webapp-server test config:\n" + appConfig.report.reportUsed)
    appConfig
  }

  private def once[A](a: => A): () => Unit = {
    lazy val o = {a; ()}
    () => o
  }

  Global.Instance = Global(
    config  = cfg.server,
    db      = null,
    logic   = null,
    taskman = null)

  def global() = Global.Instance

  val shiro: () => Unit = once {
    boot.initShiro()
  }

  val lift: () => Unit = once {
    // if (!LiftRules.doneBoot) {
    shiro()
    boot.configureLift()
    boot.preloadTemplates()
  }

  def db(): Unit = {
    TestDb.init()
    TestDb.useInLift()
  }

  lazy val dbAlgebra = new DbInterpreter()(global().config)
}
