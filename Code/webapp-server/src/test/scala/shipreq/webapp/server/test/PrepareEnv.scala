package shipreq.webapp.server.test

import bootstrap.liftweb.BootConfig
import java.time.Duration
import shipreq.base.test.BaseTestUtil.onceUnit
import shipreq.base.util.FxModule.Fx
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.{MetricsLogic, TraceLogic}

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private val cfg = {
    var (appConfig, runMode) = boot.readConfig()
    runMode foreach boot.setRunMode
    val attackDelayL = BootConfig.server ^|-> ServerConfig.security ^|-> ServerConfig.Security.attackFrustrationDelay
    appConfig = attackDelayL.set(Duration.ZERO)(appConfig)
    // println("webapp-server test config:\n" + appConfig.report.reportUsed)
    appConfig
  }

  Global.Instance = Global(
    config   = cfg.server,
    db       = null,
    logic    = null,
    metrics  = MetricsLogic.const(Fx.unit),
    ops      = null,
    security = null,
    security2 = null,
    taskman  = null,
    trace    = TraceLogic.off)

  def global() = Global.Instance

  val shiro: () => Unit = onceUnit {
    boot.initShiro()
  }

  val lift: () => Unit = onceUnit {
    // if (!LiftRules.doneBoot) {
    shiro()
    boot.configureLift()
  }

  def db(): Unit = {
    TestDb.init()
    TestDb.useInLift()
  }

  val routes: () => Unit = onceUnit {
    db()
    boot.initRoutes(global())
  }

  lazy val dbAlgebra =
    new DbInterpreter()(global().config.security)

  lazy val security = {
    PrepareEnv.shiro()
    db()
    global().security
  }
}
