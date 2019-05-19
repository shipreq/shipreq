package shipreq.webapp.server.test

import java.time.Duration
import shipreq.base.test.BaseTestUtil.onceUnit
import shipreq.base.util.FxModule.Fx
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.app.{Global, ServerConfig}
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.{MetricsLogic, TraceLogic}

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private val cfg = {
    var (appConfig, runMode, configReport) = boot.readConfig()
    runMode foreach boot.setRunMode
    val attackDelayL = ServerConfig.server ^|-> ServerLogicConfig.security ^|-> ServerLogicConfig.Security.attackFrustrationDelay
    appConfig = attackDelayL.set(Duration.ZERO)(appConfig)
    // println("webapp-server test config:\n" + configReport.reportUsed)
    appConfig
  }

  Global.Instance = Global(
    config   = cfg,
    db       = null,
    logic    = null,
    metrics  = MetricsLogic.const(Fx.unit),
    ops      = null,
    security = null,
    taskman  = null,
    trace    = TraceLogic.off)

  def global() = Global.Instance

  val lift: () => Unit = onceUnit {
    // if (!LiftRules.doneBoot) {
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
    new DbInterpreter()(global().config.server.security)

  lazy val security = {
    db()
    global().security
  }
}
