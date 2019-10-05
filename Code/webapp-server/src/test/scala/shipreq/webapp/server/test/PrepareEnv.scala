package shipreq.webapp.server.test

import java.time.Duration
import org.redisson.Redisson
import shipreq.base.test.BaseTestUtil.onceUnit
import shipreq.base.util.FxModule._
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.app.{Global, ServerConfig}
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.{MetricsLogic, TraceLogic}
import shipreq.webapp.ssr.SsrOff

object PrepareEnv {
  private val boot = new bootstrap.liftweb.Boot

  private val cfg = {
    var (appConfig, runMode, _) = boot.readConfig()
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
    ssr      = SsrOff.prepared,
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

  lazy val redissonClient =
    global().config.redis match {
      case Some(r) => Redisson.create(r.instance)
      case None    => sys.error("Redis test config not specified.")
    }
}
