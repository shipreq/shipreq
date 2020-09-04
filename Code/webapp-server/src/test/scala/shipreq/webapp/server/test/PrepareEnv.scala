package shipreq.webapp.server.test

import java.time.Duration
import org.redisson.Redisson
import shipreq.base.db.{DbAccessor, XA}
import shipreq.base.test.BaseTestUtil.onceUnit
import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.base.util.FxModule._
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.app.{Global, ServerConfig}
import shipreq.webapp.server.db.StatRecorder
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
    config       = cfg,
    runDB        = null,
    logic        = null,
    metrics      = MetricsLogic.const(Fx.unit),
    ops          = null,
    security     = null,
    ssr          = SsrOff.prepared,
    statRecorder = StatRecorder.Off,
    taskman      = null,
    trace        = TraceLogic.off)

  def global() =
    Global.Instance

  val lift: () => Unit = onceUnit {
    // if (!LiftRules.doneBoot) {
    boot.configureLift(Global.config)
  }

  val dbOnce = onceUnit {
    db()
    TestDb.init()
  }

  def db(): Unit =
    dbVia(TestDb.db)

  def dbVia(db: DbAccessor, xa: Option[XA] = None): Unit =
    Global.modify { g1 =>
      val g2 = Global.full(db, xa, None, SsrOff.prepared, g1.config)
      g1.copy(
        runDB    = g2.runDB,
        logic    = g2.logic,
        ops      = g2.ops,
        ssr      = g2.ssr,
        security = g2.security,
        taskman  = g2.taskman)
    }

  def dbVia(xa: ImperativeXA): Unit =
    dbVia(xa.dbAccessor, Some(xa))

  /** Make sure global() is setup first */
  val routes: () => Unit = onceUnit {
    boot.initRoutes(global())
  }

  lazy val redissonClient =
    global().config.redis match {
      case Some(r) => Redisson.create(r.instance)
      case None    => sys.error("Redis test config not specified.")
    }
}
