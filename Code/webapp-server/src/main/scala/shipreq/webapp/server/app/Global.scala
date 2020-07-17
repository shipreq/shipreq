package shipreq.webapp.server.app

import doobie.ConnectionIO
import java.util.concurrent.{Executors, TimeUnit}
import org.redisson.api.RedissonClient
import scalaz.~>
import shipreq.base.db._
import shipreq.base.util.FxModule._
import shipreq.base.util.ThreadUtils
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.webapp.server.db.{DbInterpreter, StatRecorder}
import shipreq.webapp.server.logic._
import shipreq.webapp.server.redis.{RedisSchema, RedisViaRedisson}
import shipreq.webapp.server.security.SecurityInterpreter
import shipreq.webapp.ssr.SsrAlgebra

final case class Global(config      : ServerConfig,
                        runDB       : ConnectionIO ~> Fx,
                        logic       : ServerLogic[Fx],
                        metrics     : MetricsLogic[Fx],
                        ops         : OpsEndpointInterpreter,
                        security    : Security.Algebra[Fx],
                        ssr         : SsrAlgebra.Prepared[Fx],
                        statRecorder: StatRecorder,
                        taskman     : TaskmanApi[Fx],
                        trace       : TraceInterpreter.ForHttp[Fx]) {

  val analyticsProxy: AnalyticsProxy =
    config.analyticsProxy.build
}

object Global {
  var Instance: Global = _

  @inline
  @nowarn("cat=unused")
  implicit def autoInstance(g: Global.type): Global = Instance

  def modify(f: Global => Global): Unit =
    Instance = f(Instance)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def default(db         : DbAccessor,
              redisClient: Option[RedissonClient],
              ssr        : SsrAlgebra.Prepared[Fx],
              config     : ServerConfig,
             ): Global =
    full(
      db          = db,
      xaOverride  = None,
      redisClient = redisClient,
      ssr         = ssr,
      config      = config,
    )

  def full(db         : DbAccessor,
           xaOverride : Option[XA],
           redisClient: Option[RedissonClient],
           ssr        : SsrAlgebra.Prepared[Fx],
           config     : ServerConfig,
          ): Global = {

    assert(db ne null, "DbAccess is null, sir.")
    import TraceInterpreter.Implicits._

    implicit def configServer   = config.server
    implicit def configSecurity = config.server.security

    implicit val metrics: MetricsLogic[Fx] =
      if (config.server.prometheus.enabled)
        new PrometheusMetrics
      else
        MetricsLogic.const(Fx.unit)

    implicit val traceAlgebra =
      config.server.traceAlgebraFx

    def t[A](name: String)(a: => A): A =
      traceAlgebra.newSpanImpure("Global:" + name)(_ => a)

    t("full") {

      val xa: XA =
        xaOverride.getOrElse {
          val (x, xaShutdown) = t("xa") {
            db.xa.allocated.unsafeRun()
          }
          ThreadUtils.runOnShutdownFx("xa", xaShutdown)
          x
        }

      implicit val trace = t("trace") {
        TraceLogic.on: TraceInterpreter.ForHttp[Fx]
      }

      implicit val runDB = t("runDB") {
        trace.injectDb(xa.transZ)
      }

      implicit val statRecorder = t("statRecorder") {
        StatRecorder(runDB, config.statRecorder)
      }

      implicit val taskman = t("taskman") {
        TaskmanApi.addLogging(TaskmanApiImpl(Some(config.server.taskmanSchema)).trans(runDB))
      }

      implicit val dbAlgebra = t("dbAlgebra") {
        new DbInterpreter()
      }

      implicit val dbForSecurity = t("dbForSecurity") {
        DB.ForSecurity.trans(DbInterpreter.ForSecurity)(runDB)
      }

      implicit val dbForOps = t("dbForOps") {
        DB.ForOps.trans(new DbInterpreter.ForOps(db.databaseName))(runDB)
      }

      implicit val server = t("server") {
        trace.injectServer(ServerInterpreter)
      }

      implicit val ops = t("ops") {
        new OpsEndpointInterpreter()
      }

      implicit val security = t("security") {
        new SecurityInterpreter[Fx]
      }

      implicit val apEvents = t("apEvents") {
        var a = ApplyEventLogic.trusted[Fx]
        a = ApplyEventLogic.withMetricsAndLogging(a, config.server.applyEventThresholdMs)
        a = ApplyEventLogic.traced(a, traceAlgebra)
        a
      }

      implicit val redis: Redis.ProjectAlgebra[Fx] = t("redis") {
        redisClient match {
          case Some(c) =>
            var r: Redis.ProjectAlgebra[Fx] = new RedisViaRedisson(c, RedisSchema.default)
            r = Redis.withMetricsAndLogging(r, metrics)
            r = trace.injectRedis(r)
            r
          case None =>
            useInMemoryRedis()
        }
      }

      val logic = t("logic") {
        ServerLogic.create[ConnectionIO, Fx]
      }

      Global(
        config       = config,
        runDB        = runDB,
        logic        = logic,
        metrics      = metrics,
        ops          = ops,
        security     = security,
        ssr          = ssr,
        statRecorder = statRecorder,
        taskman      = taskman,
        trace        = trace)
      }
    }

  private def useInMemoryRedis() = {
    val redis          = new Redis.InMemory[Fx]
    val threadGroup    = new ThreadGroup("RedisInMemory")
    val timer          = Executors.newSingleThreadScheduledExecutor(new Thread(threadGroup, _, "RedisInMemory"))
    val task: Runnable = () => redis.publishAll.unsafeRun()
    val everyMs        = 1000
    timer.scheduleAtFixedRate(task, everyMs, everyMs, TimeUnit.MILLISECONDS)
    Runtime.getRuntime.addShutdownHook(new Thread(threadGroup, task, "RedisInMemory-shutdown"))
    redis
  }
}