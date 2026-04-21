package shipreq.webapp.server.config

import cats.~>
import doobie.ConnectionIO
import java.util.concurrent.{Executors, TimeUnit}
import org.redisson.api.RedissonClient
import shipreq.base.db._
import shipreq.base.util.FxModule._
import shipreq.base.util.ThreadUtils
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.webapp.server.db.{DbInterpreter, StatRecorder}
import shipreq.webapp.server.interpreter._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.event.ApplyEventAlgebra
import shipreq.webapp.server.logic.inmem.InMemoryRedis
import shipreq.webapp.server.logic.logic.ServerLogic
import shipreq.webapp.server.redis.{RedisSchema, RedisViaRedisson}
import shipreq.webapp.server.util.AnalyticsProxy
import shipreq.webapp.ssr.SsrAlgebra

final case class Global(config      : ServerConfig,
                        cryptoD     : Crypto[ConnectionIO],
                        cryptoF     : Crypto[Fx],
                        runDB       : ConnectionIO ~> Fx,
                        logic       : ServerLogic[Fx],
                        metrics     : MetricsAlgebra[Fx],
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
    import shipreq.webapp.server.interpreter.TraceInterpreter.Implicits._

    implicit def configServer   = config.server
    implicit def configSecurity = config.server.security

    implicit val metrics: MetricsAlgebra[Fx] =
      if (config.server.prometheus.enabled)
        new PrometheusMetrics
      else
        MetricsAlgebra.const(Fx.unit)

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

      implicit val (cryptoD, cryptoF) = t("crypto") {
        (Crypto.default[ConnectionIO], Crypto.default[Fx])
      }

      implicit val trace = t("trace") {
        TraceAlgebra.on: TraceInterpreter.ForHttp[Fx]
      }

      implicit val runDB = t("runDB") {
        trace.injectDb(xa.trans)
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
        (new DbInterpreter.ForOps(db.databaseName)).trans(runDB)
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
        var a = ApplyEventAlgebra.trusted[Fx]
        a = ApplyEventAlgebra.withMetricsAndLogging(a, config.server.applyEventThresholdMs)
        a = ApplyEventAlgebra.traced(a, traceAlgebra)
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
        cryptoD      = cryptoD,
        cryptoF      = cryptoF,
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
    val redis          = new InMemoryRedis[Fx]
    val threadGroup    = new ThreadGroup("InMemoryRedis")
    val timer          = Executors.newSingleThreadScheduledExecutor(new Thread(threadGroup, _, "InMemoryRedis"))
    val task: Runnable = () => redis.publishAll.unsafeRun()
    val everyMs        = 1000
    timer.scheduleAtFixedRate(task, everyMs, everyMs, TimeUnit.MILLISECONDS)
    Runtime.getRuntime.addShutdownHook(new Thread(threadGroup, task, "InMemoryRedis-shutdown"))
    redis
  }
}
