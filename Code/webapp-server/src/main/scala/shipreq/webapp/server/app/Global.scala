package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import shipreq.base.db.DbAccess
import shipreq.base.util.FxModule._
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security.SecurityInterpreter

final case class Global(config  : ServerConfig,
                        db      : DbAccess,
                        logic   : ServerLogic[Fx],
                        metrics : MetricsLogic[Fx],
                        ops     : OpsEndpointInterpreter,
                        security: Security.Algebra[Fx],
                        taskman : TaskmanApi[Fx],
                        trace   : TraceInterpreter.ForLift[Fx])

object Global {
  var Instance: Global = _

  @inline implicit def autoInstance(g: Global.type): Global = Instance

  def modify(f: Global => Global): Unit =
    Instance = f(Instance)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def default(implicit dbAccess: DbAccess, config: ServerConfig): Global = {
    assert(dbAccess ne null, "DbAccess is null, sir.")
    import TraceInterpreter.Implicits._

    implicit def configSecurity = config.security

    implicit val metrics: MetricsLogic[Fx] =
      if (config.prometheus.enabled)
        new PrometheusMetrics
      else
        MetricsLogic.const(Fx.unit)

    implicit val traceAlgebra  = config.traceAlgebraFx
    implicit val trace         = new TraceLogic: TraceInterpreter.ForLift[Fx]
    implicit val runDB         = trace.injectDb(dbAccess.fx.trans)
    implicit val taskman       = TaskmanApi.addLogging(TaskmanApiImpl(Some(config.taskmanSchema)).trans(runDB))
    implicit val dbAlgebra     = new DbInterpreter()
    implicit val dbForSecurity = DB.ForSecurity.trans(DbInterpreter.ForSecurity)(runDB)
    implicit val dbForOps      = DB.ForOps.trans(new DbInterpreter.ForOps(dbAccess.databaseName))(runDB)
    implicit val projectStore  = metrics.injectProjectStore(Store.Algebra.concurrentHashMap())
    implicit val server        = metrics.injectServer(trace.injectServer(ServerInterpreter))
    implicit val ops           = new OpsEndpointInterpreter()
    implicit val security      = new SecurityInterpreter[Fx]

    Global(
      config   = config,
      db       = dbAccess,
      logic    = ServerLogic.create[ConnectionIO, Fx](ProjectServer.BroadcastTo.All),
      metrics  = metrics,
      ops      = ops,
      security = security,
      taskman  = taskman,
      trace    = trace)
    }
}