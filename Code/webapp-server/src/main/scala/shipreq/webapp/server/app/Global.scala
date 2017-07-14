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
                        security: Security.Algebra[Fx],
                        taskman : TaskmanApi[Fx])

object Global {
  var Instance: Global = _

  @inline implicit def autoInstance(g: Global.type): Global = Instance

  def modify(f: Global => Global): Unit =
    Instance = f(Instance)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def default(implicit dbAccess: DbAccess, config: ServerConfig): Global = {
    assert(dbAccess ne null, "DbAccess is null, sir.")
    implicit val runDB         = dbAccess.fx.trans
             val taskmanCtx    = TaskmanApiImpl.Context(Some(config.taskmanSchema))
    implicit val taskman       = TaskmanApiImpl(taskmanCtx, runDB)
    implicit val dbAlgebra     = new DbInterpreter()
    implicit val dbForSecurity = DB.ForSecurity.trans(DbInterpreter.ForSecurity)(runDB)
    implicit val projectStore  = Store.Algebra.concurrentHashMap(): ProjectServer.StoreAlgebra[Fx]
    implicit val security      = new SecurityInterpreter[Fx]
    implicit val server        = ServerInterpreter
    Global(
      config   = config,
      db       = dbAccess,
      logic    = ServerLogic.create[ConnectionIO, Fx](ProjectServer.BroadcastTo.All),
      security = security,
      taskman  = taskman)
    }
}