package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import scalaz.effect.IO
import shipreq.base.db.DbAccess
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security.SecurityInterpreter

final case class Global(config  : ServerConfig,
                        db      : DbAccess,
                        logic   : ServerLogic[IO],
                        security: Security.Algebra[IO],
                        taskman : TaskmanApi[IO])

object Global {
  var Instance: Global = _

  @inline implicit def autoInstance(g: Global.type): Global = Instance

  def modify(f: Global => Global): Unit =
    Instance = f(Instance)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def default(implicit dbAccess: DbAccess, config: ServerConfig): Global = {
    assert(dbAccess ne null, "DbAccess is null, sir.")
             val taskmanCtx    = TaskmanApiImpl.Context(Some(config.taskmanSchema))
    implicit val taskman       = TaskmanApiImpl(taskmanCtx, dbAccess.io.trans)
    implicit val dbAlgebra     = new DbInterpreter()
    implicit val dbForSecurity = DB.ForSecurity.trans(DbInterpreter.ForSecurity)(dbAccess.io.trans)
    implicit val runDB         = dbAccess.trans
    implicit val projectStore  = Store.Algebra.concurrentHashMap(): ProjectServer.StoreAlgebra[IO]
    implicit val security      = new SecurityInterpreter[IO]
    implicit val server        = ServerInterpreter
    Global(
      config   = config,
      db       = dbAccess,
      logic    = ServerLogic.create[ConnectionIO, IO](ProjectServer.BroadcastTo.All),
      security = security,
      taskman  = taskman)
    }
}