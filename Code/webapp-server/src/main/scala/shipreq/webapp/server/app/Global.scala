package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import scalaz.effect.IO
import shipreq.base.db.DbAccess
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.lib.{StatLogger, StatLoggerImpl}
import shipreq.webapp.server.logic.{ProjectServer, ServerLogic}
import shipreq.webapp.server.security.{Oshiro, SecurityProvider}

final case class Global(config    : ServerConfig,
                        db        : DbAccess,
                        logic     : ServerLogic[IO],
                        security  : SecurityProvider,
                        statLogger: StatLogger,
                        taskman   : TaskmanApi[IO])

object Global {
  var Instance: Global = _

  @inline implicit def autoInstance(g: Global.type): Global = Instance

  def modify(f: Global => Global): Unit =
    Instance = f(Instance)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def defaultSecurity  : SecurityProvider = Oshiro
  def defaultStatLogger: StatLogger       = StatLoggerImpl

  def default(implicit db: DbAccess, config: ServerConfig): Global = {
    assert(db ne null, "DbAccess is null, sir.")
    val taskmanCtx = TaskmanApiImpl.Context(Some(config.taskmanSchema))
    import Interpreters._
    Global(
      config     = config,
      db         = db,
      logic      = ServerLogic.create[ConnectionIO, IO](ProjectServer.BroadcastTo.All),
      security   = defaultSecurity,
      statLogger = defaultStatLogger,
      taskman    = TaskmanApiImpl(taskmanCtx, db.io.trans))
    }
}