package shipreq.webapp.server.app

import doobie.imports.ConnectionIO
import scalaz.effect.IO
import shipreq.base.db.DbAccess
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.lib.{StatLogger, StatLoggerImpl, TaskmanImpl, TaskmanInterface}
import shipreq.webapp.server.logic.{ProjectServer, ServerLogic}
import shipreq.webapp.server.security.{Oshiro, SecurityProvider}

final case class Global(config    : ServerConfig,
                        db        : DbAccess,
                        logic     : ServerLogic[IO],
                        security  : SecurityProvider,
                        statLogger: StatLogger,
                        taskman   : TaskmanInterface)

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
    import Interpreters._
    Global(
      config     = config,
      db         = db,
      logic      = ServerLogic.create[ConnectionIO, IO](ProjectServer.BroadcastTo.All),
      security   = defaultSecurity,
      statLogger = defaultStatLogger,
      taskman    = new TaskmanImpl(db.io, config))
    }
}