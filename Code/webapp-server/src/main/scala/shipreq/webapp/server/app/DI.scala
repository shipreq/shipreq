package shipreq.webapp.server.app

import net.liftweb.common.Full
import net.liftweb.util.{SimpleInjector, Vendor}
import shipreq.base.db.DbAccess
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.lib.{StatLogger, StatLoggerImpl, TaskmanInterface}
import shipreq.webapp.server.security.{Oshiro, SecurityProvider}

// TODO Change DI to Globals
object DI extends SimpleInjector {

  def inject[A: Manifest](a: A): Inject[A] =
    new Inject(new Vendor[A] {
      override implicit def vend = a
      override implicit val make = Full(a)
    }) {}

  val SecurityProvider: Inject[SecurityProvider] =
    inject(Oshiro)

  val StatLogger: Inject[StatLogger] =
    inject(StatLoggerImpl)

  var dbAccess: DbAccess =
    null

  var taskman: TaskmanInterface =
    null

  var serverConfig: ServerConfig =
    null
}

/**
 * Mixes in accessors to DI resources.
 */
trait DI {
  @inline final def db()               = DI.dbAccess
  @inline final def securityProvider() = DI.SecurityProvider.vend
  @inline final def statLogger()       = DI.StatLogger.vend
  @inline final def taskman()          = DI.taskman
}
