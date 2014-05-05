package shipreq.webapp.lib

import net.liftweb.actor.SpecializedLiftActor
import net.liftweb.common.Box
import net.liftweb.http.LiftSession
import shipreq.webapp.app.DI
import shipreq.webapp.db.{UserDescriptor, DaoS}
import shipreq.webapp.feature.SessionStats
import shipreq.webapp.lib.Types._

sealed trait StatLoggerCmd
case class LogShareView(id: ShareId, ip: Option[String] = Misc.clientIp) extends StatLoggerCmd
case class LogUserLogin(id: UserId, ip: Option[String] = Misc.clientIp) extends StatLoggerCmd

trait StatLogger {
  def !(msg: StatLoggerCmd): Unit
  def updateSessionStatsOnLogin(bs: Box[LiftSession], user: UserDescriptor): Unit
}

object StatLoggerImpl extends StatLogger with SpecializedLiftActor[StatLoggerCmd] with DI {

  protected def dao(f: DaoS => Unit): Unit =
    daoProvider.withSession(f)

  protected def messageHandler: PartialFunction[StatLoggerCmd, Unit] = {
    case LogShareView(id, ip) => dao(_.logShareView(id, ip))
    case LogUserLogin(id, ip) => dao(_.logUserLogin(id, ip))
  }

  override def updateSessionStatsOnLogin(bs: Box[LiftSession], user: UserDescriptor) =
    SessionStats.onLogin(bs, user)
}