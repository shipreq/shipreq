package shipreq.webapp.server.lib

import doobie.imports.ConnectionIO
import net.liftweb.actor.SpecializedLiftActor
import net.liftweb.common.Box
import net.liftweb.http.LiftSession
import shipreq.taskman.api.UserId
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.feature.SessionStats

sealed trait StatLoggerCmd
case class LogUserLogin(id: UserId, ip: Option[String] = Misc.clientIp()) extends StatLoggerCmd

trait StatLogger {
  def !(msg: StatLoggerCmd): Unit
  def updateSessionStatsOnLogin(bs: Box[LiftSession], user: UserDescriptor): Unit
}

object StatLoggerImpl extends StatLogger with SpecializedLiftActor[StatLoggerCmd] with DI {

  protected def dbRun[A](f: ConnectionIO[A]): A =
    db().io.trans(f).unsafePerformIO()

  protected def messageHandler: PartialFunction[StatLoggerCmd, Unit] = {
    case LogUserLogin(id, ip) => dbRun(DbLogic.user.logLogin(id, ip))
  }

  override def updateSessionStatsOnLogin(bs: Box[LiftSession], user: UserDescriptor) =
    SessionStats.onLogin(bs, user)
}