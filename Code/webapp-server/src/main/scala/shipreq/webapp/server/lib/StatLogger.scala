package shipreq.webapp.server.lib

import doobie.imports.ConnectionIO
import net.liftweb.actor.SpecializedLiftActor
import net.liftweb.common.Box
import net.liftweb.http.LiftSession
import shipreq.webapp.base.user._
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.feature.SessionStats

sealed trait StatLoggerCmd
case class LogUserLogin(id: UserId, ip: Option[String] = Misc.clientIp()) extends StatLoggerCmd

trait StatLogger {
  def !(msg: StatLoggerCmd): Unit
  def updateSessionStatsOnLogin(bs: Box[LiftSession], user: User): Unit
}

object StatLoggerImpl extends StatLogger with SpecializedLiftActor[StatLoggerCmd] {

  protected def dbRun[A](f: ConnectionIO[A]): A =
    Global.db.io.trans(f).unsafePerformIO()

  protected def messageHandler: PartialFunction[StatLoggerCmd, Unit] = {
    case LogUserLogin(id, ip) => dbRun(DbLogic.user.logLogin(id, ip))
  }

  override def updateSessionStatsOnLogin(bs: Box[LiftSession], user: User) =
    SessionStats.onLogin(bs, user)
}