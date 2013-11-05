package com.beardedlogic.usecase.lib

import com.beardedlogic.usecase.lib.Types._
import net.liftweb.actor.SpecializedLiftActor
import com.beardedlogic.usecase.db.DaoS
import com.beardedlogic.usecase.app.DI

sealed trait StatLoggerCmd
case class LogShareView(id: ShareId, ip: Option[String] = Misc.clientIp) extends StatLoggerCmd
case class LogUserLogin(id: UserId, ip: Option[String] = Misc.clientIp) extends StatLoggerCmd

trait StatLogger {
  def !(msg: StatLoggerCmd): Unit
}

object StatLoggerActor extends StatLogger with SpecializedLiftActor[StatLoggerCmd] {

  protected def dao(f: DaoS => Unit): Unit =
    DI.DaoProvider.withSession(f)

  protected def messageHandler: PartialFunction[StatLoggerCmd, Unit] = {
    case LogShareView(id, ip) => dao(_.logShareView(id, ip))
    case LogUserLogin(id, ip) => dao(_.logUserLogin(id, ip))
  }
}