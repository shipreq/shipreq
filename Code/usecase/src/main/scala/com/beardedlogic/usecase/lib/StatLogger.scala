package com.beardedlogic.usecase.lib

import com.beardedlogic.usecase.lib.Types.ShareId
import net.liftweb.actor.SpecializedLiftActor
import com.beardedlogic.usecase.db.DaoS
import com.beardedlogic.usecase.app.DI

sealed trait StatLoggerCmd
case class LogShareView(shareId: ShareId, ip: Option[String] = Misc.clientIp) extends StatLoggerCmd

trait StatLogger {
  def !(msg: StatLoggerCmd): Unit
}

object StatLoggerActor extends StatLogger with SpecializedLiftActor[StatLoggerCmd] {

  protected def dao(f: DaoS => Unit): Unit =
    DI.DaoProvider.withSession(f)

  protected def messageHandler: PartialFunction[StatLoggerCmd, Unit] = {
    case LogShareView(shareId, ip) => dao(_.logShareView(shareId, ip))
  }
}