package shipreq.taskman.api

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.StaticLookupFn
import japgolly.univeq.UnivEq
import shipreq.base.util.Util

/**
 * A datum that can be sent to the Taskman server and meaningfully processed.
 */
sealed trait Msg {
  final def msgTypeStr = getClass.getSimpleName
}

object Msg {

  // ********************************************************************
  // * NOTE: Fields names here need to match the JSON FieldSerializers. *
  // ********************************************************************

  case class RegistrationRequested(email: EmailAddr, verifyEmailUrl: String) extends Msg

  case class RegistrationCompleted(userId: UserId) extends Msg

  case class ReRegistrationAttempted(email: EmailAddr) extends Msg

  case class PasswordResetRequested(email: EmailAddr, resetPasswordUrl: String) extends Msg

  case class UserUpdated(userId: UserId) extends Msg

  case class LandingPageHit(email: EmailAddr, name: String, msg: Option[String], newsletter: Boolean) extends Msg

  case class DummyMsg(desc: String,
                      async: Boolean = false,
                      processingTimeMs: Long = 0,
                      retryCount: Short = 0,
                      retryDelaySec: Int = 0,
                      failureMsg: Option[String] = None) extends Msg

  case class SendDiagEmail(email: EmailAddr, subject: String, body: String) extends Msg

  case class SyncToMailingList(sqlCond: Option[String]) extends Msg

  case class WebappErrorOccurred(usr: Option[UserId], url: Option[String], report: String) extends Msg {
    override def toString = s"WebappErrorOccurred($usr, $url, ${Util.cutoffStr(report, 80)})"
  }

  implicit def univEq: UnivEq[Msg] = UnivEq.derive
}

// =====================================================================================================================

sealed abstract class MsgType(val id: Short, val msgClass: Class[_ <: Msg])

object MsgType {
  case object DummyMsg                extends MsgType(  1, classOf[Msg.DummyMsg])
  case object SendDiagEmail           extends MsgType(  2, classOf[Msg.SendDiagEmail])
  case object RegistrationRequested   extends MsgType(100, classOf[Msg.RegistrationRequested])
  case object RegistrationCompleted   extends MsgType(101, classOf[Msg.RegistrationCompleted])
  case object ReRegistrationAttempted extends MsgType(102, classOf[Msg.ReRegistrationAttempted])
  case object PasswordResetRequested  extends MsgType(103, classOf[Msg.PasswordResetRequested])
  case object UserUpdated             extends MsgType(104, classOf[Msg.UserUpdated])
  case object LandingPageHit          extends MsgType(200, classOf[Msg.LandingPageHit])
  case object SyncToMailingList       extends MsgType(300, classOf[Msg.SyncToMailingList])
  case object WebappErrorOccurred     extends MsgType(500, classOf[Msg.WebappErrorOccurred])

  val values         = AdtMacros.adtValues[MsgType].whole.toList
  val byId           = StaticLookupFn.useMapBy(values)(_.id).toOption
  val byMsgClass     = StaticLookupFn.useMapBy(values)(_.msgClass: Class[_ <: Msg])(UnivEq.force).total // TODO Fix UnivEq.force
  val byMsgClassName = StaticLookupFn.useMapBy(values)(_.msgClass.getSimpleName).toOption

  @inline def lookup(id: Short)         : Option[MsgType] = byId(id)
  @inline def lookup(name: String)      : Option[MsgType] = byMsgClassName(name)
  @inline def lookup(c: Class[_ <: Msg]): MsgType         = byMsgClass(c)
  @inline def lookup(d: Msg)            : MsgType         = byMsgClass(d.getClass)
}
