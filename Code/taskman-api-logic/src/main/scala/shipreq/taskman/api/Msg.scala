package shipreq.taskman.api

import japgolly.univeq.UnivEq
import shipreq.base.util.Util

/**
 * A datum that can be sent to the Taskman server and meaningfully processed.
 */
sealed abstract class Msg(final val msgType: MsgType) {
  final def msgTypeStr = msgType.toString
}

object Msg {

  final case class RegistrationRequested(email         : EmailAddr,
                                         verifyEmailUrl: String) extends Msg(MsgType.RegistrationRequested)

  final case class RegistrationCompleted(userId: UserId) extends Msg(MsgType.RegistrationCompleted)

  final case class ReRegistrationAttempted(email: EmailAddr) extends Msg(MsgType.ReRegistrationAttempted)

  final case class PasswordResetRequested(email           : EmailAddr,
                                          resetPasswordUrl: String) extends Msg(MsgType.PasswordResetRequested)

  final case class UserUpdated(userId: UserId) extends Msg(MsgType.UserUpdated)

  final case class LandingPageHit(email     : EmailAddr,
                                  name      : String,
                                  msg       : Option[String],
                                  newsletter: Boolean) extends Msg(MsgType.LandingPageHit)

  final case class DummyMsg(desc            : String,
                            async           : Boolean        = false,
                            processingTimeMs: Long           = 0,
                            retryCount      : Short          = 0,
                            retryDelaySec   : Int            = 0,
                            failureMsg      : Option[String] = None) extends Msg(MsgType.DummyMsg)

  final case class SendDiagEmail(email  : EmailAddr,
                                 subject: String,
                                 body   : String) extends Msg(MsgType.SendDiagEmail)

  final case class SyncToMailingList(sqlCond: Option[String]) extends Msg(MsgType.SyncToMailingList)

  final case class WebappErrorOccurred(usr   : Option[UserId],
                                       url   : Option[String],
                                       report: String) extends Msg(MsgType.WebappErrorOccurred) {
    override def toString = s"WebappErrorOccurred($usr, $url, ${Util.cutoffStr(report, 80)})"
  }

  implicit def univEq: UnivEq[Msg] = UnivEq.derive

  def sample(msgType: MsgType): Msg = {
    val ea = EmailAddr("whatever@gmail.com")
    val url = "http://hello.io"
    val uid = UserId(123)
    msgType match {
      case MsgType.DummyMsg                => DummyMsg("hello", failureMsg = Some("nope"))
      case MsgType.ReRegistrationAttempted => ReRegistrationAttempted(ea)
      case MsgType.RegistrationRequested   => RegistrationRequested(ea, url)
      case MsgType.RegistrationCompleted   => RegistrationCompleted(uid)
      case MsgType.PasswordResetRequested  => PasswordResetRequested(ea, url)
      case MsgType.UserUpdated             => UserUpdated(uid)
      case MsgType.SendDiagEmail           => SendDiagEmail(ea, "test", "hello")
      case MsgType.LandingPageHit          => LandingPageHit(ea, "Iskaral Pust", Some("No mule can match wits with me."), false)
      case MsgType.SyncToMailingList       => SyncToMailingList(Some("id < 100"))
      case MsgType.WebappErrorOccurred     => WebappErrorOccurred(Some(uid), Some("/login"), "blah")
    }
  }

}
