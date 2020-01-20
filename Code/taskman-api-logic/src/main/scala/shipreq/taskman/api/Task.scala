package shipreq.taskman.api

import japgolly.univeq.UnivEq
import shipreq.base.util.Util

/**
 * A datum that can be sent to the Taskman server and meaningfully processed.
 */
sealed abstract class Task(final val taskType: TaskType) {
  final def taskTypeStr = taskType.toString
}

object Task {

  final case class RegistrationRequested(email         : EmailAddr,
                                         verifyEmailUrl: String) extends Task(TaskType.RegistrationRequested)

  final case class RegistrationCompleted(userId: UserId) extends Task(TaskType.RegistrationCompleted)

  final case class ReRegistrationAttempted(email: EmailAddr) extends Task(TaskType.ReRegistrationAttempted)

  final case class PasswordResetRequested(email           : EmailAddr,
                                          resetPasswordUrl: String) extends Task(TaskType.PasswordResetRequested)

  final case class UserUpdated(userId: UserId) extends Task(TaskType.UserUpdated)

  final case class UserFeedbackReceived(userId  : UserId,
                                        feedback: String,
                                        metadata: Map[String, String]) extends Task(TaskType.UserFeedbackReceived)

  final case class LandingPageHit(email     : EmailAddr,
                                  name      : String,
                                  msg       : Option[String],
                                  newsletter: Boolean) extends Task(TaskType.LandingPageHit)

  final case class DummyTask(desc            : String,
                             async           : Boolean        = false,
                             processingTimeMs: Long           = 0,
                             retryCount      : Short          = 0,
                             retryDelaySec   : Int            = 0,
                             failureMsg      : Option[String] = None) extends Task(TaskType.DummyTask)

  final case class SendDiagEmail(email  : EmailAddr,
                                 subject: String,
                                 body   : String) extends Task(TaskType.SendDiagEmail)

  final case class SyncToMailingList(sqlCond: Option[String]) extends Task(TaskType.SyncToMailingList)

  final case class ReportServerError(usr   : Option[UserId],
                                     url   : Option[String],
                                     report: String) extends Task(TaskType.ReportServerError) {
    override def toString = s"ReportServerError($usr, $url, ${Util.cutoffStr(report, 80)})"
  }

  implicit def univEq: UnivEq[Task] = UnivEq.derive

  def sample(taskType: TaskType): Task = {
    val ea = EmailAddr("whatever@gmail.com")
    val url = "http://hello.io"
    val uid = UserId(123)
    taskType match {
      case TaskType.DummyTask               => DummyTask("hello", failureMsg = Some("nope"))
      case TaskType.LandingPageHit          => LandingPageHit(ea, "Iskaral Pust", Some("No mule can match wits with me."), false)
      case TaskType.PasswordResetRequested  => PasswordResetRequested(ea, url)
      case TaskType.RegistrationCompleted   => RegistrationCompleted(uid)
      case TaskType.RegistrationRequested   => RegistrationRequested(ea, url)
      case TaskType.ReportServerError       => ReportServerError(Some(uid), Some("/login"), "blah")
      case TaskType.ReRegistrationAttempted => ReRegistrationAttempted(ea)
      case TaskType.SendDiagEmail           => SendDiagEmail(ea, "test", "hello")
      case TaskType.SyncToMailingList       => SyncToMailingList(Some("id < 100"))
      case TaskType.UserFeedbackReceived    => UserFeedbackReceived(uid, "Your product sucks!", Map("url" -> "https://shipreq.com/project/abcd", "userAgent" -> "Chrome!"))
      case TaskType.UserUpdated             => UserUpdated(uid)
    }
  }

}
