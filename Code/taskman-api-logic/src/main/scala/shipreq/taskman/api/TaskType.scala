package shipreq.taskman.api

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq.UnivEq

sealed trait TaskType
object TaskType {
  case object DummyTask               extends TaskType
  case object LandingPageHit          extends TaskType
  case object PasswordResetRequested  extends TaskType
  case object RegistrationCompleted   extends TaskType
  case object RegistrationRequested   extends TaskType
  case object ReportClientError       extends TaskType
  case object ReportServerError       extends TaskType
  case object ReRegistrationAttempted extends TaskType
  case object SendDiagEmail           extends TaskType
  case object SyncToMailingList       extends TaskType
  case object UserFeedbackReceived    extends TaskType
  case object UserUpdated             extends TaskType

  implicit def univEq: UnivEq[TaskType] = UnivEq.derive

  val values = AdtMacros.adtValues[TaskType]
}
