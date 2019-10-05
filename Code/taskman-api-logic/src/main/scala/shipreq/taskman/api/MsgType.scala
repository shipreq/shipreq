package shipreq.taskman.api

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq.UnivEq

sealed trait MsgType
object MsgType {
  case object DummyMsg                extends MsgType
  case object LandingPageHit          extends MsgType
  case object PasswordResetRequested  extends MsgType
  case object RegistrationCompleted   extends MsgType
  case object RegistrationRequested   extends MsgType
  case object ReRegistrationAttempted extends MsgType
  case object SendDiagEmail           extends MsgType
  case object SyncToMailingList       extends MsgType
  case object UserUpdated             extends MsgType
  case object WebappErrorOccurred     extends MsgType

  implicit def univEq: UnivEq[MsgType] = UnivEq.derive

  val values = AdtMacros.adtValues[MsgType]
}
