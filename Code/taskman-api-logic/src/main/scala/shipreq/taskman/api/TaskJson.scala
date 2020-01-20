package shipreq.taskman.api

import io.circe._
import io.circe.syntax._
import shipreq.base.util.JsonUtil._

object TaskJson {

  implicit val decoderUserId: Decoder[UserId] =
    Decoder[Long].map(UserId.apply)

  implicit val encoderUserId: Encoder[UserId] =
    Encoder[Long].contramap(_.value)

  implicit val decoderEmailAddr: Decoder[EmailAddr] =
    Decoder[String].map(EmailAddr.apply)

  implicit val encoderEmailAddr: Encoder[EmailAddr] =
    Encoder[String].contramap(_.value)

  // ===================================================================================================================

  implicit val decoderTaskRegistrationRequested: Decoder[Task.RegistrationRequested] =
    Decoder.forProduct2("email", "verifyEmailUrl")(Task.RegistrationRequested.apply)

  implicit val encoderTaskRegistrationRequested: Encoder[Task.RegistrationRequested] =
    Encoder.forProduct2("email", "verifyEmailUrl")(a => (a.email, a.verifyEmailUrl))

  implicit val decoderTaskRegistrationCompleted: Decoder[Task.RegistrationCompleted] =
    Decoder[UserId].map(Task.RegistrationCompleted.apply)

  implicit val encoderTaskRegistrationCompleted: Encoder[Task.RegistrationCompleted] =
    Encoder[UserId].contramap(_.userId)

  implicit val decoderTaskReRegistrationAttempted: Decoder[Task.ReRegistrationAttempted] =
    Decoder[EmailAddr].map(Task.ReRegistrationAttempted.apply)

  implicit val encoderTaskReRegistrationAttempted: Encoder[Task.ReRegistrationAttempted] =
    Encoder[EmailAddr].contramap(_.email)

  implicit val decoderTaskPasswordResetRequested: Decoder[Task.PasswordResetRequested] =
    Decoder.forProduct2("email", "resetPasswordUrl")(Task.PasswordResetRequested.apply)

  implicit val encoderTaskPasswordResetRequested: Encoder[Task.PasswordResetRequested] =
    Encoder.forProduct2("email", "resetPasswordUrl")(a => (a.email, a.resetPasswordUrl))

  implicit val decoderTaskUserUpdated: Decoder[Task.UserUpdated] =
    Decoder[UserId].map(Task.UserUpdated.apply)

  implicit val encoderTaskUserUpdated: Encoder[Task.UserUpdated] =
    Encoder[UserId].contramap(_.userId)

  implicit val decoderTaskLandingPageHit: Decoder[Task.LandingPageHit] =
    Decoder.forProduct4("email", "name", "msg", "newsletter")(Task.LandingPageHit.apply)

  implicit val encoderTaskLandingPageHit: Encoder[Task.LandingPageHit] =
    Encoder.forProduct4("email", "name", "msg", "newsletter")(a => (a.email, a.name, a.msg, a.newsletter))

  implicit val decoderTaskDummyTask: Decoder[Task.DummyTask] =
    Decoder.forProduct6("desc", "async", "processingTimeMs", "retryCount", "retryDelaySec", "failureMsg")(Task.DummyTask.apply)

  implicit val encoderTaskDummyTask: Encoder[Task.DummyTask] =
    Encoder.forProduct6("desc", "async", "processingTimeMs", "retryCount", "retryDelaySec", "failureMsg")(a => (a.desc, a.async, a.processingTimeMs, a.retryCount, a.retryDelaySec, a.failureMsg))

  implicit val decoderTaskSendDiagEmail: Decoder[Task.SendDiagEmail] =
    Decoder.forProduct3("email", "subject", "body")(Task.SendDiagEmail.apply)

  implicit val encoderTaskSendDiagEmail: Encoder[Task.SendDiagEmail] =
    Encoder.forProduct3("email", "subject", "body")(a => (a.email, a.subject, a.body))

  implicit val decoderTaskSyncToMailingList: Decoder[Task.SyncToMailingList] =
    Decoder[Option[String]].map(Task.SyncToMailingList.apply)

  implicit val encoderTaskSyncToMailingList: Encoder[Task.SyncToMailingList] =
    Encoder[Option[String]].contramap(_.sqlCond)

  implicit val decoderTaskReportServerError: Decoder[Task.ReportServerError] =
    Decoder.forProduct3("userId", "url", "report")(Task.ReportServerError.apply)

  implicit val encoderTaskReportServerError: Encoder[Task.ReportServerError] =
    Encoder.forProduct3("userId", "url", "report")(a => (a.userId, a.url, a.report))

  implicit val decoderTaskUserFeedbackReceived: Decoder[Task.UserFeedbackReceived] =
    Decoder.forProduct3("userId", "feedback", "metadata")(Task.UserFeedbackReceived.apply)

  implicit val encoderTaskUserFeedbackReceived: Encoder[Task.UserFeedbackReceived] =
    Encoder.forProduct3("userId", "feedback", "metadata")(a => (a.userId, a.feedback, a.metadata))

  implicit val decoderTaskReportClientError: Decoder[Task.ReportClientError] =
    Decoder.forProduct4("userId", "nameKey", "messageKey", "data")(Task.ReportClientError.apply)

  implicit val encoderTaskReportClientError: Encoder[Task.ReportClientError] =
    Encoder.forProduct4("userId", "nameKey", "messageKey", "data")(a => (a.userId, a.nameKey, a.messageKey, a.data))

  // ===================================================================================================================

  val dataDecoder: TaskType => Decoder[_ <: Task] = {
    case TaskType.DummyTask               => decoderTaskDummyTask
    case TaskType.LandingPageHit          => decoderTaskLandingPageHit
    case TaskType.PasswordResetRequested  => decoderTaskPasswordResetRequested
    case TaskType.RegistrationCompleted   => decoderTaskRegistrationCompleted
    case TaskType.RegistrationRequested   => decoderTaskRegistrationRequested
    case TaskType.ReportClientError       => decoderTaskReportClientError
    case TaskType.ReportServerError       => decoderTaskReportServerError
    case TaskType.ReRegistrationAttempted => decoderTaskReRegistrationAttempted
    case TaskType.SendDiagEmail           => decoderTaskSendDiagEmail
    case TaskType.SyncToMailingList       => decoderTaskSyncToMailingList
    case TaskType.UserFeedbackReceived    => decoderTaskUserFeedbackReceived
    case TaskType.UserUpdated             => decoderTaskUserUpdated
  }

  val encodeData: Task => Json = {
    case m: Task.DummyTask               => m.asJson
    case m: Task.LandingPageHit          => m.asJson
    case m: Task.PasswordResetRequested  => m.asJson
    case m: Task.RegistrationCompleted   => m.asJson
    case m: Task.RegistrationRequested   => m.asJson
    case m: Task.ReportClientError       => m.asJson
    case m: Task.ReportServerError       => m.asJson
    case m: Task.ReRegistrationAttempted => m.asJson
    case m: Task.SendDiagEmail           => m.asJson
    case m: Task.SyncToMailingList       => m.asJson
    case m: Task.UserUpdated             => m.asJson
    case m: Task.UserFeedbackReceived    => m.asJson
  }

  // ===================================================================================================================

  implicit val decoderTask: Decoder[Task] = decodeSumBySoleKey {
    case ("DummyTask"              , c) => c.as[Task.DummyTask]
    case ("LandingPageHit"         , c) => c.as[Task.LandingPageHit]
    case ("PasswordResetRequested" , c) => c.as[Task.PasswordResetRequested]
    case ("RegistrationCompleted"  , c) => c.as[Task.RegistrationCompleted]
    case ("RegistrationRequested"  , c) => c.as[Task.RegistrationRequested]
    case ("ReportClientError"      , c) => c.as[Task.ReportClientError]
    case ("ReportServerError"      , c) => c.as[Task.ReportServerError]
    case ("ReRegistrationAttempted", c) => c.as[Task.ReRegistrationAttempted]
    case ("SendDiagEmail"          , c) => c.as[Task.SendDiagEmail]
    case ("SyncToMailingList"      , c) => c.as[Task.SyncToMailingList]
    case ("UserUpdated"            , c) => c.as[Task.UserUpdated]
    case ("UserFeedbackReceived"   , c) => c.as[Task.UserFeedbackReceived]
  }

  implicit val encoderTask: Encoder[Task] = Encoder.instance {
    case a: Task.DummyTask               => Json.obj("DummyTask"               -> a.asJson)
    case a: Task.LandingPageHit          => Json.obj("LandingPageHit"          -> a.asJson)
    case a: Task.PasswordResetRequested  => Json.obj("PasswordResetRequested"  -> a.asJson)
    case a: Task.RegistrationCompleted   => Json.obj("RegistrationCompleted"   -> a.asJson)
    case a: Task.RegistrationRequested   => Json.obj("RegistrationRequested"   -> a.asJson)
    case a: Task.ReportClientError       => Json.obj("ReportClientError"       -> a.asJson)
    case a: Task.ReportServerError       => Json.obj("ReportServerError"       -> a.asJson)
    case a: Task.ReRegistrationAttempted => Json.obj("ReRegistrationAttempted" -> a.asJson)
    case a: Task.SendDiagEmail           => Json.obj("SendDiagEmail"           -> a.asJson)
    case a: Task.SyncToMailingList       => Json.obj("SyncToMailingList"       -> a.asJson)
    case a: Task.UserUpdated             => Json.obj("UserUpdated"             -> a.asJson)
    case a: Task.UserFeedbackReceived    => Json.obj("UserFeedbackReceived"    -> a.asJson)
  }

}
