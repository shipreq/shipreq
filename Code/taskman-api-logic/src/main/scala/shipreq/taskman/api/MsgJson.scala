package shipreq.taskman.api

import io.circe._
import io.circe.syntax._
import shipreq.base.util.JsonUtil._

object MsgJson {

  implicit val decoderUserId: Decoder[UserId] =
    Decoder[Long].map(UserId.apply)

  implicit val encoderUserId: Encoder[UserId] =
    Encoder[Long].contramap(_.value)

  implicit val decoderEmailAddr: Decoder[EmailAddr] =
    Decoder[String].map(EmailAddr.apply)

  implicit val encoderEmailAddr: Encoder[EmailAddr] =
    Encoder[String].contramap(_.value)

  // ===================================================================================================================

  implicit val decoderMsgRegistrationRequested: Decoder[Msg.RegistrationRequested] =
    Decoder.forProduct2("email", "verifyEmailUrl")(Msg.RegistrationRequested.apply)

  implicit val encoderMsgRegistrationRequested: Encoder[Msg.RegistrationRequested] =
    Encoder.forProduct2("email", "verifyEmailUrl")(a => (a.email, a.verifyEmailUrl))

  implicit val decoderMsgRegistrationCompleted: Decoder[Msg.RegistrationCompleted] =
    Decoder[UserId].map(Msg.RegistrationCompleted.apply)

  implicit val encoderMsgRegistrationCompleted: Encoder[Msg.RegistrationCompleted] =
    Encoder[UserId].contramap(_.userId)

  implicit val decoderMsgReRegistrationAttempted: Decoder[Msg.ReRegistrationAttempted] =
    Decoder[EmailAddr].map(Msg.ReRegistrationAttempted.apply)

  implicit val encoderMsgReRegistrationAttempted: Encoder[Msg.ReRegistrationAttempted] =
    Encoder[EmailAddr].contramap(_.email)

  implicit val decoderMsgPasswordResetRequested: Decoder[Msg.PasswordResetRequested] =
    Decoder.forProduct2("email", "resetPasswordUrl")(Msg.PasswordResetRequested.apply)

  implicit val encoderMsgPasswordResetRequested: Encoder[Msg.PasswordResetRequested] =
    Encoder.forProduct2("email", "resetPasswordUrl")(a => (a.email, a.resetPasswordUrl))

  implicit val decoderMsgUserUpdated: Decoder[Msg.UserUpdated] =
    Decoder[UserId].map(Msg.UserUpdated.apply)

  implicit val encoderMsgUserUpdated: Encoder[Msg.UserUpdated] =
    Encoder[UserId].contramap(_.userId)

  implicit val decoderMsgLandingPageHit: Decoder[Msg.LandingPageHit] =
    Decoder.forProduct4("email", "name", "msg", "newsletter")(Msg.LandingPageHit.apply)

  implicit val encoderMsgLandingPageHit: Encoder[Msg.LandingPageHit] =
    Encoder.forProduct4("email", "name", "msg", "newsletter")(a => (a.email, a.name, a.msg, a.newsletter))

  implicit val decoderMsgDummyMsg: Decoder[Msg.DummyMsg] =
    Decoder.forProduct6("desc", "async", "processingTimeMs", "retryCount", "retryDelaySec", "failureMsg")(Msg.DummyMsg.apply)

  implicit val encoderMsgDummyMsg: Encoder[Msg.DummyMsg] =
    Encoder.forProduct6("desc", "async", "processingTimeMs", "retryCount", "retryDelaySec", "failureMsg")(a => (a.desc, a.async, a.processingTimeMs, a.retryCount, a.retryDelaySec, a.failureMsg))

  implicit val decoderMsgSendDiagEmail: Decoder[Msg.SendDiagEmail] =
    Decoder.forProduct3("email", "subject", "body")(Msg.SendDiagEmail.apply)

  implicit val encoderMsgSendDiagEmail: Encoder[Msg.SendDiagEmail] =
    Encoder.forProduct3("email", "subject", "body")(a => (a.email, a.subject, a.body))

  implicit val decoderMsgSyncToMailingList: Decoder[Msg.SyncToMailingList] =
    Decoder[Option[String]].map(Msg.SyncToMailingList.apply)

  implicit val encoderMsgSyncToMailingList: Encoder[Msg.SyncToMailingList] =
    Encoder[Option[String]].contramap(_.sqlCond)

  implicit val decoderMsgWebappErrorOccurred: Decoder[Msg.WebappErrorOccurred] =
    Decoder.forProduct3("usr", "url", "report")(Msg.WebappErrorOccurred.apply)

  implicit val encoderMsgWebappErrorOccurred: Encoder[Msg.WebappErrorOccurred] =
    Encoder.forProduct3("usr", "url", "report")(a => (a.usr, a.url, a.report))

  // ===================================================================================================================

  val dataDecoder: MsgType => Decoder[_ <: Msg] = {
    case MsgType.DummyMsg                => decoderMsgDummyMsg
    case MsgType.LandingPageHit          => decoderMsgLandingPageHit
    case MsgType.PasswordResetRequested  => decoderMsgPasswordResetRequested
    case MsgType.RegistrationCompleted   => decoderMsgRegistrationCompleted
    case MsgType.RegistrationRequested   => decoderMsgRegistrationRequested
    case MsgType.ReRegistrationAttempted => decoderMsgReRegistrationAttempted
    case MsgType.SendDiagEmail           => decoderMsgSendDiagEmail
    case MsgType.SyncToMailingList       => decoderMsgSyncToMailingList
    case MsgType.UserUpdated             => decoderMsgUserUpdated
    case MsgType.WebappErrorOccurred     => decoderMsgWebappErrorOccurred
  }

  val encodeData: Msg => Json = {
    case m: Msg.DummyMsg                => m.asJson
    case m: Msg.LandingPageHit          => m.asJson
    case m: Msg.PasswordResetRequested  => m.asJson
    case m: Msg.RegistrationCompleted   => m.asJson
    case m: Msg.RegistrationRequested   => m.asJson
    case m: Msg.ReRegistrationAttempted => m.asJson
    case m: Msg.SendDiagEmail           => m.asJson
    case m: Msg.SyncToMailingList       => m.asJson
    case m: Msg.UserUpdated             => m.asJson
    case m: Msg.WebappErrorOccurred     => m.asJson
  }

  // ===================================================================================================================

  implicit val decoderMsg: Decoder[Msg] = decodeSumBySoleKey {
    case ("DummyMsg"               , c) => c.as[Msg.DummyMsg]
    case ("LandingPageHit"         , c) => c.as[Msg.LandingPageHit]
    case ("PasswordResetRequested" , c) => c.as[Msg.PasswordResetRequested]
    case ("ReRegistrationAttempted", c) => c.as[Msg.ReRegistrationAttempted]
    case ("RegistrationCompleted"  , c) => c.as[Msg.RegistrationCompleted]
    case ("RegistrationRequested"  , c) => c.as[Msg.RegistrationRequested]
    case ("SendDiagEmail"          , c) => c.as[Msg.SendDiagEmail]
    case ("SyncToMailingList"      , c) => c.as[Msg.SyncToMailingList]
    case ("UserUpdated"            , c) => c.as[Msg.UserUpdated]
    case ("WebappErrorOccurred"    , c) => c.as[Msg.WebappErrorOccurred]
  }

  implicit val encoderMsg: Encoder[Msg] = Encoder.instance {
    case a: Msg.DummyMsg                => Json.obj("DummyMsg"                -> a.asJson)
    case a: Msg.LandingPageHit          => Json.obj("LandingPageHit"          -> a.asJson)
    case a: Msg.PasswordResetRequested  => Json.obj("PasswordResetRequested"  -> a.asJson)
    case a: Msg.ReRegistrationAttempted => Json.obj("ReRegistrationAttempted" -> a.asJson)
    case a: Msg.RegistrationCompleted   => Json.obj("RegistrationCompleted"   -> a.asJson)
    case a: Msg.RegistrationRequested   => Json.obj("RegistrationRequested"   -> a.asJson)
    case a: Msg.SendDiagEmail           => Json.obj("SendDiagEmail"           -> a.asJson)
    case a: Msg.SyncToMailingList       => Json.obj("SyncToMailingList"       -> a.asJson)
    case a: Msg.UserUpdated             => Json.obj("UserUpdated"             -> a.asJson)
    case a: Msg.WebappErrorOccurred     => Json.obj("WebappErrorOccurred"     -> a.asJson)
  }

}
