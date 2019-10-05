package shipreq.taskman.api

import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil._
import utest._

object MsgJsonTest extends TestSuite {

  // Remember: this format is used in the database.
  // Maintain backwards-compatibility.

  private val expectedJsonFor: MsgType => String = {
    case MsgType.DummyMsg                => """{"async":false,"desc":"hello","failureMsg":"nope","processingTimeMs":0,"retryCount":0,"retryDelaySec":0}"""
    case MsgType.ReRegistrationAttempted => """"whatever@gmail.com""""
    case MsgType.RegistrationRequested   => """{"email":"whatever@gmail.com","verifyEmailUrl":"http://hello.io"}"""
    case MsgType.RegistrationCompleted   => """123"""
    case MsgType.PasswordResetRequested  => """{"email":"whatever@gmail.com","resetPasswordUrl":"http://hello.io"}"""
    case MsgType.UserUpdated             => """123"""
    case MsgType.SendDiagEmail           => """{"body":"hello","email":"whatever@gmail.com","subject":"test"}"""
    case MsgType.LandingPageHit          => """{"email":"whatever@gmail.com","msg":"No mule can match wits with me.","name":"Iskaral Pust","newsletter":false}"""
    case MsgType.SyncToMailingList       => """"id < 100""""
    case MsgType.WebappErrorOccurred     => """{"report":"blah","url":"/login","usr":123}"""
  }

  private def test(t: MsgType): Unit = {
    val msg = Msg.sample(t)

    {
      implicit val dec = MsgJson.dataDecoder(t).map[Msg](m => m)
      val json         = expectedJsonFor(t)
      assertDecodeOk(json, msg)
      assertDecodeOk(MsgJson.encodeData(msg), msg)
    }

    import MsgJson.{decoderMsg, encoderMsg}
    assertRoundTrip(msg)
  }

  override def tests = Tests {
    "DummyMsg"                - test(MsgType.DummyMsg)
    "ReRegistrationAttempted" - test(MsgType.ReRegistrationAttempted)
    "RegistrationRequested"   - test(MsgType.RegistrationRequested)
    "RegistrationCompleted"   - test(MsgType.RegistrationCompleted)
    "PasswordResetRequested"  - test(MsgType.PasswordResetRequested)
    "UserUpdated"             - test(MsgType.UserUpdated)
    "SendDiagEmail"           - test(MsgType.SendDiagEmail)
    "LandingPageHit"          - test(MsgType.LandingPageHit)
    "SyncToMailingList"       - test(MsgType.SyncToMailingList)
    "WebappErrorOccurred"     - test(MsgType.WebappErrorOccurred)
  }
}
