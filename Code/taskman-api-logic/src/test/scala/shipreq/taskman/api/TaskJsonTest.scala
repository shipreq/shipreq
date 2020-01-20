package shipreq.taskman.api

import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil._
import utest._

object TaskJsonTest extends TestSuite {

  // Remember: this format is used in the database.
  // Maintain backwards-compatibility.

  private val expectedJsonFor: TaskType => String = {
    case TaskType.DummyTask               => """{"async":false,"desc":"hello","failureMsg":"nope","processingTimeMs":0,"retryCount":0,"retryDelaySec":0}"""
    case TaskType.LandingPageHit          => """{"email":"whatever@gmail.com","msg":"No mule can match wits with me.","name":"Iskaral Pust","newsletter":false}"""
    case TaskType.PasswordResetRequested  => """{"email":"whatever@gmail.com","resetPasswordUrl":"http://hello.io"}"""
    case TaskType.RegistrationCompleted   => """123"""
    case TaskType.RegistrationRequested   => """{"email":"whatever@gmail.com","verifyEmailUrl":"http://hello.io"}"""
    case TaskType.ReportServerError       => """{"report":"blah","url":"/login","usr":123}"""
    case TaskType.ReRegistrationAttempted => """"whatever@gmail.com""""
    case TaskType.SendDiagEmail           => """{"body":"hello","email":"whatever@gmail.com","subject":"test"}"""
    case TaskType.SyncToMailingList       => """"id < 100""""
    case TaskType.UserFeedbackReceived    => """{"userId":123,"feedback":"Your product sucks!","metadata":{"url":"https://shipreq.com/project/abcd","userAgent":"Chrome!"}}"""
    case TaskType.UserUpdated             => """123"""
  }

  private def test(t: TaskType): Unit = {
    val task = Task.sample(t)

    {
      implicit val dec = TaskJson.dataDecoder(t).map[Task](m => m)
      val json = expectedJsonFor(t)
      // println(TaskJson.encodeData(task).noSpaces)
      assertDecodeOk(json, task)
      assertDecodeOk(TaskJson.encodeData(task), task)
    }

    import TaskJson.{decoderTask, encoderTask}
    assertRoundTrip(task)
  }

  override def tests = Tests {
    "DummyTask"               - test(TaskType.DummyTask)
    "LandingPageHit"          - test(TaskType.LandingPageHit)
    "PasswordResetRequested"  - test(TaskType.PasswordResetRequested)
    "RegistrationCompleted"   - test(TaskType.RegistrationCompleted)
    "RegistrationRequested"   - test(TaskType.RegistrationRequested)
    "ReportServerError"       - test(TaskType.ReportServerError)
    "ReRegistrationAttempted" - test(TaskType.ReRegistrationAttempted)
    "SendDiagEmail"           - test(TaskType.SendDiagEmail)
    "SyncToMailingList"       - test(TaskType.SyncToMailingList)
    "UserFeedbackReceived"    - test(TaskType.UserFeedbackReceived)
    "UserUpdated"             - test(TaskType.UserUpdated)
  }
}
