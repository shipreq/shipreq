package shipreq.taskman.api

import org.scalacheck.{Gen, Arbitrary}
import shipreq.taskman.api.{TaskType => T}
import shipreq.taskman.api.{Task => M}

object TestHelpers {

  import Arbitrary._

  def genEmail: Gen[EmailAddr] = arbitrary[String].map(EmailAddr.apply)
  def genUserId: Gen[UserId] = arbitrary[Long].map(UserId.apply)
  def genUserIdO: Gen[Option[UserId]] = Gen.option(genUserId)

  implicit def arbTask: Arbitrary[Task] =
    Arbitrary(Gen.oneOf(TaskType.values.whole).flatMap(genTaskForType))

  def genTaskForType(m: TaskType): Gen[Task] = m match {

    case T.RegistrationRequested =>
      for {
        email <- genEmail
        url   <- arbitrary[String]
      } yield M.RegistrationRequested(email, url)

    case T.ReRegistrationAttempted =>
      for {
        email <- genEmail
      } yield M.ReRegistrationAttempted(email)

    case T.RegistrationCompleted =>
      for {
        userId <- genUserId
      } yield M.RegistrationCompleted(userId)

    case T.UserUpdated =>
      for {
        userId <- genUserId
      } yield M.UserUpdated(userId)

    case T.PasswordResetRequested =>
      for {
        email <- genEmail
        url   <- arbitrary[String]
      } yield M.PasswordResetRequested(email, url)

    case T.LandingPageHit =>
      for {
        email      <- genEmail
        name       <- arbitrary[String]
        msg        <- arbitrary[Option[String]]
        newsletter <- arbitrary[Boolean]
      } yield M.LandingPageHit(email, name, msg, newsletter)

    case T.ReportClientError =>
      for {
        userId    <- genUserIdO
        nameKey   <- arbitrary[String]
        nameValue <- arbitrary[Option[String]]
        msgKey    <- arbitrary[String]
        msgValue  <- arbitrary[Option[String]]
        info      <- arbitrary[Map[String, String]]
      } yield {
        var data = info
        for (v <- nameValue) data = data.updated(nameKey, v)
        for (v <- msgValue) data = data.updated(msgKey, v)
        M.ReportClientError(userId, nameKey, msgKey, data)
      }

    case T.ReportServerError =>
      for {
        userId    <- genUserIdO
        nameKey   <- arbitrary[String]
        nameValue <- arbitrary[Option[String]]
        msgKey    <- arbitrary[String]
        msgValue  <- arbitrary[Option[String]]
        info      <- arbitrary[Map[String, String]]
      } yield {
        var data = info
        for (v <- nameValue) data = data.updated(nameKey, v)
        for (v <- msgValue) data = data.updated(msgKey, v)
        M.ReportServerError(userId, nameKey, msgKey, data)
      }

    case T.DummyTask =>
      for {
        desc             <- arbitrary[String]
        async            <- arbitrary[Boolean]
        processingTimeMs <- arbitrary[Long]
        retryCount       <- arbitrary[Short]
        retryDelaySec    <- arbitrary[Int]
        failureMsg       <- arbitrary[Option[String]]
      } yield M.DummyTask(desc, async, processingTimeMs, retryCount, retryDelaySec, failureMsg)

    case T.SendDiagEmail =>
      for {
        email   <- genEmail
        subject <- arbitrary[String]
        body    <- arbitrary[String]
      } yield M.SendDiagEmail(email, subject, body)

    case T.SyncToMailingList =>
      for {
        sql <- arbitrary[Option[String]]
      } yield M.SyncToMailingList(sql)

    case T.UserFeedbackReceived =>
      for {
        userId   <- genUserId
        feedback <- arbitrary[String]
        metadata <- arbitrary[Map[String, String]]
      } yield M.UserFeedbackReceived(userId, feedback, metadata)
  }

//  def genMsgOfEachType: Gen[List[Msg]] =
//    Gen.sequence[List, Msg](taskDefClasses.toList map genMsg)
}
