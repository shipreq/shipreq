package shipreq.taskman.server.business

import org.joda.time.Period
import org.specs2.matcher.Matcher
import org.specs2.mutable._
import org.specs2.time.NoTimeConversions
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.base.util.Error
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.taskman.server._
import TestHelpers._
import Bop.SendEmail
import Sop._
import Failure._
import Worker._


class FailureTest extends Specification with NoTimeConversions {

  val genericError = Error("NO!")
  val deterministicError = Error("ALWAYS NO!").tag(Deterministic)
  val ctx_det = FailureCtx(node1, worker2, md_1, deterministicError, timeNow)
  val ctx_nd = FailureCtx(node1, worker2, md_1, genericError, timeNow)

  def reactWith(f: FailedJobReaction) =
    be_==(Some(f)) ^^ {(_:Option[FailureResponse]).map(_.reaction)}

  def retryIn(p: Period)(implicit c: FailureCtx) =
    reactWith(UpdateMsgAbort(c.n, c.w, c.m, p))

  def notifySupport(implicit c: FailureCtx): Matcher[Option[FailureResponse]] =
    beSome(contain(beAnInstanceOf[NotifySupportWorkerFailed]).exactly(1)) ^^ {(_:Option[FailureResponse]).map(_.additionalOps)}

  "abortDeterministicErrors" should {
    implicit val c = ctx_det

    "abort when error is deterministic" in {
      abortDeterministicErrors(c) must reactWith(UpdateMsgRetry(c.n, c.w, c.m))
    }

    "notify support when error is deterministic" in {
      abortDeterministicErrors(c) must notifySupport
    }

    "pass through when error is not deterministic" in {
      abortDeterministicErrors(ctx_nd) ==== None
    }
  }

  "retryAndNotify" should {

    "on first failure, retry in 30s and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 0)
      retryAndNotify(c) must retryIn(30 sec) and notifySupport
    }

    "on second failure, retry in 90s and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 1)
      retryAndNotify(c) must retryIn(90 sec) and notifySupport
    }

    "on 20th failure before cutoff, retry in 4h and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 19)
      retryAndNotify(c) must retryIn(4 hr) and notifySupport
    }

    "on 20th failure after cutoff, pass through" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 19).copy(now = timeNow plus 2.days)
      retryAndNotify(c) ==== None
    }
  }

  "handleFailedWorker" should {
    "notify support" in {
      val bop = new MockBops
      val sop = new MockSops
      handleFailedWorker(MockEmails, bop, sop)(sampleNotifySupportWorkerFailed).unsafePerformIO()
      (sop, bop) must haveRun2[Sop, Bop](_.none, _.op[SendEmail])
    }

    "raise a taskman error if fails to notify support" in {
      val bop = crashOnSendEmail(new MockBops)
      val sop = new MockSops
      handleFailedWorker(MockEmails, bop, sop)(sampleNotifySupportWorkerFailed).unsafePerformIO()
      (sop, bop) must haveRun2[Sop, Bop](_.op[NotifySupportTaskmanError], _.op[SendEmail])
    }
  }

  "handleFailedTaskman" should {
    "notify support" in {
      val bop = new MockBops
      handleFailedTaskman(MockEmails, bop)(sampleNotifySupportTaskmanError).unsafePerformIO()
      bop must haveRun[Bop].op[SendEmail]
    }

    "recover if unable to notify support" in {
      val bop = crashOnSendEmail(new MockBops)
      handleFailedTaskman(MockEmails, bop)(sampleNotifySupportTaskmanError).unsafePerformIO()
      bop must haveRun[Bop].op[SendEmail]
    }
  }
}
