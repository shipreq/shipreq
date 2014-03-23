package shipreq.taskman.server.business

import org.joda.time.Period
import org.specs2.matcher.Matcher
import org.specs2.mutable._
import shipreq.base.util.Error
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.taskman.server._
import TestHelpers._
import Sop._
import Failure._
import Worker._

class FailureTest extends Specification {

  val genericError = Error.error("NO!")
  val deterministicError = Error.error("ALWAYS NO!").tag(Deterministic)
  val ctx_det = FailureCtx(md_1, deterministicError, timeNow)
  val ctx_nd = FailureCtx(md_1, genericError, timeNow)

  override implicit def intToRichLong(v: Int): Nothing = ??? //new longAsTime(v.toLong) // fuck off specs2

  def reactWith(f: FailedJobReaction) =
    be_==(Some(f)) ^^ {(_:Option[FailureResponse]).map(_.reaction)}

  def retryIn(p: Period)(implicit c: FailureCtx) =
    reactWith(MsgFailedRetry(c.m, p))

  def notifySupport(implicit c: FailureCtx): Matcher[Option[FailureResponse]] =
    beSome(contain(beAnInstanceOf[NotifySupportWorkerFailed]).exactly(1)) ^^ {(_:Option[FailureResponse]).map(_.additionalOps)}

  "abortDeterministicErrors" should {
    implicit val c = ctx_det

    "abort when error is deterministic" in {
      abortDeterministicErrors(c) must reactWith(MsgFailedAbort(c.m))
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
}
