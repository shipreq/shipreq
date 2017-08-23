package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import org.specs2.matcher.Matcher
import org.specs2.mutable._
import shipreq.base.util.ArticulateError
import TestHelpers._
import ServerOp._
import Failure._
import Worker._

class FailureTest extends Specification {

  val genericError = ArticulateError("NO!")
  val deterministicError = ArticulateError("ALWAYS NO!").tagDeterministic
  val ctx_det = FailureCtx(node1, worker2, md_1, deterministicError, timeNow)
  val ctx_nd = FailureCtx(node1, worker2, md_1, genericError, timeNow)

  def reactWith(f: FailedJobReaction) =
    beSome(f) ^^ {(_:Option[FailureResponse]).map(_.reaction)}

  def retryIn(d: Duration)(implicit c: FailureCtx) =
    reactWith(UpdateMsgRetry(c.node, c.worker, c.msg, d))

  def notifySupport(implicit c: FailureCtx): Matcher[Option[FailureResponse]] =
    beSome(contain(beAnInstanceOf[NotifySupportWorkerFailed]).exactly(1)) ^^ {(_:Option[FailureResponse]).map(_.additionalOps)}

  "abortDeterministicErrors" should {
    implicit val c = ctx_det
    val test = abortDeterministicErrors.partial

    "abort when error is deterministic" in {
      test(c) must reactWith(UpdateMsgAbort(c.node, c.worker, c.msg))
    }

    "notify support when error is deterministic" in {
      test(c) must notifySupport
    }

    "pass through when error is not deterministic" in {
      test(ctx_nd) ==== None
    }
  }

  "retryAndNotify" should {
    val test = retryAndNotify.partial

    "on first failure, retry in 30s and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 0)
      test(c) must retryIn(30 seconds) and notifySupport
    }

    "on second failure, retry in 90s and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 1)
      test(c) must retryIn(90 seconds) and notifySupport
    }

    "on 20th failure before cutoff, retry in 4h and notify support" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 19)
      test(c) must retryIn(4 hours) and notifySupport
    }

    "on 20th failure after cutoff, pass through" in {
      implicit val c = lenses.failureCtx.failureCountL.set(ctx_nd, 19).copy(now = timeNow plus 2.days)
      test(c) ==== None
    }
  }
}
