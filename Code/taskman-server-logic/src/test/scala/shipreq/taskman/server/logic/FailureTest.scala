package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.util.ArticulateError
import shipreq.taskman.server.logic.Failure._
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.TestHelpers._
import shipreq.taskman.server.logic.Worker._
import utest._

object FailureTest extends TestSuite {

  private val genericError = ArticulateError("NO!")
  private val deterministicError = ArticulateError("ALWAYS NO!").tagDeterministic
  private val ctx_det = FailureCtx(node1, worker2, td_1, deterministicError, timeNow)
  private val ctx_nd = FailureCtx(node1, worker2, td_1, genericError, timeNow)

  implicit class Assertions(private val self: Option[FailureResponse]) extends AnyVal {
    def assertReactWith(f: FailedJobReaction) =
      assert(self.map(_.reaction) == Some(f))

    def assertRetryIn(d: Duration)(implicit c: FailureCtx) =
      assertReactWith(UpdateTaskRetry(c.node, c.worker, c.taskDetail, d))

    def assertNotifySupport(): Unit =
      assert(self.map(_.additionalOps).exists(_.count(_.isInstanceOf[NotifySupportWorkerFailed]) == 1))
  }

  override def tests = Tests {
    "abortDeterministicErrors" - {
      implicit val c = ctx_det
      val test = abortDeterministicErrors.partial

      "abort when error is deterministic" - {
        test(c).assertReactWith(UpdateTaskAbort(c.node, c.worker, c.taskDetail))
      }

      "notify support when error is deterministic" - {
        test(c).assertNotifySupport()
      }

      "pass through when error is not deterministic" - {
        test(ctx_nd) ==> None
      }
    }

    "retryAndNotify" - {
      val test = retryAndNotify.partial

      "on first failure, retry in 30s and notify support" - {
        implicit val c = lenses.failureCtx.failureCountL.replace(0)(ctx_nd)
        val result = test(c)
        result.assertRetryIn(30 seconds)
        result.assertNotifySupport()
      }

      "on second failure, retry in 90s and notify support" - {
        implicit val c = lenses.failureCtx.failureCountL.replace(1)(ctx_nd)
        val result = test(c)
        result.assertRetryIn(90 seconds)
        result.assertNotifySupport()
      }

      "on 20th failure before cutoff, retry in 4h and notify support" - {
        implicit val c = lenses.failureCtx.failureCountL.replace(19)(ctx_nd)
        val result = test(c)
        result.assertRetryIn(4 hours)
        result.assertNotifySupport()
      }

      "on 20th failure after cutoff, pass through" - {
        implicit val c = lenses.failureCtx.failureCountL.replace(19)(ctx_nd).copy(now = timeNow plus 2.days)
        test(c) ==> None
      }
    }
  }
}

