package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.util.log.HasLogger
import shipreq.base.util.{?=>, FnWithFallback}
import shipreq.taskman.api.Priority
import shipreq.taskman.api.Task.DummyTask
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.Worker.{FailureCtx, FailurePolicy, FailureResponse}

object Failure extends HasLogger {

  private def chooseByIndex[A, B](f: A => Int, values: IndexedSeq[B]): A ?=> B = {
    val vs = values.map(Some(_)).toVector
    FnWithFallback.optionKleisli { a =>
      val i = f(a)
      if (i >= vs.length) None else vs(i)
    }
  }

  // ===================================================================================================================

  type Attempt[A] = FailureCtx ?=> A
  type Rule = Attempt[FailureResponse]
  type RetryRule = Attempt[Duration]

  def chooseByFailureCount[A](values: A*): Attempt[A] =
    chooseByIndex(_.taskDetail.failureCount, values.toIndexedSeq)

  def retryEveryUntil(every: Duration, cutoff: Duration): RetryRule = {
    val someEvery = Some(every)
    FnWithFallback.optionKleisli { ctx =>
      val retryExpiry = ctx.taskDetail.hdr.created plus cutoff
      if (ctx.now.isAfter(retryExpiry)) None else someEvery
    }
  }

  def addOp(r: FailureResponse, op: ServerOp[Unit]): FailureResponse =
    r.copy(additionalOps = op :: r.additionalOps)

  def retryResponse(ctx: FailureCtx, delay: Duration): FailureResponse =
    FailureResponse(UpdateTaskRetry(ctx.node, ctx.worker, ctx.taskDetail, delay), Nil)

  def notifySupport(ctx: FailureCtx): ServerOp[Unit] =
    if (ctx.err is Deliberate)
      Nop
    else
      NotifySupportWorkerFailed(ctx.now, ctx.taskDetail, ctx.err)

  val abortAndDontNotify: FailurePolicy =
    ctx => FailureResponse(UpdateTaskAbort(ctx.node, ctx.worker, ctx.taskDetail), Nil)

  val abortAndNotify: FailurePolicy =
    ctx => FailureResponse(UpdateTaskAbort(ctx.node, ctx.worker, ctx.taskDetail), notifySupport(ctx) :: Nil)

  def abortDeterministicErrors: Rule =
    FnWithFallback.when((_: FailureCtx).err.isDeterministic)(abortAndNotify)

  def dummyTaskRules: Rule =
    FnWithFallback(f => (ctx: FailureCtx) =>
      ctx.taskDetail.task match {
        case _: DummyTask if ctx.err.isDeterministic => abortAndDontNotify(ctx)
        case m: DummyTask                            => retryResponse(ctx, m.retryDelaySec seconds)
        case _                                       => f(ctx)
      }
    )

  val impatientRetries: RetryRule =
    chooseByFailureCount(
      10 seconds    // Failure #1,  next attempt @ 10 sec
      , 15 seconds  // Failure #2,  next attempt @ 25 sec
      , 20 seconds  // Failure #3,  next attempt @ 45 sec
      , 35 seconds  // Failure #4,  next attempt @ 80 sec
      , 40 seconds  // Failure #5,  next attempt @ 2 min
      , 60 seconds  // Failure #6,  next attempt @ 3 min
      , 90 seconds  // Failure #7,  next attempt @ 4.5 min
      , 150 seconds // Failure #8,  next attempt @ 7 min
      , 4 minutes   // Failure #9,  next attempt @ 11 min
      , 6 minutes   // Failure #10, next attempt @ 17 min
      , 13 minutes  // Failure #11, next attempt @ 30 min
      , 20 minutes  // Failure #12, next attempt @ 50 min
      , 30 minutes  // Failure #13, next attempt @ 80 min
      , 40 minutes  // Failure #14, next attempt @ 2 hr
    ) | retryEveryUntil(1 hours, 24 hours)

  val patientRetries: RetryRule =
    chooseByFailureCount(
      30 seconds   // Failure #1, next attempt @ 30 sec
      , 90 seconds // Failure #2, next attempt @  2 min
      , 3 minutes  // Failure #3, next attempt @  5 min
      , 5 minutes  // Failure #4, next attempt @ 10 min
      , 10 minutes // Failure #5, next attempt @ 20 min
      , 15 minutes // Failure #6, next attempt @ 35 min
      , 25 minutes // Failure #7, next attempt @  1 hr
      , 1 hours    // Failure #8, next attempt @  2 hr
      , 2 hours    // Failure #9, next attempt @  4 hr
    ) | retryEveryUntil(4 hours, 24 hours)

  val priorityBasedRetryRule: RetryRule =
    FnWithFallback.choose((ctx: FailureCtx) =>
      if (ctx.taskDetail.priority.value >= Priority.High.value)
        impatientRetries
      else
        patientRetries)

  val retryAndNotify: Rule =
    priorityBasedRetryRule
      .mapWithInput((ctx, dur) => addOp(retryResponse(ctx, dur), notifySupport(ctx)))

  val failurePolicy: FailurePolicy =
    (dummyTaskRules | abortDeterministicErrors | retryAndNotify) withFallback abortAndNotify
}
