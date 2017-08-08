package shipreq.taskman.server

import java.time.Duration
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Msg.DummyMsg
import shipreq.taskman.api.Priority
import Sop._
import Worker.{FailureResponse, FailurePolicy, FailureCtx}

object Failure extends HasLogger {

  def composeF[R,A,B,C](h: B => A => C, g: R => B): R => A => C =
    r => h(g(r))

  def mapO[A,B,C](g: A => B => C)(f: A => Option[B]): A => Option[C] =
    c => f(c).map(a => g(c)(a))

  def ifO[A, B](p: A => Boolean, f: A => B): A => Option[B] =
    a => if (p(a)) Some(f(a)) else None

  def chooseByIndex[A, B](f: A => Int, values: IndexedSeq[B]): A => Option[B] = {
    val vs = values.map(Some(_)).toVector
    a => {
      val i = f(a)
      if (i >= vs.length) None else vs(i)
    }
  }

  implicit class PriMExt[A, B](val f: A => Option[B]) extends AnyVal {
    def ?>>?(g: A => Option[B]): A => Option[B] =
      a => f(a) orElse g(a)

    def ?>>(g: A => B): A => B =
      a => f(a) getOrElse g(a)

    def =<<[C](g: A => B => C): A => Option[C] =
      mapO(g)(f)
  }

  // ===================================================================================================================

  type Attempt[A] = FailureCtx => Option[A]
  type Rule = Attempt[FailureResponse]
  type RetryRule = Attempt[Duration]

  def chooseByFailureCount[A](values: A*): Attempt[A] =
    chooseByIndex(_.m.failureCount, values.toIndexedSeq)

  def retryEveryUntil(every: Duration, cutoff: Duration): RetryRule = {
    val everyS = Some(every)
    ctx => {
      val retryExpiry = ctx.m.created plus cutoff
      if (ctx.now.isAfter(retryExpiry)) None else everyS
    }
  }

  def addOp(op: Sop[Unit])(r: FailureResponse): FailureResponse =
    r.copy(additionalOps = op :: r.additionalOps)

  def addOpF(op: FailureCtx => Sop[Unit]) =
    composeF(addOp, op)

  def retryResponse(ctx: FailureCtx)(delay: Duration): FailureResponse =
    FailureResponse(UpdateMsgRetry(ctx.n, ctx.w, ctx.m, delay), Nil)

  def notifySupport(ctx: FailureCtx): Sop[Unit] =
    if (ctx.err is Deliberate)
      Nop
    else
      NotifySupportWorkerFailed(ctx.now, ctx.m, ctx.err)

  val abortAndDontNotify: FailurePolicy =
    ctx => FailureResponse(UpdateMsgAbort(ctx.n, ctx.w, ctx.m), Nil)

  val abortAndNotify: FailurePolicy =
    ctx => FailureResponse(UpdateMsgAbort(ctx.n, ctx.w, ctx.m), notifySupport(ctx) :: Nil)

  def abortDeterministicErrors: Rule =
    ifO(_.err is Deterministic, abortAndNotify)

  def dummyMsgRules: Rule =
    ctx => ctx.m.msg match {
      case m: DummyMsg =>
        if (ctx.err is Deterministic)
          Some(abortAndDontNotify(ctx))
        else
          Some(retryResponse(ctx)(m.retryDelaySec seconds))
      case _ => None
    }

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
    ) ?>>?
      retryEveryUntil(1 hours, 24 hours)

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
    ) ?>>?
      retryEveryUntil(4 hours, 24 hours)

  val priorityBasedRetryRule: RetryRule =
    ctx => {
      val p = ctx.m.priority.value
      val r: RetryRule =
        if (p >= Priority.High.value) impatientRetries
        else patientRetries
      r(ctx)
    }

  val retryAndNotify: Rule =
    priorityBasedRetryRule =<< retryResponse =<< addOpF(notifySupport)

  val failurePolicy: FailurePolicy =
    dummyMsgRules ?>>? abortDeterministicErrors ?>>? retryAndNotify ?>> abortAndNotify
}
