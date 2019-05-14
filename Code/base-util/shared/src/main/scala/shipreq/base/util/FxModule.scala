package shipreq.base.util

import java.time.{Duration, Instant}
import scalaz._
import scalaz.std.function.function0Instance
import scalaz.syntax.catchable._

/**
  * The chosen target for algebra interpretation.
  *
  * Usage: import shipreq.base.util.FxModule._
  */
object FxModule {

  type Fx[A] = Free.Trampoline[A]

  implicit val fxInstance: Monad[Fx] with BindRec[Fx] =
    Free.trampolineInstance

  implicit val fxInstanceCatchable: Catchable[Fx] =
    new Catchable[Fx] {
      override def attempt[A](f: Fx[A]): Fx[Throwable \/ A] =
        Fx(
          try \/-(f.unsafeRun())
          catch { case t: Throwable => -\/(t) })

      override def fail[A](err: Throwable): Fx[A] =
        Fx.fail(err)
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Fx {
    def apply[A](a: => A): Fx[A] =
      fxInstance.point(a)

    def pure[A](a: A): Fx[A] =
      Free.pure(a)

    /** In case there are errors in construction (!) */
    def safe[A](fa: => Fx[A]): Fx[A] =
      Fx(fa).flatMap(Identity.apply)

    def fail[A](err: Throwable): Fx[A] =
      Fx(throw err)

    def lift[A](fa: Throwable \/ A): Fx[A] =
      fa.fold(fail, pure)

    val unit: Fx[Unit] =
      Free.pure(())

    val now: Fx[Instant] =
      apply(Instant.now())
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class FxOps[A](private val fx: Fx[A]) extends AnyVal {

    @inline def unsafeRun(): A =
      fx.go(_())

    def tap[B](f: A => Fx[B]): Fx[A] =
      for {
        a <- fx
        _ <- f(a)
      } yield a

    def tap_[B](f: Fx[B]): Fx[A] =
      tap(_ => f)

    def unsafeTap[B](f: A => B): Fx[A] =
      tap(a => Fx(f(a)))

    def measureDuration: Fx[(A, Duration)] =
      for {
        start <- Fx.now
        a     <- fx
        end   <- Fx.now
      } yield (a, Duration.between(start, end))

    def measureDuration_ : Fx[Duration] =
      for {
        start <- Fx.now
        _     <- fx
        end   <- Fx.now
      } yield Duration.between(start, end)

    /**
      * @param inspect Inspect the result for failure/success
      * @param continue Int arg            = number of attempts thus far (i.e. ≥ 1)
      *                 Result of None     = concede failure; abort
      *                 Result of Some(fx) = continue; run fx between retries
      */
    def retry[E, B](inspect: A => E \/ B)(continue: (Int, E) => Option[Fx[Unit]]): Fx[E \/ B] =
      fxInstance.tailrecM((n: Int) =>
        fx.flatMap[Int \/ (E \/ B)](a =>
          inspect(a) match {
            case \/-(b) => Fx.pure(\/-(\/-(b)))
            case -\/(e) =>
              val attempts = n + 1
              continue(attempts, e) match {
                case Some(x) => x.map(_ => -\/(attempts))
                case None    => Fx.pure(\/-(-\/(e)))
              }
          })
      )(0)

    /**
      * @param continue Int arg            = number of attempts thus far (i.e. ≥ 1)
      *                 Result of None     = concede failure; abort
      *                 Result of Some(fx) = continue; run fx between retries
      */
    def retryOnException(continue: (Int, Throwable) => Option[Fx[Unit]]): Fx[A] =
      fx.attempt.retry(identity)(continue).map(_.fold(throw _, identity))

    /** Executes the handler if an exception is raised. */
    def recoverException[AA >: A](handler: Throwable => Fx[AA]): Fx[AA] =
      fx.attempt.flatMap {
        case \/-(a) => Fx.pure(a)
        case -\/(e) => handler(e)
      }

    def recoverArticulateError[AA >: A](handler: ArticulateError => Fx[AA]): Fx[AA] =
      recoverException(t => handler(ArticulateError(t)))

    def attemptArticulateError: Fx[ArticulateError \/ A] =
      fx.attempt.map(_.leftMap(ArticulateError(_)))

    def mapArticulateError(f: ArticulateError => Throwable): Fx[A] =
      recoverArticulateError(e => Fx.fail(f(e)))

    /** Like `finally`, but only performs the final action if there was an exception. */
    def onException[B](action: Throwable => Fx[B]): Fx[A] =
      recoverException(e => action(e).flatMap(_ => Fx.fail(e)))

    /** Like `finally`, but only performs the final action if there was an exception. */
    def onArticulateError[B](action: ArticulateError => Fx[B]): Fx[A] =
      onException(t => action(ArticulateError(t)))

    def andFinally[B](finallyClause: Fx[B]): Fx[A] =
      for {
        r <- onException(_ => finallyClause)
        _ <- finallyClause
      } yield r

    def bracket[B, C](release: A => Fx[B], use: A => Fx[C]): Fx[C] =
      fx.flatMap(a => use(a).andFinally(release(a)))

    def bracket_[B, C](release: Fx[B], use: Fx[C]): Fx[C] = {
      val useAndRelease = use.andFinally(release)
      fx.flatMap(_ => useAndRelease)
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class FxOptionOps[A](private val fx: Fx[Option[A]]) extends AnyVal {
    def getOrFail(errMsg: => String): Fx[A] =
      fx.flatMap {
        case Some(a) => Fx pure a
        case None    => Fx fail ArticulateError(errMsg)
      }
  }

}
