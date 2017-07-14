package shipreq.base.util

import java.time.{Duration, Instant}
import scalaz._
import scalaz.std.function.function0Instance
import scalaz.syntax.catchable._

/**
  * The chosen target for algebra interpretation.
  *
  * Usage: import shipreq.base.util.Fx._
  */
object Fx {

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
        Fx(throw err)
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Fx {
    def apply[A](a: => A): Fx[A] =
      fxInstance.point(a)

    def pure[A](a: A): Fx[A] =
      Free.pure(a)

    val unit: Fx[Unit] =
      Free.pure(())

    val now: Fx[Instant] =
      apply(Instant.now())
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class FxOps[A](private val fx: Fx[A]) extends AnyVal {

    @inline def unsafeRun(): A =
      fx.go(_())

    def measureDuration: Fx[(Duration, A)] =
      for {
        start <- Fx.now
        a     <- fx
        end   <- Fx.now
      } yield (Duration.between(start, end), a)

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
  }

}
