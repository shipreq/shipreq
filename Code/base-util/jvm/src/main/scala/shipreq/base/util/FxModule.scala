package shipreq.base.util

import cats.CommutativeApplicative
import cats.effect.IO
import cats.effect.IO.ioEffect
import java.time.{Duration, Instant}
import scala.collection.Factory
import scala.concurrent.blocking
import scalaz.{-\/, BindRec, Catchable, Monad, \/, \/-}

/**
  * The chosen target for algebra interpretation.
  *
  * Usage: import shipreq.base.util.FxModule._
  */
object FxModule {

  type Fx[A] = IO[A]

  implicit object fxScalazInstance extends Monad[Fx] with BindRec[Fx] with Catchable[Fx] {
    override def ap[A, B](fa: => Fx[A])(f: => Fx[A => B]): Fx[B] =
      ioEffect.ap(f)(fa)

    override def point[A](a: => A): Fx[A] =
      IO(a)

    override def bind[A, B](fa: Fx[A])(f: A => Fx[B]): Fx[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Fx[A])(f: A => B): Fx[B] =
      fa.map(f)

    override def tailrecM[A, B](f: A => Fx[A \/ B])(a: A): Fx[B] =
      ioEffect.tailRecM(a)(f(_).map(_.toEither)) // TODO Use either in Fx interface?

    override def attempt[A](f: Fx[A]): Fx[Throwable \/ A] =
      Fx {
        try \/-(f.unsafeRun())
        catch {
          case t: Throwable => -\/(t)
        }
      }

    override def fail[A](err: Throwable): Fx[A] =
      Fx.fail(err)
  }

  implicit object fxCatsInstance extends CommutativeApplicative[Fx] {

    override def pure[A](a: A) =
      IO.pure(a)

    override def ap[A, B](ff: Fx[A => B])(fa: Fx[A]) =
      for {
        a <- fa
        f <- ff
      } yield f(a)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object Fx {
    def apply[A](a: => A): Fx[A] =
      IO(a)

    def pure[A](a: A): Fx[A] =
      IO.pure(a)

    /** In case there are errors in construction (!) */
    def safe[A](fa: => Fx[A]): Fx[A] =
      IO(fa).flatMap(Identity.apply)

    def fail[A](err: Throwable): Fx[A] =
      IO.raiseError(err)

    def lift[A](fa: Throwable \/ A): Fx[A] =
      fa.fold(fail, pure)

    val unit: Fx[Unit] =
      IO.unit

    val now: Fx[Instant] =
      IO(Instant.now())

    def sleepSec(sec: Int): Fx[Unit] =
      sleep(Duration.ofSeconds(sec))

    def sleepMs(ms: Int): Fx[Unit] =
      sleep(Duration.ofMillis(ms))

    def sleep(dur: Duration): Fx[Unit] = {
      val ms = dur.toMillis
      val ns = dur.getNano
      Fx(blocking(Thread.sleep(ms, ns))) // Can this be better?
    }

    def liftTraverse[A, B](f: A => Fx[B]): LiftTraverseDsl[A, B] =
      new LiftTraverseDsl(f)

    final class LiftTraverseDsl[A, B](private val f: A => Fx[B]) extends AnyVal {

      def id: Fx[A => B] =
        Fx(f(_).unsafeRun())

      /** Anything traversable by the Scala stdlib definition */
      def std[T[X] <: IterableOnce[X]](implicit cbf: Factory[B, T[B]]): Fx[T[A] => T[B]] =
        Fx { ta =>
          val r = cbf.newBuilder
          ta.iterator.foreach(a => r += f(a).unsafeRun())
          r.result()
        }

      def option: Fx[Option[A] => Option[B]] =
        Fx(_.map(f(_).unsafeRun()))
    }

    def traverse[T[X] <: IterableOnce[X], A, B](ta: => T[A])(f: A => Fx[B])(implicit cbf: Factory[B, T[B]]): Fx[T[B]] =
      liftTraverse(f).std[T](cbf).map(_(ta))

    def sequence[T[X] <: IterableOnce[X], A](tca: => T[Fx[A]])(implicit cbf: Factory[A, T[A]]): Fx[T[A]] =
      traverse(tca)(Identity.apply)(cbf)

    def traverseOption[A, B](oa: => Option[A])(f: A => Fx[B]): Fx[Option[B]] =
      liftTraverse(f).option.map(_(oa))

    def sequenceOption[A](oca: => Option[Fx[A]]): Fx[Option[A]] =
      traverseOption(oca)(Identity.apply)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  implicit class FxOps[A](private val fx: Fx[A]) extends AnyVal {

    @inline def unsafeRun(): A =
      fx.unsafeRunSync()

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
      ioEffect.tailRecM(0)(n =>
        fx.flatMap[Int Either (E \/ B)](a =>
          inspect(a) match {
            case \/-(b) => Fx.pure(Right(\/-(b)))
            case -\/(e) =>
              val attempts = n + 1
              continue(attempts, e) match {
                case Some(x) => x.map(_ => Left(attempts))
                case None    => Fx.pure(Right(-\/(e)))
              }
          }
        )
      )

    @inline def attemptFx: Fx[Throwable \/ A] =
      fxScalazInstance.attempt(fx)

    /**
      * @param continue Int arg            = number of attempts thus far (i.e. ≥ 1)
      *                 Result of None     = concede failure; abort
      *                 Result of Some(fx) = continue; run fx between retries
      */
    def retryOnException(continue: (Int, Throwable) => Option[Fx[Unit]]): Fx[A] =
      fx.attempt.retry(\/.fromEither)(continue).map(_.fold(throw _, identity))

    /** Executes the handler if an exception is raised. */
    def recoverException[AA >: A](handler: Throwable => Fx[AA]): Fx[AA] =
      fx.attempt.flatMap {
        case Right(a) => Fx.pure(a)
        case Left(e) => handler(e)
      }

    def recoverArticulateError[AA >: A](handler: ArticulateError => Fx[AA]): Fx[AA] =
      recoverException(t => handler(ArticulateError(t)))

    def attemptArticulateError: Fx[ArticulateError \/ A] =
      fx.attempt.map {
        case Right(a) => \/-(a)
        case Left(e) => -\/(ArticulateError(e))
      }

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

    /** Note: This is the init clause. */
    def bracketFx[B, C](release: A => Fx[B], use: A => Fx[C]): Fx[C] =
      fx.bracket(use)(release(_).map(_ => ()))

    /** Note: This is the init clause. */
    def bracketFx_[B, C](release: Fx[B], use: Fx[C]): Fx[C] = {
      val useAndRelease = use.andFinally(release)
      fx.flatMap(_ => useAndRelease)
    }

    def toJavaRunnable: Runnable =
      () => {
        unsafeRun()
        ()
      }

    def withTimeLimit(maxDur: Duration): Fx[Option[A]] =
      Fx(ThreadUtils.unsafeRunWithTimeLimit(maxDur)(fx.unsafeRun()))
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
