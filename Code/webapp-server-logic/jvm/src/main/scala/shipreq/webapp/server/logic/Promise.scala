package shipreq.webapp.server.logic

import japgolly.univeq.UnivEq
import monocle._
import java.time.Instant
import scalaz.{-\/, Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util.Retries

sealed trait Promise[+E, +A] {
  def map[B](f: A => B): Promise[E, B]
}

object Promise {

  final case class Available[+A](value: A) extends Promise[Nothing, A] {
    override def map[B](f: A => B): Promise[Nothing, B] =
      Available(f(value))
  }

  final case class Failure[+E](error: E) extends Promise[E, Nothing] {
    override def map[B](f: Nothing => B) = this
  }

  final case class InProgress(started: Instant) extends Promise[Nothing, Nothing] {
    override def map[B](f: Nothing => B) = this
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait GetOrSet[+E, +A] {
    def map[B](f: A => B): GetOrSet[E, B]
  }

  object GetOrSet {
    final case class Success[+A](value: A) extends GetOrSet[Nothing, A] {
      override def map[B](f: A => B) = Success(f(value))
    }

    sealed trait Failure[+E] extends GetOrSet[E, Nothing] {
      final override def map[B](f: Nothing => B) = this
    }
    final case class CustomFailure[+E](error: E) extends Failure[E]
    case object NoPromise extends Failure[Nothing]
    case object Timeout extends Failure[Nothing]

    implicit def univEq[E: UnivEq, A: UnivEq]: UnivEq[GetOrSet[E, A]] =
      UnivEq.force
  }

  final case class Optics[V, S, E, A](fieldS: Optional[Option[V], S], fieldP: Lens[S, Promise[E, A]]) {
    val fieldSP: Optional[Option[V], Promise[E, A]] =
      fieldS ^|-> fieldP
  }

  type InitFn[F[_], E, S, A] = (S, E) \/ S => Option[F[E \/ A]]

  /**
    * Given an optional store value: Store -> K -> Option V -> Option S
    * And a promise therein: S -> Promise[E, A]
    *
    * This will atomically and asynchronously-safely get an S and it's available A,
    * or attempt to initialise an E \/ A and set it itself.
    *
    * When an A is returned, the S from the same atomic transaction is always returned too, thus you can be sure that
    * both values will be in sync.
    */
  def getOrSet[F[_], K, V >: Null, E, S, A](store     : Store.Algebra[F, K, V],
                                            optics    : Optics[V, S, E, A])
                                           (key       : K,
                                            retries   : Retries,
                                            initFn    : InitFn[F, E, S, A])
                                           (implicit F: Monad[F],
                                            time      : Server.Time[F]): F[GetOrSet[E, (S, A)]] = {
    type Result = GetOrSet[E, (S, A)]
    import optics._

    /** React according to what's in the store */
    def main(ov: Option[V]): F[Result] =
      fieldS.getOption(ov) match {
        case Some(s) =>
          fieldP.get(s) match {

            case p: Available[A] =>
              F pure GetOrSet.Success((s, p.value))

            case p: InProgress =>
              time.now.flatMap { now =>
                if (!retries.isEmpty && p.started.isBefore(now.minus(retries.totalTime)))
                  // InProgress expired, we still have retry budget, try ourselves
                  initOr(\/-(s), GetOrSet.Timeout)
                else
                  retryOr(GetOrSet.Timeout)
              }

            // Previously failed
            case Failure(e) =>
              initOr(-\/((s, e)), GetOrSet.CustomFailure(e))
          }

        // No node in store
        case None => F pure GetOrSet.NoPromise
      }

    /** Init if allowed, or return `orResult` */
    def initOr(input: (S, E) \/ S, orResult: Result): F[Result] =
      initFn(input) match {
        case Some(fea) => runInit(fea)
        case None      => F pure orResult
      }

    /** Retry if allowed, or return `orResult` */
    def retryOr(orResult: Result): F[Result] =
      retries.pop match {
        case Some((delay, nextRetries)) =>
          // InProgress, wait and check again
          val next = getOrSet(store, optics)(key, nextRetries, initFn)
          time.delay(next, delay)
        case None =>
          // InProgress, no more retries
          F pure orResult
      }

    /** Evaluate the InitFn and update the store when it's done */
    def runInit(fea: F[E \/ A]): F[Result] = {
      type IsSelf = Promise[E, A] => Boolean

      def attemptBegin(ifWin: IsSelf => F[Result]): F[Result] =
        for {
          now <- time.now
          p = InProgress(now)
          isSelf = (_: Promise[E, A]) eq p
          ov ← store.storeModO(key)(fieldSP.modify {
            case a: Available[A] => a
            case _: Failure[E] => p
            case q: InProgress => if (q.started.isAfter(p.started)) q else p
          })
          won = fieldSP.getOption(ov).exists(isSelf)
          result <- if (won) ifWin(isSelf) else main(ov)
        } yield result

      def beginInit(isSelf: IsSelf): F[Result] =
        fea.flatMap {
          case \/-(a) => installSuccess(a)
          case -\/(e) => installFailure(isSelf, e)
        }

      def installSuccess(a: A): F[Result] =
        store.storeModO(key)(
          fieldSP.modify {
            case _: InProgress | _: Failure[E] => Available(a)
            case wonRace: Available[A]         => wonRace
          }
        ).flatMap(ov =>
          fieldS.getOption(ov) match {
            case Some(s) => fieldP.get(s) match {
              case p: Available[A]            => F pure GetOrSet.Success((s, p.value))
              case Failure(_) | InProgress(_) => installSuccess(a)
            }
            case None                         => F pure GetOrSet.NoPromise
          }
        )

      def installFailure(isSelf: IsSelf, e: E): F[Result] =
        store.storeModO(key)(
          fieldSP.modify(p => if (isSelf(p)) Failure(e) else p)
        ).flatMap(ov =>
          fieldS.getOption(ov) match {
            case Some(s) => fieldP.get(s) match {
              case Failure(f)    => F pure GetOrSet.CustomFailure(f)
              case Available(a)  => F pure GetOrSet.Success((s, a))
              case InProgress(_) => retryOr(GetOrSet.CustomFailure(e))
            }
            case None            => F pure GetOrSet.NoPromise
          }
        )

      attemptBegin(beginInit)
    }

    store.storeGet(key).flatMap(main)
  }

}
