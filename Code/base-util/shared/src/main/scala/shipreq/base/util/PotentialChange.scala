package shipreq.base.util

import japgolly.microlibs.nonempty.NonEmpty
import japgolly.univeq.UnivEq
import scalaz.{-\/, Equal, \/, \/-}
import PotentialChange._

sealed abstract class PotentialChange[+E, +A] {
  final def foreach(f: A => Unit): Unit =
    this match {
      case Success(a)             => f(a)
      case Failure(_) | Unchanged => ()
    }

  final def map[B](f: A => B): PotentialChange[E, B] =
    flatMap(a => Success(f(a)))

  final def flatMap[B, EE >: E](f: A => PotentialChange[EE, B]): PotentialChange[EE, B] =
    this match {
      case Success(a)   => f(a)
      case x@Failure(_) => x
      case Unchanged    => Unchanged
    }

  final def mapFailure[F](f: E => F): PotentialChange[F, A] =
    flatMapFailure(e => Failure(f(e)))

  final def flatMapFailure[F, AA >: A](f: E => PotentialChange[F, AA]): PotentialChange[F, AA] =
    this match {
      case x@Success(_) => x
      case Failure(e)   => f(e)
      case Unchanged    => Unchanged
    }

  final def getUpdate: Option[A] =
    this match {
      case Success(a)             => Some(a)
      case Failure(_) | Unchanged => None
    }

  final def getFailure: Option[E] =
    this match {
      case Success(_) | Unchanged => None
      case Failure(e)             => Some(e)
    }

  final def isFailure: Boolean =
    this match {
      case Success(_) | Unchanged => false
      case Failure(_)             => true
    }

  final def isUnchanged: Boolean =
    this match {
      case Success(_) | Failure(_) => false
      case Unchanged               => true
    }

  @inline final def isChanged: Boolean =
    !isUnchanged

  /** [[Unchanged]] is considered valid. */
  final def validity: Validity =
    Invalid when isFailure

  final def compare(f: A => Permission): PotentialChange[E, A] =
    this match {
      case Success(a) if f(a) is Deny => Unchanged
      case _                          => this
    }

  final def filter(f: A => Boolean): PotentialChange[E, A] =
    compare(Allow.fnToThisWhen(f))

  final def ignore(f: A => Boolean): PotentialChange[E, A] =
    compare(Deny.fnToThisWhen(f))

  final def ignoreValue[AA >: A](a: => AA)(implicit e: Equal[AA]): PotentialChange[E, A] =
    ignore(e.equal(a, _))

  final def ignoreOption[AA >: A](o: => Option[AA])(implicit e: Equal[AA]): PotentialChange[E, A] =
    ignore(a => o.fold(false)(e.equal(a, _)))

  final def ignoreEmpty[AA >: A, B](implicit p: NonEmpty.Proof[AA, B]): PotentialChange[E, B] =
    flatMap(PotentialChange.nonEmpty[AA, B](_)(p))

  final def setDiff[B](prev: Set[B])(implicit ev: A <:< Set[B], univEq: UnivEq[B]): PotentialChange[E, SetDiff.NE[B]] =
    flatMap(a => PotentialChange.fromOption(NonEmpty(SetDiff.compare(prev, ev(a)))))

  final def setDiffOption[B](prev: Option[Set[B]])(implicit ev: A <:< Set[B], univEq: UnivEq[B]): PotentialChange[E, SetDiff.NE[B]] =
    flatMap(a => PotentialChange.fromOption(NonEmpty(SetDiff.compareOption(prev, ev(a)))))

  final def toOption: Option[A] =
    this match {
      case Success(a)             => Some(a)
      case Failure(_) | Unchanged => None
    }

  final def toDisjOption: E \/ Option[A] =
    this match {
      case Success(a) => \/-(Some(a))
      case Unchanged  => \/-(None)
      case Failure(e) => -\/(e)
    }
}

object PotentialChange {

  sealed trait NonFailure[+A] extends PotentialChange[Nothing, A] {
    final def foldNonFailure[B](changed: A => B, unchanged: => B): B =
      this match {
        case Success(a) => changed(a)
        case Unchanged  => unchanged
      }
  }

  sealed trait Changed[+E, +A] extends PotentialChange[E, A]

  case object Unchanged extends NonFailure[Nothing]

  final case class Success[+A](update: A) extends NonFailure[A] with Changed[Nothing, A]

  final case class Failure[+E](failure: E) extends Changed[E, Nothing]

  implicit def univEq[E: UnivEq, A: UnivEq]: UnivEq[PotentialChange[E, A]] =
    UnivEq.derive

  import scalaz.{\/, \/-, -\/}

  def fromDisjunction[E, A](d: E \/ A): PotentialChange[E, A] =
    d match {
      case \/-(a) => Success(a)
      case -\/(e) => Failure(e)
    }

  def fromOption[A](o: Option[A]): NonFailure[A] =
    o match {
      case Some(a) => Success(a)
      case None    => Unchanged
    }

  def nonEmpty[A, B](a: A)(implicit p: NonEmpty.Proof[A, B]): NonFailure[B] =
    fromOption(p tryProve a)
}