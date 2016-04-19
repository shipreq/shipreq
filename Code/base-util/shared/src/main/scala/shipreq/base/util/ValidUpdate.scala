package shipreq.base.util

import ValidUpdate._

sealed abstract class ValidUpdate[+E, +A] {
  final def map[B](f: A => B): ValidUpdate[E, B] =
    flatMap(a => Success(f(a)))

  final def flatMap[B, EE >: E](f: A => ValidUpdate[EE, B]): ValidUpdate[EE, B] =
    this match {
      case Success(a)   => f(a)
      case x@Failure(_) => x
      case Unchanged    => Unchanged
    }

  final def mapFailure[F](f: E => F): ValidUpdate[F, A] =
    flatMapFailure(e => Failure(f(e)))

  final def flatMapFailure[F, AA >: A](f: E => ValidUpdate[F, AA]): ValidUpdate[F, AA] =
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
    Invalid <~ isFailure

  final def compare(f: A => Permission): ValidUpdate[E, A] =
    this match {
      case Success(a) if f(a) :: Deny => Unchanged
      case _                          => this
    }

  final def ignore(f: A => Boolean): ValidUpdate[E, A] =
    compare(Deny <~ f(_))

  final def filter(f: A => Boolean): ValidUpdate[E, A] =
    compare(Allow <~ f(_))

  final def toOption: Option[A] =
    this match {
      case Success(a)             => Some(a)
      case Failure(_) | Unchanged => None
    }
}

object ValidUpdate {

  case class Success[+A](update: A) extends ValidUpdate[Nothing, A]

  case class Failure[+E](failure: E) extends ValidUpdate[E, Nothing]

  case object Unchanged extends ValidUpdate[Nothing, Nothing]

  import scalaz.{\/, \/-, -\/, Validation}

  def fromDisjunction[E, A](d: E \/ A): ValidUpdate[E, A] =
    d match {
      case \/-(a) => Success(a)
      case -\/(e) => Failure(e)
    }

  def fromValidation[E, A](v: Validation[E, A]): ValidUpdate[E, A] =
    v match {
      case scalaz.Success(a) => Success(a)
      case scalaz.Failure(e) => Failure(e)
    }

  def option[A](o: Option[A]): ValidUpdate[Nothing, A] =
    o match {
      case Some(a) => Success(a)
      case None    => Unchanged
    }

  def setDiff[A](d: SetDiff[A]): ValidUpdate[Nothing, SetDiff[A]] =
    if (d.isEmpty)
      Unchanged
    else
      Success(d)

  def setDiffNE[A](d: SetDiff[A]): ValidUpdate[Nothing, SetDiff.NE[A]] =
    setDiff(d).map(NonEmpty.force)
}