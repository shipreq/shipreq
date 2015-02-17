package shipreq.base.util

import scalaz.{Monad, \/}

/**
 * A value that we believe must exist but cannot prove to exist via types.
 *
 * [[japgolly.nyaya]] should be used to assert the pertinent invariants.
 *
 * In the event that this fails, there will likely be side-effects in future.
 * Something like the
 */
sealed abstract class Must[+A] {
  def map    [B](f: A => B)                : Must[B]
  def flatMap[B](f: A => Must[B])          : Must[B]
  def fold   [B](e: String => B, f: A => B): B
}

object Must {

  implicit val scalazInstance: Monad[Must] =
    new Monad[Must] {
      override def point[A]   (a: => A)                     : Must[A] = Exists(a)
      override def map  [A, B](fa: Must[A])(f: A => B)      : Must[B] = fa map f
      override def bind [A, B](fa: Must[A])(f: A => Must[B]): Must[B] = fa flatMap f
    }

  //implicit def equality[A: Equal]: Equal[Must[A]] =
  //  def equal(a1: F, a2: F): Boolean
  //  def equalIsNatural: Boolean = false

  @inline implicit def autoMust[A](a: A): Must[A] = Exists(a)

//  implicit class opeortpewort[A](val o: Option[A]) extends AnyVal {
//    def toMust(explanation: => String): Must[A] = fromOption(o, explanation)
//  }

  def fromOption[A](o: Option[A], explanation: => String): Must[A] =
    o.fold[Must[A]](Failed(explanation))(Exists(_))

  def fromDisjunction[A](d: String \/ A): Must[A] =
    d.fold[Must[A]](Failed, Exists(_))

  final case class Exists[A](value: A) extends Must[A] {
    override def map    [B](f: A => B)                : Must[B] = Exists(f(value))
    override def flatMap[B](f: A => Must[B])          : Must[B] = f(value)
    override def fold   [B](e: String => B, f: A => B): B       = f(value)
  }

  final case class Failed(explanation: String) extends Must[Nothing] {
    override def map    [B](f: Nothing => B)                : Must[B] = this
    override def flatMap[B](f: Nothing => Must[B])          : Must[B] = this
    override def fold   [B](e: String => B, f: Nothing => B): B       = e(explanation)
  }

}