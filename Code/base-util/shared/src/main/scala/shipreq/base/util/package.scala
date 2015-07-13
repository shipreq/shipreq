package shipreq.base

import scalaz.\/

package object util {

  type ErrorOr[A] = Error \/ A

  /** Faster than Vector(a) */
  @inline def Vector1[A](a: A): Vector[A] = Vector.empty :+ a

  // The field immediately before which the subject field should be ordered. None means append.
  type Position[+A] = Option[A]
}
