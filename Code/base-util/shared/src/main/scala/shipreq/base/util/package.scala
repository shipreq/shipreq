package shipreq.base

import scalaz.\/

package object util {

  type ErrorOr[A] = Error \/ A

  type ?=>[A, B] = FnWithFallback[A, B]

  /** Faster than Vector(a) */
  @inline def Vector1[A](a: A): Vector[A] = Vector.empty :+ a

  /**
    * Relative Position.
    *
    * The field immediately before which the subject field should be ordered. `None` means append.
    */
  type RelPos[+A] = Option[A]
}
