package shipreq.base

import scalaz.\/

package object util {

  type ErrorOr[A] = Error \/ A

  /** Faster than Vector(a) */
  @inline def Vector1[A](a: A): Vector[A] = Vector.empty :+ a

  @inline implicit def univEqOps[A](a: A): UnivEq.Ops[A] =
    new UnivEq.Ops(a)

  /**
    * Relative Position.
    *
    * The field immediately before which the subject field should be ordered. `None` means append.
    */
  type RelPos[+A] = Option[A]
}
