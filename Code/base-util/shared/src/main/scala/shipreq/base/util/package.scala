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

  /**
   * A map where keys are polymorphic and the value type depends on the key type.
   *
   * You are trusted to ensure you don't overlap keys.
   *
   * @tparam A The super class of all key types.
   */
  type PolyMap[A] = Map[A, Any]
}
