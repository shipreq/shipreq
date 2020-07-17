package shipreq.base

package object util {

  type ?=>[A, B] = FnWithFallback[A, B]

  type IfApplicable[+A] = NotApplicable.type \/ A

  /** Faster than Vector(a) */
  @inline def Vector1[A](a: A): Vector[A] = Vector.empty :+ a

  /**
    * Relative Position.
    *
    * The field immediately before which the subject field should be ordered. `None` means append.
    */
  type RelPos[+A] = Option[A]
}
