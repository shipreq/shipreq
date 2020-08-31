package shipreq.base

package object util {

  type ?=>[A, B] = FnWithFallback[A, B]

  type IfApplicable[+A] = NotApplicable.type \/ A

  /**
    * Relative Position.
    *
    * The field immediately before which the subject field should be ordered. `None` means append.
    */
  type RelPos[+A] = Option[A]
}
