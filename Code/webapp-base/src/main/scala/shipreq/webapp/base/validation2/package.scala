package shipreq.webapp.base

import shipreq.base.util.TaggedTypes.{TaggedTypeCtor, TaggedType}
import scalaz.Validation

package object validation2 {

  type ValidationResult[+A] = Validation[VFailure, A]
  object ValidationResult {
    def apply[A](value: A): ValidationResult[A] = Validation.success(value)
  }

  final case class InputCorrected[A](value: A) extends TaggedType {
    override type U = A
    def map[B](f: A => B) = InputCorrected[B](f(value))
  }
  implicit def InputCorrectedCtor[R] = TaggedTypeCtor[InputCorrected[R]](InputCorrected[R])
}
