package shipreq.webapp.base

import shipreq.base.util.TaggedTypes.{TaggedTypeCtor, TaggedType}
import scalaz.Validation

package object validation {

  type ValidationResult[+A] = Validation[VFailure, A]
  object ValidationResult {
    def apply[A](value: A): ValidationResult[A] = Validation.success(value)

    def option[A](value: Option[A], f: => VFailure): ValidationResult[A] =
      value.fold[ValidationResult[A]](Validation failure f)(Validation.success)

    @inline def test[A](cond: Boolean, a: => A, f: => VFailure): ValidationResult[A] =
      if (cond) Validation.success(a) else Validation.failure(f)
  }

  final case class InputCorrected[A](value: A) extends TaggedType {
    override type U = A
    def map[B](f: A => B) = InputCorrected[B](f(value))
  }
  implicit def InputCorrectedCtor[R] = TaggedTypeCtor[InputCorrected[R]](InputCorrected[R])

  type CorrectionPartU[I, C] = CorrectionPart[Unit, I, C]
  type ValidationPartU[C, V] = ValidationPart[Unit, C, V]
  type ValidatorU[I, C, V]   = Validator[Unit, I, C, V]
}
