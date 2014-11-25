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

  type CorrectionPart[I, C] = CorrectionPartS[Unit, I, C]
  type ValidationPart[C, V] = ValidationPartS[Unit, C, V]
  type Validator[I, C, V]   = ValidatorS[Unit, I, C, V]

  type ValidatePlusR[S, R, O] = R => ValidatePlusS[S, O]
  type ValidatePlusS[S, O] = (S, O) => Option[VFailure]
}
