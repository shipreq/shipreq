package shipreq.webapp.feature.validation

import scalaz.{Validation, Success}
import shipreq.webapp.lib.Types._

trait CorrectionPart[-I, +C <: AnyRef] {
  final type CI = C @@ InputCorrected
  def correct(input: I): CI
}

trait ValidationPart[-C <: AnyRef, +V] {
  def validate(input: C @@ InputCorrected): ValidationResult[V]
}

trait Validator[-I, C <: AnyRef, +V] extends CorrectionPart[I, C] with ValidationPart[C, V] {
  final def correctAndValidate(input: I): ValidationResult[V] =
    validate(correct(input))

  final def isValid(input: CI): Boolean =
    validate(input).isSuccess

  def map[V2](f: V => V2): Validator[I, C, V2] =
    new Validator.Mapped(this, f)
}

object Validator {

  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  class Mapped[I, C <: AnyRef, V, V2](base: Validator[I, C, V], f: V => V2) extends Validator[I, C, V2] {
    override def correct(i: I): CI = base.correct(i)
    override def validate(c: C @@ InputCorrected): ValidationResult[V2] = base.validate(c).map(f)
    override def map[V3](g: V2 => V3): Validator[I, C, V3] =
      new Validator.Mapped(base, g compose f)
  }

  abstract class UseConstraintValidator[T <: AnyRef](validator: ConstraintValidator[T])
    extends Validator[T, T, T @@ Validated] {
    final override def validate(input: T @@ InputCorrected) = validator.validate(input)
  }

  abstract class Typical[T <: AnyRef](c: T => T @@ InputCorrected, validator: ConstraintValidator[T])
    extends UseConstraintValidator(validator) {
    final override def correct(input: T) = c(input)
  }

  trait NoInputCorrection[I <: AnyRef] extends CorrectionPart[I, I] {
    final override def correct(input: I) = input.tag[InputCorrected]
  }

  trait NoValidation[I <: AnyRef, O <: AnyRef] extends ValidationPart[I, O @@ Validated] {
    final override def validate(input: I @@ InputCorrected) = Success(input.tag)
  }

  trait ValidatorT[I, C <: AnyRef, V <: AnyRef] extends Validator[I, C, V @@ Validated]

  trait ValidatorT3[T <: AnyRef] extends ValidatorT[T, T, T]

}

