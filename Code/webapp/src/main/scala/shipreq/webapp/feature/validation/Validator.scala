package shipreq.webapp.feature.validation

import scalaz.{Endo, NonEmptyList, Failure, Validation, Success}
import shipreq.webapp.lib.Types._

final case class CorrectionPart[-I, C](correct: I => InputCorrected[C]) {
  def contramap[I0](f: I0 => I) = CorrectionPart[I0, C](correct compose f)
  def map[C2](f: C => InputCorrected[C2]) = CorrectionPart[I, C2](i => f(correct(i).value))
  def mapQ[C2](f: C => C2) = CorrectionPart[I, C2](i => InputCorrected(f(correct(i).value)))
}

object CorrectionPart {
  def lift[I, C](f: I => C) = CorrectionPart[I, C](i => InputCorrected(f(i)))
  def liftE[A](f: A => A) = lift(f)
  def endo[A](f: Endo[A]) = lift(f.run)
  def nop[A] = liftE[A](identity)
}

// =====================================================================================================================

final class ValidationPart[C, +V](val validate: InputCorrected[C] => ValidationResult[V]) {
  def contramap[C0](f: InputCorrected[C0] => InputCorrected[C]) =
    ValidationPart[C0, V](validate compose f)

  def map[V2](f: V => V2) = ValidationPart[C, V2](validate(_) map f)
}

object ValidationPart {

  def apply[C, V](f: InputCorrected[C] => ValidationResult[V]) =
    new ValidationPart(f)

  def liftO[C, V](f: InputCorrected[C] => ValidationResult[V]) =
    ValidationPart[Option[C], Option[V]](_.value match {
      case None    => Success(None)
      case Some(c) => f(InputCorrected(c)).map(s => Some(s))
    })

  def test[A](test: InputCorrected[A] => Boolean, fail: VFailure) = {
    val failure = Failure(fail)
    apply[A, A](a => if (test(a)) Success(a.value) else failure)
  }

  /**
   * @param fieldName The field name. Prepend to validation failure messages.
   */
  def forConstraint[A](fieldName: String, c: Constraint[A]) = ValidationPart[A, A](input => {
    val a: A = input.value
    c.invalidate(a) match {
      case Nil    => Success(a)
      case h :: t => Failure(VFailure.forField(fieldName, NonEmptyList.nel(h, t)))
    }
  })
}

// =====================================================================================================================

case class Validator[-I, C, +V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V]) {
  @inline final def correct = cp.correct
  @inline final def correctU = correct andThen (_.value)
  @inline final def validate = vp.validate

  def correctAndValidate(input: I): ValidationResult[V] =
    validate(correct(input))

  def isValid(input: InputCorrected[C]): Boolean =
    validate(input).isSuccess

  def map[V2](f: V => V2): Validator[I, C, V2] =
    Validator(cp, vp map f)

  def ***[I2, C2, V2](b: Validator[I2, C2, V2]): Validator[(I, I2), (C, C2), (V, V2)] =
    Validator(
      CorrectionPart[(I, I2), (C, C2)](i => InputCorrected(this correct i._1 value, b correct i._2 value)),
      ValidationPart[(C, C2), (V, V2)](i =>
        Validator.Ap.apply2(this validate InputCorrected(i.value._1), b validate InputCorrected(i.value._2))((x, y) => (x, y)))
    )
}

object Validator {

  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  def choose[I <: C, C, V](f: C => Validator[I, C, V]): Validator[I, C, V] =
    new Validator[I, C, V](
      CorrectionPart[I, C](i => f(i).correct(i)),
      ValidationPart[C, V](c => f(c.value).validate(c))) {
      override def correctAndValidate(i: I): ValidationResult[V] = f(i).correctAndValidate(i)
    }
}