package shipreq.webapp.feature.validation

import scalaz.{Endo, NonEmptyList, Failure, Validation, Success}
import shipreq.webapp.lib.Types._

final case class CorrectionPart[-I, +C <: AnyRef](correct: I => C @@ InputCorrected) {
  def contramap[I0](f: I0 => I) = CorrectionPart[I0, C](correct compose f)
  def map[C2 <: AnyRef](f: C => C2 @@ InputCorrected) = CorrectionPart[I, C2](f compose correct)
}

object CorrectionPart {
  def lift[A <: AnyRef](f: A => A) = CorrectionPart[A, A](f(_).tag[InputCorrected])
  def endo[A <: AnyRef](f: Endo[A]) = lift(f.run)
  def nop[A <: AnyRef] = lift[A](identity)
}

// =====================================================================================================================

final class ValidationPart[-C <: AnyRef, +V](val validate: C @@ InputCorrected => ValidationResult[V]) {
  def contramap[C0 <: AnyRef](f: C0 @@ InputCorrected => C @@ InputCorrected) =
    ValidationPart.untyped[C0, V](validate compose f)

  def map[V2](f: V => V2) = ValidationPart.untyped[C, V2](validate(_) map f)
}

object ValidationPart {

  def apply[C <: AnyRef, V <: AnyRef](f: C @@ InputCorrected => ValidationResult[V @@ Validated]) =
    new ValidationPart(f)

  def untyped[C <: AnyRef, V](f: C @@ InputCorrected => ValidationResult[V]) =
    new ValidationPart(f)

  def liftO[C <: AnyRef, V](f: C @@ InputCorrected => ValidationResult[V]) =
    new ValidationPart[Option[C], Option[V] @@ Validated]((_: Option[C]) match {
      case None    => Success((None: Option[V]).tag)
      case Some(c) => f(c.tag).map(s => Some(s).tag)
    })

  def test[A <: AnyRef](test: A @@ InputCorrected => Boolean, fail: VFailure) = {
    val failure = Failure(fail)
    apply[A, A](a => if (test(a)) Success(a.tag) else failure)
  }

  /**
   * @param fieldName The field name. Prepend to validation failure messages.
   */
  def forConstraint[A <: AnyRef](fieldName: String, c: Constraint[A]) = ValidationPart[A, A](input => {
    val a: A = input
    c.invalidate(a) match {
      case Nil    => Success(a.tag)
      case h :: t => Failure(VFailure.forField(fieldName, NonEmptyList.nel(h, t)))
    }
  })
}

// =====================================================================================================================

case class Validator[-I, C <: AnyRef, +V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V]) {
  @inline final def correct = cp.correct
  @inline final def validate = vp.validate

  def correctAndValidate(input: I): ValidationResult[V] =
    validate(correct(input))

  def isValid(input: C @@ InputCorrected): Boolean =
    validate(input).isSuccess

  def map[V2](f: V => V2): Validator[I, C, V2] =
    Validator(cp, vp map f)

  def ***[I2, C2 <: AnyRef, V2](b: Validator[I2, C2, V2]): Validator[(I, I2), (C, C2), (V, V2) @@ Validated] =
    Validator(
      CorrectionPart[(I, I2), (C, C2)](i => (this correct i._1, b correct i._2).tag),
      ValidationPart[(C, C2), (V, V2)](i =>
        Validator.Ap.apply2(this validate i._1.tag, b validate i._2.tag)((x, y) => (x, y).tag))
    )
}

object Validator {

  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  def choose[I <: C, C <: AnyRef, V](f: C => Validator[I, C, V]): Validator[I, C, V] =
    new Validator[I, C, V](
      CorrectionPart[I, C](i => f(i).correct(i)),
      ValidationPart.untyped[C, V](c => f(c).validate(c))) {
      override def correctAndValidate(i: I): ValidationResult[V] = f(i).correctAndValidate(i)
    }
}