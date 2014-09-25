package shipreq.webapp.shared.validation

import scalaz.{Endo, NonEmptyList, Failure, Validation, Success}
import scalaz.Isomorphism.<=>

final class CorrectionPart[I, C](val correct: I => InputCorrected[C], val ci: C => I) {
  def contramap[A](iso: A <=> I): CorrectionPart[A, C] =
    contramap(iso.to)(iso.from)

  def contramap[A](f: A => I)(g: I => A): CorrectionPart[A, C] =
    new CorrectionPart(correct compose f, g compose ci)

  def map[D](iso: C <=> D): CorrectionPart[I, D] =
    map(iso.to)(iso.from)

  def map[D](f: C => D)(g: D => C): CorrectionPart[I, D] =
    new CorrectionPart(i => InputCorrected(f(correct(i).value)), ci compose g)
}

object CorrectionPart {
  def apply[I, C](f: I => C)(ci: C => I): CorrectionPart[I, C] =
    new CorrectionPart(i => InputCorrected(f(i)), ci)

  def lift[I, C](iso: I <=> C): CorrectionPart[I, C] =
    apply(iso.to)(iso.from)

  def liftE[A](f: A => A) =
    apply(f)(identity)

  def endo[A](f: Endo[A]) =
    apply(f.run)(identity)

  def nop[A] =
    liftE[A](identity)
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

  def nop[A]: ValidationPart[A, A] =
    ValidationPart(c => Success(c.value))

  def nop[C, V](f: C => V): ValidationPart[C, V] =
    ValidationPart(c => Success(f(c.value)))
}

// =====================================================================================================================

// TODO Determine Validator properties/laws

class Validator[I, C, +V](val cp: CorrectionPart[I, C], val vp: ValidationPart[C, V]) {
  @inline final def ci(c: C): I = cp.ci(c)
  @inline final def ci(c: InputCorrected[C]): I = cp.ci(c.value)
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
      new CorrectionPart[(I, I2), (C, C2)](
        i => InputCorrected(this correct i._1 value, b correct i._2 value),
        c => (this ci c._1, b ci c._2)),
      ValidationPart[(C, C2), (V, V2)](i =>
        Validator.Ap.apply2(this validate InputCorrected(i.value._1), b validate InputCorrected(i.value._2))((x, y) => (x, y)))
    )
}

object Validator {
  val Ap = Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  def apply[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V]): Validator[I, C, V] =
    new Validator(cp, vp)

  def nop[A] =
    apply[A, A, A](CorrectionPart.nop, ValidationPart.nop)

  def choose[I <: C, C, V](f: C => Validator[I, C, V]): Validator[I, C, V] =
    new Validator[I, C, V](
        new CorrectionPart[I, C](i => f(i).correct(i), c => f(c).ci(c)),
        ValidationPart[C, V](c => f(c.value).validate(c))) {
      override def correctAndValidate(i: I): ValidationResult[V] = f(i).correctAndValidate(i)
    }
}
