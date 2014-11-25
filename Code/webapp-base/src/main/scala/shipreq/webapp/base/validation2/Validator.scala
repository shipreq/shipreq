package shipreq.webapp.base.validation2

import scalaz.{Validation => _, _}
import scalaz.Isomorphism.<=>
import scalaz.syntax.functor._
import scalaz.syntax.traverse._
import scalaz.Validation.FlatMap._
import shipreq.base.util.ScalaExt._

final class CorrectionPartS[S, I, C](val correct: (S, I) => InputCorrected[C], val ci: C => I) {
  def contramapS[X](f: X => S): CorrectionPartS[X, I, C] =
    new CorrectionPartS((t, i) => correct(f(t), i), ci)

  def xmapI[X](g: I => X)(f: X => I): CorrectionPartS[S, X, C] =
    new CorrectionPartS((s, a) => correct(s, f(a)), g compose ci)

  def xmapC[X](f: C => X)(g: X => C): CorrectionPartS[S, I, X] =
    new CorrectionPartS((s, i) => correct(s, i) map f, ci compose g)

  def lift[M[_]](implicit M: Functor[M]): CorrectionPartS[S, M[I], M[C]] =
    new CorrectionPartS((s, m) => InputCorrected(M.map(m)(correct(s, _).value)), M.map(_)(ci))

  def imapI[X](iso: X <=> I): CorrectionPartS[S, X, C] = xmapI(iso.from)(iso.to)
  def imapC[X](iso: C <=> X): CorrectionPartS[S, I, X] = xmapC(iso.to)(iso.from)

  def ***[I2, C2](b: CorrectionPartS[S, I2, C2]): CorrectionPartS[S, (I,I2), (C,C2)] =
      new CorrectionPartS[S, (I,I2), (C,C2)](
        (s,i) => InputCorrected(correct(s, i._1).value, b.correct(s, i._2).value),
        c => (this ci c._1, b ci c._2))
}

object CorrectionPart {
  @inline def apply[I, C](f: I => C)(ci: C => I): CorrectionPart[I, C] =
    new CorrectionPart((_, i) => InputCorrected(f(i)), ci)

  def lift [I, C](iso: I <=> C): CorrectionPart[I, C] = apply(iso.to)(iso.from)
  def liftE[A]   (f: A => A)   : CorrectionPart[A, A] = apply(f)(identity)
  def endo [A]   (f: Endo[A])  : CorrectionPart[A, A] = apply(f.run)(identity)
  def nop  [A]                 : CorrectionPart[A, A] = liftE[A](identity)
}

// =====================================================================================================================

final class ValidationPartS[S, C, V](val validate: (S, InputCorrected[C]) => ValidationResult[V]) {
  def contramapS[X](f: X => S): ValidationPartS[X, C, V] =
    new ValidationPartS((t, c) => validate(f(t), c))

  def contramap[X](f: X => C): ValidationPartS[S, X, V] =
    contramap2(_ map f)

  def contramap2[X](f: InputCorrected[X] ⇒ InputCorrected[C]): ValidationPartS[S, X, V] =
    new ValidationPartS((s, b) => validate(s, f(b)))

  def map[X](f: V => X): ValidationPartS[S, C, X] =
    new ValidationPartS((s, c) => validate(s, c) map f)

  def lift[M[_]](implicit M: Traverse[M]): ValidationPartS[S, M[C], M[V]] =
    new ValidationPartS((s, m) => M.map(m.value)(c => validate(s, InputCorrected(c))).sequence[ValidationResult, V])

  def andThen[A](that: ValidationPartS[S, V, A]): ValidationPartS[S, C, A] = that compose this
  def compose[A](that: ValidationPartS[S, A, C]): ValidationPartS[S, A, V] =
    new ValidationPartS((s,a) => that.validate(s, a).flatMap(c => this.validate(s, InputCorrected(c))))

  def ***[C2, V2](that: ValidationPartS[S, C2, V2]): ValidationPartS[S, (C,C2), (V,V2)] =
    new ValidationPartS[S, (C,C2), (V,V2)]((s, i) => Validator.Ap.tuple2(
      this.validate(s, i.map(_._1)),
      that.validate(s, i.map(_._2))))
}

object ValidationPart {

  @inline def apply[C, V](f: InputCorrected[C] => ValidationResult[V]): ValidationPart[C, V] =
    new ValidationPart((_, c) => f(c))

  def liftO[C, V](f: InputCorrected[C] => ValidationResult[V]): ValidationPart[Option[C], Option[V]] =
    ValidationPart[Option[C], Option[V]](_.value match {
      case None    => Success(None)
      case Some(c) => f(InputCorrected(c)).map(s => Some(s))
    })

  def test[A](test: InputCorrected[A] => Boolean, fail: VFailure): ValidationPart[A, A] = {
    val failure = Failure(fail)
    apply[A, A](a => if (test(a)) Success(a.value) else failure)
  }

  /**
   * @param fieldName The field name. Prepend to validation failure messages.
   */
  def forConstraint[A](fieldName: String, c: Constraint[A]): ValidationPart[A, A] =
    ValidationPart[A, A](input => {
      val a = input.value
      c.invalidate(a) match {
        case Nil    => Success(a)
        case h :: t => Failure(VFailure.forField(fieldName, NonEmptyList.nel(h, t)))
      }
    })

  def nop[A]              : ValidationPart[A, A] = ValidationPart(c => Success(c.value))
  def nop[C, V](f: C => V): ValidationPart[C, V] = ValidationPart(c => Success(f(c.value)))
}

// =====================================================================================================================

// TODO Determine Validator properties/laws

class ValidatorS[S, I, C, V](val cp: CorrectionPartS[S, I, C], val vp: ValidationPartS[S, C, V]) {
  @inline final def ci(c: C): I = cp.ci(c)
  @inline final def correct = cp.correct
  @inline final def correctU = correct andThenA (_.value)
  @inline final def validate = vp.validate

  def correctAndValidate(s: S, i: I): ValidationResult[V] =
    validate(s, correct(s, i))

  def isValid (s: S, i: I)                : Boolean = correctAndValidate(s, i).isSuccess
  def isValidC(s: S, c: InputCorrected[C]): Boolean = validate(s, c).isSuccess

  def contramapS[X](f: X => S)           : ValidatorS[X, I, C, V]          = Validator(cp contramapS f, vp contramapS f)
  def imapI     [X](iso: X <=> I)        : ValidatorS[S, X, C, V]          = xmapI(iso.from)(iso.to)
  def xmapI     [X](g: I => X)(f: X => I): ValidatorS[S, X, C, V]          = Validator(cp.xmapI(g)(f), vp)
  def xmapC     [X](g: C => X)(f: X => C): ValidatorS[S, I, X, V]          = Validator(cp.xmapC(g)(f), vp contramap f)
  def map       [X](f: V => X)           : ValidatorS[S, I, C, X]          = Validator(cp, vp map f)
  def lift[M[_]](implicit M: Traverse[M]): ValidatorS[S, M[I], M[C], M[V]] = Validator(cp.lift[M], vp.lift[M])

  def ***[I2, C2, V2](b: ValidatorS[S, I2, C2, V2]): ValidatorS[S, (I,I2), (C,C2), (V,V2)] =
    new ValidatorS(cp *** b.cp, vp *** b.vp)

  def addValidation[X](f: ValidationPartS[S, V, X]): ValidatorS[S, I, C, X] =
    new ValidatorS(cp, vp andThen f)
}

object Validator {
  val Ap = scalaz.Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  @inline def apply[S, I, C, V](cp: CorrectionPartS[S, I, C], vp: ValidationPartS[S, C, V]): ValidatorS[S, I, C, V] =
    new ValidatorS(cp, vp)

  def nop[A]: Validator[A, A, A] =
    apply(CorrectionPart.nop[A], ValidationPart.nop[A])

  def choose[S, I <: C, C, V](f: C => ValidatorS[S, I, C, V]): ValidatorS[S, I, C, V] =
    new ValidatorS[S, I, C, V](
        new CorrectionPartS[S, I, C]((s, i) => f(i).correct(s, i), c => f(c).ci(c)),
        new ValidationPartS[S, C, V]((s, c) => f(c.value).validate(s, c))) {
      override def correctAndValidate(s: S, i: I): ValidationResult[V] = f(i).correctAndValidate(s, i)
    }
}
