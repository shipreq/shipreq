package shipreq.webapp.base.validation2

import scalaz.{Validation => _, _}
import scalaz.Isomorphism.<=>
import scalaz.syntax.functor._
import scalaz.syntax.traverse._
import scalaz.Validation.FlatMap._
import shipreq.base.util.ScalaExt._

final class CorrectionPart[S, I, C](val liveCorrect: I => I,
                                    val fullCorrect: (S, I) => InputCorrected[C],
                                    val ci         : C => I) {
  type _S = S
  type _I = I
  type _C = C

  def addLiveCorrect(f: I => I): CorrectionPart[S, I, C] =
    new CorrectionPart(f compose liveCorrect, fullCorrect, ci)

  @inline def correct(s: S, i: I) = fullCorrect(s, liveCorrect(i))
  @inline def correct_(i: I)(implicit ev: Unit =:= S) = correct((), i)

  @inline def liftS[X](implicit ev: Unit =:= S): CorrectionPart[X, I, C] =
    contramapS[X](_ => ())

  def contramapS[X](f: X => S): CorrectionPart[X, I, C] =
    new CorrectionPart(liveCorrect, (t, i) => correct(f(t), i), ci)

  def xmapI[X](g: I => X)(f: X => I): CorrectionPart[S, X, C] =
    new CorrectionPart(a => g(liveCorrect(f(a))), (s, a) => fullCorrect(s, f(a)), g compose ci)

  def xmapC[X](f: C => X)(g: X => C): CorrectionPart[S, I, X] =
    new CorrectionPart(liveCorrect, (s, i) => fullCorrect(s, i) map f, ci compose g)

  def lift[M[_]](implicit M: Functor[M]): CorrectionPart[S, M[I], M[C]] =
    new CorrectionPart(
      M.map(_)(liveCorrect),
      (s, m) => InputCorrected(M.map(m)(fullCorrect(s, _).value)),
      M.map(_)(ci))

  def imapI[X](iso: X <=> I): CorrectionPart[S, X, C] = xmapI(iso.from)(iso.to)
  def imapC[X](iso: C <=> X): CorrectionPart[S, I, X] = xmapC(iso.to)(iso.from)

  def ***[I2, C2](b: CorrectionPart[S, I2, C2]): CorrectionPart[S, (I,I2), (C,C2)] =
      new CorrectionPart[S, (I,I2), (C,C2)](
        i => (this liveCorrect i._1, b liveCorrect i._2),
        (s,i) => InputCorrected(correct(s, i._1).value, b.correct(s, i._2).value),
        c => (this ci c._1, b ci c._2))
}

object CorrectionPart {
  @inline def apply[I, C](f: I => C, ci: C => I): CorrectionPartU[I, C] =
    apply3(identity, f, ci)

  @inline def apply3[I, C](lc: I => I, f: I => C, ci: C => I): CorrectionPartU[I, C] =
    new CorrectionPartU(lc, (_, i) => InputCorrected(f(i)), ci)

  def lift [I, C](iso: I <=> C): CorrectionPartU[I, C] = apply(iso.to, iso.from)
  def liftE[A]   (f: A => A)   : CorrectionPartU[A, A] = apply(f, identity)
  def endo [A]   (f: Endo[A])  : CorrectionPartU[A, A] = apply(f.run, identity)
  def nop  [A]                 : CorrectionPartU[A, A] = liftE[A](identity)
}

// =====================================================================================================================

final class ValidationPart[S, C, V](val validate: (S, InputCorrected[C]) => ValidationResult[V]) {
  type _S = S
  type _C = C
  type _V = V

  @inline def validate_(c: InputCorrected[C])(implicit ev: Unit =:= S) = validate((), c)

  @inline def liftS[X](implicit ev: Unit =:= S): ValidationPart[X, C, V] =
    contramapS[X](_ => ())

  def contramapS[X](f: X => S): ValidationPart[X, C, V] =
    new ValidationPart((t, c) => validate(f(t), c))

  def contramap[X](f: X => C): ValidationPart[S, X, V] =
    contramap2(_ map f)

  def contramap2[X](f: InputCorrected[X] ⇒ InputCorrected[C]): ValidationPart[S, X, V] =
    new ValidationPart((s, b) => validate(s, f(b)))

  def map[X](f: V => X): ValidationPart[S, C, X] =
    new ValidationPart((s, c) => validate(s, c) map f)

  def lift[M[_]](implicit M: Traverse[M]): ValidationPart[S, M[C], M[V]] =
    new ValidationPart((s, m) => M.map(m.value)(c => validate(s, InputCorrected(c))).sequence[ValidationResult, V])

  def andThen[A](that: ValidationPart[S, V, A]): ValidationPart[S, C, A] = that compose this
  def compose[A](that: ValidationPart[S, A, C]): ValidationPart[S, A, V] =
    new ValidationPart((s,a) => that.validate(s, a).flatMap(c => this.validate(s, InputCorrected(c))))

  def ***[C2, V2](that: ValidationPart[S, C2, V2]): ValidationPart[S, (C,C2), (V,V2)] =
    new ValidationPart[S, (C,C2), (V,V2)]((s, i) => Validator.Ap.tuple2(
      this.validate(s, i.map(_._1)),
      that.validate(s, i.map(_._2))))

  def liftO: ValidationPart[S, Option[C], Option[V]] =
    new ValidationPart[S, Option[C], Option[V]]((s, ic) => ic.value match {
      case None    => Success(None)
      case Some(c) => validate(s, InputCorrected(c)).map(s => Some(s))
    })
}

object ValidationPart {

  @inline def apply[C, V](f: C => ValidationResult[V]): ValidationPartU[C, V] =
    new ValidationPart((_, c) => f(c.value))

  @inline def apply[S, C, V](f: (S, C) => ValidationResult[V]): ValidationPart[S, C, V] =
    new ValidationPart((s, c) => f(s, c.value))

//  def testU[A](test: InputCorrected[A] => Boolean, fail: VFailure): ValidationPartU[A, A] = {
//    val failure = Failure(fail)
//    apply[A, A](a => if (test(a)) Success(a.value) else failure)
//  }

  def test[S, A](test: (S, A) => Boolean, fail: VFailure): ValidationPart[S, A, A] = {
    val failure = Failure(fail)
    apply[S, A, A]((s, a) => if (test(s, a)) Success(a) else failure)
  }

  /**
   * @param fieldName The field name. Prepend to validation failure messages.
   */
  def forConstraint[A](fieldName: String, c: Constraint[A]): ValidationPartU[A, A] =
    ValidationPart[A, A](a =>
      c.invalidate(a) match {
        case Nil    => Success(a)
        case h :: t => Failure(VFailure.forField(fieldName, NonEmptyList.nel(h, t)))
      }
    )

  def nop[A]              : ValidationPartU[A, A] = ValidationPart(c => Success(c))
  def nop[C, V](f: C => V): ValidationPartU[C, V] = ValidationPart(c => Success(f(c)))
}

// =====================================================================================================================

// TODO Determine Validator properties/laws

class Validator[S, I, C, V](val cp: CorrectionPart[S, I, C], val vp: ValidationPart[S, C, V]) {
  final type _S = S
  final type _I = I
  final type _C = C
  final type _V = V
  final type VR = ValidationResult[V]

  @inline final def liveCorrect       (i: I)                      : I                   = cp.liveCorrect(i)
  @inline final def ci                (c: C)                      : I                   = cp.ci(c)
  @inline final def correct           (s: S, i: I)                : InputCorrected[C]   = cp.correct(s, i)
  @inline final def correctU          (s: S, i: I)                : C                   = correct(s, i).value
  @inline final def isValid           (s: S, i: I)                : Boolean             = correctAndValidate(s, i).isSuccess
  @inline final def isValidC          (s: S, c: InputCorrected[C]): Boolean             = validate(s, c).isSuccess
  @inline final def validate          (s: S, c: InputCorrected[C]): ValidationResult[V] = vp.validate(s, c)
                def correctAndValidate(s: S, i: I)                : ValidationResult[V] = validate(s, correct(s, i))

  @inline final def correct_           (i: I)                (implicit ev: Unit =:= S) = correct           ((), i)
  @inline final def correctU_          (i: I)                (implicit ev: Unit =:= S) = correctU          ((), i)
  @inline final def validate_          (c: InputCorrected[C])(implicit ev: Unit =:= S) = validate          ((), c)
  @inline final def correctAndValidate_(i: I)                (implicit ev: Unit =:= S) = correctAndValidate((), i)
  @inline final def isValid_           (i: I)                (implicit ev: Unit =:= S) = isValid           ((), i)

  def contramapS    [X]   (f: X => S)                 : Validator[X, I, C, V]          = Validator(cp contramapS f, vp contramapS f)
  def imapI         [X]   (iso: X <=> I)              : Validator[S, X, C, V]          = xmapI(iso.from)(iso.to)
  def xmapI         [X]   (g: I => X)(f: X => I)      : Validator[S, X, C, V]          = Validator(cp.xmapI(g)(f), vp)
  def xmapC         [X]   (g: C => X)(f: X => C)      : Validator[S, I, X, V]          = Validator(cp.xmapC(g)(f), vp contramap f)
  def map           [X]   (f: V => X)                 : Validator[S, I, C, X]          = Validator(cp, vp map f)
  def liftS         [X]   (implicit ev: Unit =:= S)   : Validator[X, I, C, V]          = contramapS[X](_ => ())
  def lift          [M[_]](implicit M: Traverse[M])   : Validator[S, M[I], M[C], M[V]] = Validator(cp.lift[M], vp.lift[M])
  def addLiveCorrect      (f: I => I)                 : Validator[S, I, C, V]          = Validator(cp addLiveCorrect f, vp)
  def addValidation [X]   (f: ValidationPart[S, V, X]): Validator[S, I, C, X]          = Validator(cp, vp andThen f)

  def ***[I2, C2, V2](b: Validator[S, I2, C2, V2]): Validator[S, (I,I2), (C,C2), (V,V2)] =
    new Validator(cp *** b.cp, vp *** b.vp)
}

object Validator {
  val Ap = scalaz.Validation.ValidationApplicative[VFailure](VFailure.semigroup)

  @inline def apply[S, I, C, V](cp: CorrectionPart[S, I, C], vp: ValidationPart[S, C, V]): Validator[S, I, C, V] =
    new Validator(cp, vp)

  def nop[A]: ValidatorU[A, A, A] =
    apply(CorrectionPart.nop[A], ValidationPart.nop[A])

  def choose[S, I <: C, C, V](f: C => Validator[S, I, C, V]): Validator[S, I, C, V] =
    new Validator[S, I, C, V](
        new CorrectionPart[S, I, C](identity, (s, i) => f(i).correct(s, i), c => f(c).ci(c)),
        new ValidationPart[S, C, V]((s, c) => f(c.value).validate(s, c))) {
      override def correctAndValidate(s: S, i: I): ValidationResult[V] = f(i).correctAndValidate(s, i)
    }
}