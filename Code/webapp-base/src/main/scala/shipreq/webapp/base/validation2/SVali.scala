package shipreq.webapp.base.validation2

import shipreq.webapp.base.validation._
import scalaz.{Validation => _, _}
import scalaz.Isomorphism.<=>
import scalaz.syntax.semigroup._
import scalaz.syntax.functor._
import scalaz.syntax.traverse._
import Validator.Ap

final class CPS[S, I, C](val correct: (S, I) => InputCorrected[C], val ci: C => I) {
  def contramapS[X]   (f: X => S)             : CPS[X, I, C]       = new CPS((t,i) => correct(f(t), i), ci)
  def xmapI     [X]   (g: I => X)(f: X => I)  : CPS[S, X, C]       = new CPS((s,a) => correct(s, f(a)), g compose ci)
  def xmapC     [X]   (f: C => X)(g: X => C)  : CPS[S, I, X]       = new CPS((s,i) => correct(s, i) map f, ci compose g)
  def lift      [M[_]](implicit M: Functor[M]): CPS[S, M[I], M[C]] = new CPS((s,m) => InputCorrected(M.map(m)(correct(s, _).value)), M.map(_)(ci))
}

// =====================================================================================================================

final class VPS[S, C, V](val validate: (S, InputCorrected[C]) => ValidationResult[V]) {
  def contramapS[X]   (f: X => S)                                : VPS[X, C, V] = new VPS((t,c) => validate(f(t), c))
  def contramap [X]   (f: InputCorrected[X] => InputCorrected[C]): VPS[S, X, V] = new VPS((s,b) => validate(s, f(b)))
  def contramap2[X]   (f: X => C)                                : VPS[S, X, V] = contramap(_ map f)
  def map       [X]   (f: V => X)                                : VPS[S, C, X] = new VPS((s,c) => validate(s, c) map f)
  def lift      [M[_]](implicit M: Traverse[M])                  : VPS[S, M[C], M[V]] =
    new VPS((s, m) => M.map(m.value)(c => validate(s, InputCorrected(c))).sequence[ValidationResult, V])

  def andThen[A](that: VPS[S, V, A]): VPS[S, C, A] = that compose this
  def compose[A](that: VPS[S, A, C]): VPS[S, A, V] =
    new VPS((s,a) => that.validate(s, a).flatMap(c => this.validate(s, InputCorrected(c))))
}

// =====================================================================================================================

final class ValiS[S, I, C, V](val cp: CPS[S, I, C], val vp: VPS[S, C, V]) {
  @inline final def ci = cp.ci
  @inline final def correct = cp.correct
  @inline final def validate = vp.validate
  @inline final def correctU(s: S, i: I): C = correct(s, i).value

  def correctAndValidate(s: S, i: I): ValidationResult[V] =
    validate(s, correct(s, i))

  def isValid (s: S, i: I)                : Boolean = correctAndValidate(s, i).isSuccess
  def isValidC(s: S, c: InputCorrected[C]): Boolean = validate(s, c).isSuccess

  def contramapS[X](f: X => S)           : ValiS[X, I, C, V]          = new ValiS(cp contramapS f, vp contramapS f)
  def xmapI     [X](g: I => X)(f: X => I): ValiS[S, X, C, V]          = new ValiS(cp.xmapI(g)(f), vp)
  def xmapC     [X](g: C => X)(f: X => C): ValiS[S, I, X, V]          = new ValiS(cp.xmapC(g)(f), vp contramap2 f)
  def map       [X](f: V => X)           : ValiS[S, I, C, X]          = new ValiS(cp, vp map f)
  def lift[M[_]](implicit M: Traverse[M]): ValiS[S, M[I], M[C], M[V]] = new ValiS(cp.lift[M], vp.lift[M])

  //def andThenV[A](that: VPS[S, V, A]): VPS[S, C, A] = composeV(that composeV this
  def addValidation[X](f: VPS[S, V, X]): ValiS[S, I, C, X] =
    new ValiS(cp, vp andThen f)
}

object ValiS {
  implicit class CPartExt[I, C](val p: CorrectionPart[I, C]) extends AnyVal {
    def toCPS[S]: CPS[S, I, C] = new CPS((_, i) => p.correct(i), p.ci)
  }
  implicit class VPartExt[C, V](val p: ValidationPart[C, V]) extends AnyVal {
    def toVPS[S]: VPS[S, C, V] = new VPS((_, c) => p.validate(c))
  }
  implicit class ValiExt[I, C, V](val p: Validator[I, C, V]) extends AnyVal {
    def toValiS[S]: ValiS[S, I, C, V] = new ValiS(p.cp.toCPS, p.vp.toVPS)
  }
}