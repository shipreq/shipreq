package shipreq.webapp.shared.validation

import scalaz.{Failure, Success, Endo}

class ValidatorPlus[I, C, V](val liveCorrect: I => I, cp: CorrectionPart[I, C], vp: ValidationPart[C, V])
  extends Validator[I, C, V](cp, vp) {

  override def map[V2](f: V => V2): ValidatorPlus[I, C, V2] =
    new ValidatorPlus(liveCorrect, cp, vp map f)

  def stateful[S](t: ValidatePlusS[S, V]): S => ValidatorPlus[I, C, V] =
    s => {
      val vp2 = ValidationPart[C, V](c =>
        validate(c) match {
          case f@ Failure(_) => f
          case r@ Success(v) => t(s, v).fold(r: ValidationResult[V])(Failure(_))
        })
      new ValidatorPlus[I, C, V](liveCorrect, cp, vp2)
    }
}

object ValidatorPlus {

  def apply[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V], liveCorrect: Endo[I]): ValidatorPlus[I, C, V] =
    new ValidatorPlus(liveCorrect.run, cp, vp)

  def apply[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V], liveCorrect: I => I): ValidatorPlus[I, C, V] =
    new ValidatorPlus(liveCorrect, cp, vp)

  def apply[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V]): ValidatorPlus[I, C, V] =
    new ValidatorPlus(identity[I], cp, vp)

  def nop[A]: ValidatorPlus[A, A, A] =
    apply(CorrectionPart.nop[A], ValidationPart.nop[A], Endo.idEndo[A])

  def nop[I, V](f: I => V): ValidatorPlus[I, I, V] =
    apply(CorrectionPart.nop[I], ValidationPart.nop(f), Endo.idEndo[I])

  object Implicits {
    implicit class ValidatorExt[I, C, V](val v: Validator[I, C, V]) extends AnyVal {

      def toPlus: ValidatorPlus[I, C, V] =
        ValidatorPlus[I, C, V](v.cp, v.vp)

      def toPlus(liveCorrect: I => I): ValidatorPlus[I, C, V] =
        ValidatorPlus[I, C, V](v.cp, v.vp, liveCorrect)

      def toPlus(liveCorrect: Endo[I]): ValidatorPlus[I, C, V] =
        ValidatorPlus[I, C, V](v.cp, v.vp, liveCorrect)
    }
  }
}