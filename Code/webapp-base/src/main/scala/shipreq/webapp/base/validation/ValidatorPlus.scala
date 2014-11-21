package shipreq.webapp.base.validation

import scalaz.Isomorphism._
import scalaz._
import ValidatorPlus.Implicits._

class ValidatorPlus[I, C, V](val liveCorrect: I => I, cp: CorrectionPart[I, C], vp: ValidationPart[C, V])
  extends Validator[I, C, V](cp, vp) {

  override def contraimap[A](iso: A <=> I): ValidatorPlus[A, C, V] =
    super.contraimap(iso).toPlus(iso.from compose liveCorrect compose iso.to)

  override def contraxmap[A](g: I => A)(f: A => I): ValidatorPlus[A, C, V] =
    super.contraxmap(g)(f).toPlus(g compose liveCorrect compose f)

  override def map[W](f: V => W): ValidatorPlus[I, C, W] =
    new ValidatorPlus(liveCorrect, cp, vp map f)

  override def lift[M[_]](implicit M: Traverse[M]): ValidatorPlus[M[I], M[C], M[V]] =
    super.lift[M].toPlus(M.map(_)(liveCorrect))

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

  def apply[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V], liveCorrect: I => I): ValidatorPlus[I, C, V] =
    new ValidatorPlus(liveCorrect, cp, vp)

  def lift[I, C, V](v: Validator[I, C, V]): ValidatorPlus[I, C, V] =
    liftParts(v.cp, v.vp)

  def liftParts[I, C, V](cp: CorrectionPart[I, C], vp: ValidationPart[C, V]): ValidatorPlus[I, C, V] =
    new ValidatorPlus(identity[I], cp, vp)

  def nop[A]: ValidatorPlus[A, A, A] =
    apply(CorrectionPart.nop[A], ValidationPart.nop[A], identity[A])

  def nop[I, V](f: I => V): ValidatorPlus[I, I, V] =
    apply(CorrectionPart.nop[I], ValidationPart.nop(f), identity[I])

  object Implicits {

    implicit def endoToFn[A](e: Endo[A]): A => A = e.run

    implicit class ValidatorExt[I, C, V](val v: Validator[I, C, V]) extends AnyVal {
      def liftToPlus: ValidatorPlus[I, C, V] =
        ValidatorPlus.liftParts[I, C, V](v.cp, v.vp)

      def toPlus(liveCorrect: I => I): ValidatorPlus[I, C, V] =
        ValidatorPlus[I, C, V](v.cp, v.vp, liveCorrect)
    }
  }
}