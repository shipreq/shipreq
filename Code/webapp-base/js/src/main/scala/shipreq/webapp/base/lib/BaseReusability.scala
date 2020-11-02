package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.base.util._

object BaseReusability extends BaseReusability {

  final class ReusabilityObjExt(private val r: Reusability.type) extends AnyVal {
    def byUnivEq[A: UnivEq]: Reusability[A] =
      Reusability.by_==[A]

    def byUnivEq[A, B: UnivEq](f: A => B): Reusability[A] =
      byUnivEq[B] contramap f

    def byRefOrUnivEq[A <: AnyRef : UnivEq]: Reusability[A] =
      Reusability.byRef[A] || byUnivEq[A]

    def byRefOrUnivEq[A <: AnyRef, B: UnivEq](f: A => B): Reusability[A] =
      Reusability.byRef[A] || byUnivEq(f)

    def mapSameOrEmpty[K, V]: Reusability[Map[K, V]] =
      Reusability.byRef || Reusability.when(_.isEmpty)
  }

}

abstract class BaseReusability {
  import BaseReusability._

  implicit def toReusabilityObjExt(r: Reusability.type): ReusabilityObjExt =
    new ReusabilityObjExt(r)

  private[this] def taggedIntReuse = Reusability.byUnivEq[TaggedInt]
  implicit def reusabilityTaggedInt[T <: TaggedInt]: Reusability[T] =
    taggedIntReuse.narrow

  implicit def reusabilityIsoBool[B <: IsoBool[B]: UnivEq]: Reusability[B] =
    Reusability.byUnivEq

  implicit def reusabilityIsoBoolValues[B <: IsoBool[B], A: Reusability]: Reusability[IsoBool.Values[B, A]] =
    Reusability((x, y) => (x.pos ~=~ y.pos) && (x.neg ~=~ y.neg))

  implicit def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.byRef || Reusability.by(_.whole)

  implicit def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.byRef || Reusability.by(_.whole)

  //implicit def reusabilityValidation[S, I, C, V]: Reusability[Validator[S, I, C, V]] =
  //  Reusability.byRef
}