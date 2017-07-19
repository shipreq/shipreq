package shipreq.webapp.base.lib

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react.extra._
import java.time.Instant
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.protocol.ServerSideProc

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

  implicit def reusabilityInstant: Reusability[Instant] =
    Reusability.by(_.toEpochMilli)

  private[this] def taggedIntReuse = Reusability.byUnivEq[TaggedInt]
  implicit def reusabilityTaggedInt[T <: TaggedInt]: Reusability[T] =
    taggedIntReuse.narrow

  implicit def reusabilityIsoBool[B <: IsoBool[B]: UnivEq]: Reusability[B] =
    Reusability.byUnivEq

  implicit def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.byRef || Reusability.by(_.whole)

  implicit def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.byRef || Reusability.by(_.whole)

  implicit def reusabilityServerSideProc[I, O]: Reusability[ServerSideProc[I, O]] =
    Reusability.by(_.key)

  //implicit def reusabilityValidation[S, I, C, V]: Reusability[Validator[S, I, C, V]] =
  //  Reusability.byRef


}