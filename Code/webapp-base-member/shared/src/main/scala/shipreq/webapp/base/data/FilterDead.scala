package shipreq.webapp.base.data

import scala.collection.Factory
import shipreq.base.util.{IsoBool, OptionalBoolFn}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.derivation._

sealed abstract class FilterDead(final val filterFn: OptionalBoolFn[Live]) extends IsoBool[FilterDead] {
  override final def companion = FilterDead

  final def apply[A, C[x] <: Iterable[x]](as: C[A])(f: => (A => Live))(implicit fac: Factory[A, C[A]]): C[A] =
    filterFn.value.fold(as) { g =>
      val ff = f
      val gf = g compose ff
      as.iterator.filter(gf).to(fac)
    }

  final def apply[A](as: Iterator[A])(f: => (A => Live)): Iterator[A] =
    filterFn.value.fold(as)(g => as.filter(g compose f))

  final val filter: Live => Boolean =
    filterFn.toFn

  final def filterFnBy[A](f: A => Live): A => Boolean =
    filterFn.value.fold((_: A) => true)(_ compose f)

  def ldStatAccessor[A]: LiveDeadStat[A] => A

  final def ldStatsAccessor[K, A](stats: LiveDeadStatMap[K, A]): K => A = {
    val get = ldStatAccessor[A]
    k => get(stats(k))
  }

  def overrideIfDead(live: => Live): Option[FilterDead]
  def overrideIfDeadOption(live: => Option[Live]): Option[FilterDead]
}

object FilterDead extends IsoBool.Object[FilterDead] {
  override def positive = HideDead
  override def negative = ShowDead
}

case object HideDead extends FilterDead(OptionalBoolFn(_ ==* Live)) {
  override def ldStatAccessor[A] = _.live
  override def overrideIfDead(live: => Live) = if (live is Dead) Some(ShowDead) else None
  override def overrideIfDeadOption(live: => Option[Live]) = if (live.exists(_ is Dead)) Some(ShowDead) else None
}

case object ShowDead extends FilterDead(OptionalBoolFn.empty) {
  override def ldStatAccessor[A] = _.all
  override def overrideIfDead(live: => Live) = None
  override def overrideIfDeadOption(live: => Option[Live]) = None
}
