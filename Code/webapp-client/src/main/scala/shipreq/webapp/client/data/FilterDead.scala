package shipreq.webapp.client.data

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import scala.collection.GenTraversableLike
import shipreq.base.util.IsoBool
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{LDStats, LDStat, Live}

sealed trait FilterDead extends IsoBool[FilterDead] {
  override final def companion = FilterDead

  val filter: Option[Live => Boolean]

  final def apply[A, C[x] <: GenTraversableLike[x, C[x]]](as: C[A])(f: => (A => Live)): C[A] =
    filter.fold(as)(g => as.filter(g compose f))

  final def filterFn: Live => Boolean =
    filter.getOrElse(_ => true)

  final def filterFnA[A](f: A => Live): A => Boolean =
    filter.fold((_: A) => true)(_ compose f)

  def ldStatAccessor[A]: LDStat[A] => A

  final def ldStatsAccessor[K, A](stats: LDStats[K, A]): K => A = {
    val get = ldStatAccessor[A]
    k => get(stats(k))
  }
}

object FilterDead extends IsoBool.Object[FilterDead] {
  override def positive = HideDead
  override def negative = ShowDead
  implicit val reusability = Reusability.byEqual[FilterDead]
}

case object HideDead extends FilterDead {
  override val filter: Option[Live => Boolean] = Some(_ ==* Live)
  override def ldStatAccessor[A] = _.live
}

case object ShowDead extends FilterDead {
  override val filter: Option[Live => Boolean] = None
  override def ldStatAccessor[A] = _.all
}
