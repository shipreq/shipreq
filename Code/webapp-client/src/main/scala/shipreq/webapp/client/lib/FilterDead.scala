package shipreq.webapp.client.lib

import scala.collection.GenTraversableLike
import shipreq.base.util.IsoBool
import shipreq.webapp.base.data.Alive

sealed trait FilterDead {
  val filter: Option[Alive => Boolean]

  def apply[A, C[x] <: GenTraversableLike[x, C[x]]](as: C[A])(f: => (A => Alive)): C[A] =
    filter.fold(as)(g => as.filter(g compose f))

  def filterFn: Alive => Boolean =
    filter.getOrElse(_ => true)

  def filterFnA[A](f: A => Alive): A => Boolean =
    filter.fold((_: A) => true)(_ compose f)
}

object FilterDead extends IsoBool.ObjOnly[FilterDead] {
  override protected def pos = ShowDead
  override protected def neg = HideDead
}

case object ShowDead extends FilterDead with IsoBool[FilterDead] {
  override protected def neg = HideDead
  override val filter: Option[Alive => Boolean] = None
}

case object HideDead extends FilterDead with IsoBool[FilterDead] {
  override protected def neg = ShowDead
  override val filter: Option[Alive => Boolean] = Some(Alive.equality.equal(Alive, _))
}
