package shipreq.webapp.base.delta

import japgolly.nyaya._
import shipreq.webapp.base.data.Rev

final case class RemoteDeltaP[P <: Partition] private[delta](
    del: Set[P#Id],
    upd: List[P#Data]) {

  override def toString = s"Δᵖ($del, $upd)"

  def isEmpty = del.isEmpty && upd.isEmpty
  def nonEmpty = !isEmpty
}

// =====================================================================================================================

object RemoteDeltaG {
  def apply(p: Partition, from: Rev, to: Rev)(del: Set[p.Id], upd: List[p.Data]) =
    new RemoteDeltaG(p, from, to, RemoteDeltaP(del, upd))

  lazy val prop =
    Prop.test[RemoteDeltaG]("from ≥ 0", _.from.value >= 0) ∧
    Prop.test[RemoteDeltaG]("from ≤ to", r => r.from.value <= r.to.value)
}

final class RemoteDeltaG private(
    val p: Partition,
    val from: Rev,
    val to: Rev,
    delta: RemoteDeltaP[_]) {

  this assertSatisfies RemoteDeltaG.prop

  override def hashCode = p.hashCode + delta.hashCode + from.hashCode + to.hashCode
  override def equals(o: Any): Boolean = o match {
    case b: RemoteDeltaG => p==b.p && b.forceDeltaP(p)==delta && from==b.from && to==b.to
    case _ => false
  }

  override def toString = s"Δᵍ($p [${from.value}..${to.value}] $delta)"

  def isEmpty = delta.isEmpty
  def nonEmpty = !isEmpty

  def forceDeltaP[P <: Partition](p: P) = delta.asInstanceOf[RemoteDeltaP[P]]

  def applicableToRev(tgt: Rev): Applicability =
    if (tgt.value < from.value - 1)
      Unapplicable
    else if (tgt.value >= to.value)
      NoNeed
    else
      Applies
}

// =====================================================================================================================

sealed trait Applicability
case object Applies extends Applicability
case object NoNeed extends Applicability
case object Unapplicable extends Applicability