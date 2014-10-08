package shipreq.webapp.shared.data.delta

final case class RemoteDeltaP[P <: Partition] private[delta](
    del: List[P#Id],
    upd: List[P#Instance]) {

  override def toString = s"Δᵖ($del, $upd)"

  def isEmpty = del.isEmpty || upd.isEmpty
  def nonEmpty = !isEmpty
}

// =====================================================================================================================

object RemoteDeltaG {
  def apply[P <: Partition](p: P, from: Rev, to: Rev)(del: List[P#Id], upd: List[P#Instance]) =
    new RemoteDeltaG(p, from, to, RemoteDeltaP(del, upd))
}

final class RemoteDeltaG private(
    val p: Partition,
    val from: Rev,
    val to: Rev,
    delta: RemoteDeltaP[_]) {

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