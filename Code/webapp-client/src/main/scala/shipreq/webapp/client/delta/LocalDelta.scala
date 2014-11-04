package shipreq.webapp.client.delta

import scala.annotation.tailrec
import shipreq.webapp.base.data.delta.Partition

case class LocalDeltaP[P <: Partition](del: Set[P#Id],
                                       upd: List[P#Data]) {

  def deltaG(p: P): LocalDeltaG = new LocalDeltaR(p, this)
}

private[delta] class LocalDeltaR[_P <: Partition](_p: _P, d: LocalDeltaP[_P]) extends LocalDeltaG {
  override type P = _P
  override def p = _p
  override def deltaP = d
}

trait LocalDeltaG {
  type P <: Partition
  def p: P
  def deltaP: LocalDeltaP[P]
}

// Space:
//   UpdateSet  ∈ O(n)
//   UpdateSetG ∈ O(1)
//   UpdateSets ∈ O(2m + n)
object LocalDelta {

  @tailrec
  def filter[P <: Partition](p: P, s: LocalDelta): LocalDeltaP[P] = s match {
    case h :: t =>
      Partition.testEq(h.p, p) match {
        case Some(ev) => ev.subst(h.deltaP)
        case None     => filter(p, t)
      }
    case Nil => LocalDeltaP[P](Set.empty, Nil)
  }
}