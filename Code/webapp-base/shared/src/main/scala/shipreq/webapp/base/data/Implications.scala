package shipreq.webapp.base.data

import japgolly.nyaya.CycleDetector
import japgolly.nyaya.util.Multimap
import monocle.macros.Lenses
import shipreq.base.util.UnivEq
import shipreq.webapp.base.util.TransitiveClosure
import shipreq.webapp.base.util.TypeclassDerivation._
import Implications.Uni

object Implications {

  /** Unidirectional */
  type Uni = Multimap[ReqId, Set, ReqId]

  def cycleDetector =
    CycleDetector.Directed.multimap[Set, ReqId, Int](_.value, UnivEq.emptySet)

  def transitiveClosure(keys: Iterable[ReqId], dead: Set[ReqId], uni: Uni): TransitiveClosure[ReqId] =
    TransitiveClosure.auto[ReqId](keys)(uni.apply, !dead.contains(_))

  def emptyUni: Uni = Multimap.empty
  def empty = Implications(emptyUni)

  implicit def equality: UnivEq[Implications] = deriveUnivEq
}

/**
 * Bi-directional implications between requirements.
 */
@Lenses
case class Implications(srcToTgt: Uni) {
  lazy val tgtToSrc: Uni = srcToTgt.reverse

  def members: Set[ReqId] =
    srcToTgt.m.toStream.foldLeft(UnivEq.emptySet[ReqId]) {
      case (q, (k, vs)) => q + k ++ vs
    }
}

