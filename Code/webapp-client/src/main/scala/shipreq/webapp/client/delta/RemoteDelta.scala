package shipreq.webapp.client.delta

import shipreq.webapp.shared.data.{CustomReqTypes, Project}
import shipreq.webapp.shared.data.delta._
import Partition._

sealed trait ApplicationResult
case class Applied(p: Project, d: LocalDeltas) extends ApplicationResult
case object CouldntApply extends ApplicationResult

final class RemoteDelta(deltas: RemoteDeltas) {
  
  def apply(p: Project, deltas: RemoteDeltas): ApplicationResult =
    apply2(Applied(p, Nil), deltas)

  private def apply2(z: ApplicationResult, deltas: RemoteDeltas): ApplicationResult =
    deltas.foldLeft(z)((acc, d) => {

      def y(a: Applied): ApplicationResult = {
        def x[P <: Partition](q: P, b: Fns[P]): ApplicationResult =
          d.applicableToRev(b.rev(a.p)) match {
            case Applies =>
              val ds = forceEqProof[Partition, P].subst(d.updateSet)
              val p = b.update(a.p, d.toRev, ds)
              val u = LocalDeltaP(ds.del, ds.upd).deltaG(q) :: a.d
              Applied(p, u)
            case NoNeed => a
            case Unapplicable => CouldntApply
          }

        d.meta match {
          case t@ CustReqType => x(t, CustReqTypeFns)
        }
      }
      
      acc match {
        case a@ Applied(_, _) => y(a)
        case CouldntApply => CouldntApply
      }
    })
}

object CustReqTypeFns extends Fns[CustReqType.type] {

  override def rev(p: Project) =
    p.customReqTypes.rev

  override def update(p: Project, rev: Rev, ds: RemoteDeltaP[CustReqType.type]) = {
    var vs = p.customReqTypes.data.toStream

    val dels = ds.del.toSet
    if (dels.nonEmpty)
      vs = vs.filterNot(p => dels contains p.id)

    val upds = ds.upd.map(p => p.id -> p).toMap
    if (upds.nonEmpty)
      vs = vs.map(p => upds.getOrElse(p.id, p))

    p.copy(customReqTypes = CustomReqTypes(rev, vs.toSeq))
  }
}
