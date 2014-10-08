package shipreq.webapp.client.delta

import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.{data => D}, D.Project
import Partition._

sealed trait ApplicationResult
case class Applied(p: Project, d: LocalDelta) extends ApplicationResult
case object CouldntApply extends ApplicationResult

object RemoteDelta {

  def apply(p: Project, deltas: RemoteDelta): ApplicationResult =
    apply2(Applied(p, Nil), deltas)

  private def apply2(z: ApplicationResult, deltas: RemoteDelta): ApplicationResult =
    deltas.foldLeft(z)((acc, d) => {

      def y(a: Applied): ApplicationResult = {
        def x[P <: Partition](part: P, fns: Fns[P]): ApplicationResult =
          d.applicableToRev(fns.rev(a.p)) match {
            case Applies =>
              val ds = d.forceDeltaP(part)
              val p = fns.update(a.p, d.to, ds)
              val u = LocalDeltaP(ds.del, ds.upd).deltaG(part) :: a.d
              Applied(p, u)
            case NoNeed => a
            case Unapplicable => CouldntApply
          }

        d.p match {
          case t@ CustomReqTypes => x(t, CustomReqTypeFns)
        }
      }
      
      acc match {
        case a@ Applied(_, _) => y(a)
        case CouldntApply => CouldntApply
      }
    })
}

object CustomReqTypeFns extends Fns[CustomReqTypes.type] {

  override def rev(p: Project) =
    p.customReqTypes.rev

  override def update(p: Project, rev: Rev, ds: RemoteDeltaP[CustomReqTypes.type]) = {
    var vs = p.customReqTypes.data.toStream

    val dels = ds.del.toSet
    if (dels.nonEmpty)
      vs = vs.filterNot(p => dels contains p.id)

    val upds = ds.upd.map(p => p.id -> p).toMap
    if (upds.nonEmpty)
      vs = vs.map(p => upds.getOrElse(p.id, p))

    p.copy(customReqTypes = D.CustomReqTypes(rev, vs.toSeq))
  }
}
