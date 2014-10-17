package shipreq.webapp.client.delta

import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import shipreq.webapp.shared.data.DataImplicits._
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
          case t@ CustomIncmpTypes => x(t, new GenericPartitionFns(t))
          case t@ CustomReqTypes   => x(t, new GenericPartitionFns(t))
        }
      }
      
      acc match {
        case a@ Applied(_, _) => y(a)
        case CouldntApply => CouldntApply
      }
    })
}

class GenericPartitionFns[T <: Partition](tt: T)(implicit ia: IdAccessor[T#DI], da: DataSetAccessor[T#DI]) extends Fns[T] {

  def rev(p: Project): Rev = da.getRev(p)

  def update(p: Project, rev: Rev, ds: RemoteDeltaP[T]): Project = {

    var vs = da.getData(p)

    val dels = ds.del.toSet
    if (dels.nonEmpty)
      vs = vs.filterNot(data => dels contains data.id)

    val upds = ds.upd.map(p => p.id -> p).toMap
    if (upds.nonEmpty)
      vs = vs.map(p => upds.getOrElse(p.id, p))

    da.set(p, rev, vs)
  }
}
