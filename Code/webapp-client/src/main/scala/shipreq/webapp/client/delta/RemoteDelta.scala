package shipreq.webapp.client.delta

import scalaz.{-\/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.TagProtocol.PovRelations
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.delta._
import shipreq.webapp.base.data.DataImplicits._
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
          case t@ CustomIncmpTypes => x(t, GenericPartitionFns(t))
          case t@ CustomReqTypes   => x(t, GenericPartitionFns(t))
          case t@ Tags             => x(t, TagPartitionFns)
        }
      }

      acc match {
        case a@ Applied(_, _) => y(a)
        case CouldntApply => CouldntApply
      }
    })
}

object GenericPartitionFns {
  def apply(q: Partition)(implicit da: DataSetAccessor[q.Data]) =
    new GenericPartitionFns[q.type, q.Data](q)(da)
}

class GenericPartitionFns[P <: Partition {type Data = D}, D](q: P)(implicit da: DataSetAccessor[D]) extends Fns[P] {
  import q.di

  def rev(p: Project): Rev = da.getRev(p)

  def update(p: Project, rev: Rev, ds: RemoteDeltaP[P]): Project = {
    var vs = da.getData(p)

    val dels = ds.del
    if (dels.nonEmpty)
      vs = vs.filterNot(data => dels contains data.id)

    val upds = ds.upd.map(data => data.id -> data).toMap
    if (upds.nonEmpty)
      vs = vs.map(p => upds.getOrElse(p.id, p))

    da.set(p, rev, vs)
  }
}

object TagPartitionFns extends Fns[Tags.type] {
  type T = Tags.type

  def rev(p: Project): Rev =
    p.tags.rev

  def update(p: Project, rev: Rev, ds: RemoteDeltaP[T]): Project = {
    var t = p.tags.data

    // Delete tags
    for (id <- ds.del)
      t = t.mapUnderlying(_.mapValues(_ removeChild id) - id)

    // Insert/update
    // (Separate phases ∵ all ids must exist before updating structure)
    t = t.addAll(ds.upd.map(u => TagInTree(u.tag, Vector.empty)): _*)
    t = PovRelations.trustedApplyN(ds.upd.map(_.tmap2(_.id, _.rels)), t)

    // Done
    p.copy(tags = RevAnd(rev, t))
  }
}