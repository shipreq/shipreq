package shipreq.webapp.client.delta

import monocle.Lens
import scalaz.Leibniz.===
import shipreq.base.util.IMap
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.delta._
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
          case t@ CustomIssueTypes => x(t, GenericPartitionFns(t, Project._customIssueTypes))
          case t@ CustomReqTypes   => x(t, GenericPartitionFns(t, Project._customReqTypes))
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
  def apply[Id, D](q: Partition, l: Lens[Project, RevAnd[IMap[Id, D]]])
                  (implicit evD: q.Data === D, evI: q.Id === Id): Fns[q.type] =
    new Fns[q.type] {

      def rev(p: Project): Rev = l.get(p).rev

      def update(p: Project, rev: Rev, ds: RemoteDeltaP[q.type]): Project = {
        var m = l.get(p).data

        // Deletions
        m --= evI.subst(ds.del)

        // Updates
        m = m.addAll(evD.subst(ds.upd): _*)

        l.set(RevAnd(rev, m))(p)
      }
    }
}

object TagPartitionFns extends Fns[Tags.type] {
  import shipreq.webapp.base.protocol.TagProtocol._

  def rev(p: Project): Rev =
    p.tags.rev

  def update(p: Project, rev: Rev, ds: RemoteDeltaP[Tags.type]): Project = {
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