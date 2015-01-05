package shipreq.webapp.client.delta

import shipreq.webapp.base.data._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.protocol._
import Partition._


sealed trait ApplicationResult
case class Applied(p: Project, d: LocalDelta) extends ApplicationResult
case object CouldntApply                      extends ApplicationResult


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
          case t@ CustomIssueTypes => x(t, CustomIssueTypeProtocol.partitionFns)
          case t@ CustomReqTypes   => x(t, CustomReqTypeProtocol  .partitionFns)
          case t@ Fields           => x(t, FieldProtocol          .PartitionFns)
          case t@ Tags             => x(t, TagProtocol            .PartitionFns)
        }
      }

      acc match {
        case a@ Applied(_, _) => y(a)
        case CouldntApply => CouldntApply
      }
    })
}

