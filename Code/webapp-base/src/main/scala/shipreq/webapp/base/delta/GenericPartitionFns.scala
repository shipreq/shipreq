package shipreq.webapp.base.delta

import monocle.Lens
import scalaz.Leibniz.===
import shipreq.base.util.IMap
import shipreq.webapp.base.data._
import shipreq.webapp.base.delta.Partition.Fns

object GenericPartitionFns {
  def apply[Id, D](q: Partition, l: Lens[Project, RevAnd[IMap[Id, D]]])
                  (implicit evD: q.Data === D, evI: q.Id === Id): Fns[q.type] =
    new Fns[q.type] {

      def rev(p: Project): Rev =
        l.get(p).rev

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
