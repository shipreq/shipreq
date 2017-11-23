package shipreq.webapp.base.data.deletion

import japgolly.microlibs.nonempty._
import japgolly.univeq._
import scala.annotation.tailrec
import scala.collection.TraversableOnce
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText

object RestorationLogic {

  type ReqRow = DeletionLogic.ReqRow
  val  ReqRow = DeletionLogic.ReqRow

  type RestorableReqs = Vector[ReqRow]

  final case class Data(project       : Project,
                        restorableReqs: RestorableReqs,
                        initialReqs   : Set[ReqId]) {

    val optionalReqIds: Set[ReqId] =
      restorableReqs.iterator.filter(_.indent !=* 0).map(_.req.id).toSet
  }

  object Data {
    import MTrie.Ops

    /** Completely ignores code groups.
      * Clearer UX to separate Req deletion (with implication intelligence) from Code Group deletion.
      */
    def forReqs(project: Project, directlySelectedReqs: NonEmptySet[ReqId]): Data = {
      val restorableReqs = calcRestorableReqs(project, directlySelectedReqs)
      val initSelReqs    = calcInitiallySelectedReqs(project, restorableReqs, directlySelectedReqs).whole
      Data(project        = project,
           restorableReqs = restorableReqs,
           initialReqs    = initSelReqs)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Requirement logic

    private def calcRestorableReqs(p: Project, directSel: NonEmptySet[ReqId]): RestorableReqs = {
      val imps_> = p.content.implications.forwards
      val imps_< = p.content.implications.backwards

      val reqTypes = p.config.reqTypes

      val reqOrder = Ordering.by((_: Req).pubid)(reqTypes.pubidOrdering)

      def sortReqs(a: Array[Req]): Unit =
        java.util.Arrays.sort(a, reqOrder)

      val reqFilter: Req => Boolean =
        r => r.live(reqTypes).is(Dead)

      def lookupAll(reqIds: TraversableOnce[ReqId]): Array[Req] = {
        val a = reqIds.toIterator.map(p.content.reqs.need).filter(reqFilter).toArray
        sortReqs(a)
        a
      }

      var reqRows = Vector.newBuilder[ReqRow]

      def go(reqIds: TraversableOnce[ReqId], level: Int): Unit =
        for (r <- lookupAll(reqIds))
          if (r.allowLiveChange(reqTypes) is Allow) {

            // Add row
            val impBy = lookupAll(imps_<(r.id)).toVector
            reqRows += ReqRow(r, level, impBy)

            // Add implied reqs
            val kids = imps_>(r.id).iterator.filterNot(directSel.contains)
            go(kids, level + 1)
          }

      go(directSel.whole, 0)

      reqRows.result()
    }

    private def calcInitiallySelectedReqs(p: Project, restorableReqs: RestorableReqs, directSel: NonEmptySet[ReqId]): NonEmptySet[ReqId] = {

      // Means we don't know yet whether the deletion should be cascaded by default to this item
      class CascadePending(val req: Req, val imp: Vector[Req], var pending: Boolean)
      var cascadePending =
        restorableReqs.iterator
          .filter(_.indent != 0)
          .map(r => new CascadePending(r.req, r.impliedBy, true))
          .toList

      var select = directSel

      // Copy-paste with Backend#render
      def liveGivenState(r: Req): Live =
        Live.when(select contains r.id) //& r.live(p.config.reqTypes)

      // Decide which implied reqs to recommend cascading deletion
      // (I'm sure there's a smarter way but this will do)
      var changed = true
      while (changed) {
        changed = false

        for (t <- cascadePending if t.pending) {
          t.pending = false
          if (t.imp.forall(liveGivenState(_) is Live)) {
            changed = true
            select += t.req.id
          }
        }
      }

      select
    }

  }

}
