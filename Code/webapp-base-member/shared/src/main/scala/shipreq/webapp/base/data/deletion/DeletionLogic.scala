package shipreq.webapp.base.data.deletion

import japgolly.microlibs.nonempty._
import japgolly.univeq._
import scala.annotation.tailrec
import scala.collection.TraversableOnce
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText

object DeletionLogic {

  type DeletableReqs   = Vector[ReqRow]
  type DeletableGroups = Vector[GroupRow]

  final case class Data(project        : Project,
                        deletableReqs  : DeletableReqs,
                        deletableGroups: DeletableGroups,
                        initialReqs    : Set[ReqId],
                        initialGroups  : Set[ReqCodeId]) {

    val optionalReqIds: Set[ReqId] =
      deletableReqs.iterator.filter(_.indent !=* 0).map(_.req.id).toSet
  }

  final case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])

  final case class GroupRow(group    : LiveCodeGroup,
                            codeStr  : String,
                            subReqs  : Set[(ReqId, String)],
                            subGroups: Set[(ReqCodeId, String)]) {
    @inline def id = group.id

    def liveSubs(r: ReqId => Live, g: ReqCodeId => Live): Iterator[String] =
      subReqs  .iterator.filter(t => r(t._1) is Live).map(_._2) ++
      subGroups.iterator.filter(t => g(t._1) is Live).map(_._2)
  }

  object Data {
    import MTrie.Ops

    /** Completely ignores code groups.
      * Clearer UX to separate Req deletion (with implication intelligence) from Code Group deletion.
      */
    def forReqs(project: Project, directlySelectedReqs: NonEmptySet[ReqId]): Data = {
      val deletableReqs = calcDeletableReqs(project, directlySelectedReqs)
      val initSelReqs   = calcInitiallySelectedReqs(project, deletableReqs, directlySelectedReqs).whole
      Data(project         = project,
           deletableReqs   = deletableReqs,
           deletableGroups = Vector.empty,
           initialReqs     = initSelReqs,
           initialGroups   = Set.empty)
    }

    def forReqsAndCodeGroups__TEST_ONLY(project               : Project,
                                        directlySelectedReqs  : NonEmptySet[ReqId],
                                        directlySelectedGroups: Set[ReqCodeId]): Data = {
      val directlySelectedCodeGroups: Set[ReqCode.Value] =
        directlySelectedGroups.map(project.content.reqCodes.reqCode)

      val deletableReqs   = calcDeletableReqs(project, directlySelectedReqs)
      val deletableGroups = calcDeletableGroups(project, directlySelectedCodeGroups, deletableReqs)

      val initSelReqs   = calcInitiallySelectedReqs(project, deletableReqs, directlySelectedReqs).whole
      val initSelGroups = calcInitiallySelectedGroups(project, directlySelectedGroups, deletableGroups, initSelReqs)

      Data(project         = project,
           deletableReqs   = deletableReqs,
           deletableGroups = deletableGroups,
           initialReqs     = initSelReqs,
           initialGroups   = initSelGroups)
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Requirement logic

    private def calcDeletableReqs(p: Project, directSel: NonEmptySet[ReqId]): DeletableReqs = {
      val imps_> = p.content.implications.forwards
      val imps_< = p.content.implications.backwards

      val reqOrder = Ordering.by((_: Req).pubid)(p.config.reqTypes.pubidOrdering)

      def sortReqs(a: Array[Req]): Unit =
        java.util.Arrays.sort(a, reqOrder)

      val reqFilter: Req => Boolean =
        _.live(p.config.reqTypes) is Live

      def lookupAll(reqIds: TraversableOnce[ReqId]): Array[Req] = {
        val a = reqIds.toIterator.map(p.content.reqs.need).filter(reqFilter).toArray
        sortReqs(a)
        a
      }

      var reqRows = Vector.newBuilder[ReqRow]

      def go(reqIds: TraversableOnce[ReqId], level: Int): Unit =
        for (r <- lookupAll(reqIds)) {

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

    private def calcInitiallySelectedReqs(p: Project, deletableReqs: DeletableReqs, directSel: NonEmptySet[ReqId]): NonEmptySet[ReqId] = {

      // Means we don't know yet whether the deletion should be cascaded by default to this item
      class CascadePending(val req: Req, val imp: Vector[Req], var pending: Boolean)
      var cascadePending =
        deletableReqs.iterator
          .filter(_.indent != 0)
          .map(r => new CascadePending(r.req, r.impliedBy, true))
          .toList

      var select = directSel

      // Copy-paste with Backend#render
      def liveGivenState(r: Req): Live =
        Dead.when(select contains r.id) //& r.live(p.config.reqTypes)

      // Decide which implied reqs to recommend cascading deletion
      // (I'm sure there's a smarter way but this will do)
      var changed = true
      while (changed) {
        changed = false
        for (t <- cascadePending if t.pending) {
          t.pending = false
          if (t.imp.forall(liveGivenState(_) is Dead)) {
            changed = true
            select += t.req.id
          }
        }
      }

      select
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // ReqCode Group logic

    import ReqCode.{ActiveGroup, ActiveReq, Inactive, Trie, Value => Code}

    /**
     * Is a code useless, considering itself and its children.
     *
     * A useless group is a group with no live children or useless children groups.
     */
    private def isUselessLookingDown(t: Trie, c: Code): Boolean = {
      def isTrieUseless(s: Trie): Boolean =
        s.valuesIterator.forall(isNodeUseless)

      def isNodeUseless(node: Trie.Node): Boolean =
        node.fold(
          b => b.value.forall(n => isDataUseless(n.value)) && isTrieUseless(b.next),
          v => isDataUseless(v.value))

      def isDataUseless(d: ReqCode.Data): Boolean =
        d match {
          case _: ActiveReq => false
          case _: ActiveGroup | _: Inactive => true
        }

      t.getNode(c).forall(isNodeUseless)
    }

    private def codesOfActiveChildGroups(t: Trie, c: Code): Set[Code] =
      t.dropPath(c).flatStream
        .collect { case (code, _: ActiveGroup) => c ++ code }
        .toSet

    @tailrec
    private def codesOfUselessChildGroups(trie: Trie, queue: List[Code], acc: Set[Code]): Set[Code] =
      queue match {
        case code :: queueTail =>
          val acc2 =
            if (isUselessLookingDown(trie, code))
              acc | codesOfActiveChildGroups(trie, code) + code
            else
              acc
          codesOfUselessChildGroups(trie, queueTail, acc2)
        case Nil => acc
      }

    @tailrec
    private def codesOfUselessParentGroups(trie: Trie, queue: List[Code], acc: Set[Code]): Set[Code] =
      queue match {
        case h :: t =>
          if (isUselessLookingDown(trie, h)) {
            val next = NonEmptyVector.maybe(h.init, t)(_ :: t)
            codesOfUselessParentGroups(trie, next, acc + h)
          } else
            codesOfUselessParentGroups(trie, t, acc)
        case Nil => acc
      }

    private def calcDeletableGroups(p: Project, directSelRcgCodes: TraversableOnce[Code], deletableReqs: DeletableReqs): DeletableGroups = {

      // By "externally" I mean external to this fn/logic. All deletable if this fn did nothing.
      val externallyDeletable: List[Code] = {
        var b = Set.newBuilder[Code]
        b ++= deletableReqs.iterator.flatMap(r => p.content.reqCodes.activeReqCodesByReqId(r.req.id).iterator)
        b ++= directSelRcgCodes
        b.result().toList
      }

      // Step 1. After all deletions have occurred, under everything selected(able?), what's useless?

      val trie1: Trie =
        p.content.reqCodes.trie @-- externallyDeletable

      val step1: Set[Code] =
        codesOfUselessChildGroups(trie1, externallyDeletable, Set.empty)

      // Step 2. After all deletions have occurred, for everything selected(able?), which parents are now useless?

      val trie2: Trie =
        trie1 @-- step1

      val step2: Set[Code] =
        codesOfUselessParentGroups(trie2, externallyDeletable, step1)

      // Done. Build results.

      def makeRcgRow(c: Code, g: LiveCodeGroup): GroupRow = {
        var subReqs = Set.newBuilder[(ReqId, String)]
        var subGrps = Set.newBuilder[(ReqCodeId, String)]
        def subCodeStr(c2: Code) = "." + PlainText.reqCode(c2)

        p.content.reqCodes.trie.dropPath(c).foreachPathAndValue {
          case (k, a: ActiveReq)   => subReqs += ((a.reqId, subCodeStr(k)))
          case (k, a: ActiveGroup) => subGrps += ((a.id, subCodeStr(k)))
          case (_, _: Inactive)    => ()
        }
        GroupRow(g, PlainText.reqCode(c), subReqs.result(), subGrps.result())
      }

      step2.iterator
        .map(c => (c, p.content.reqCodes.get(c)))
        .collect { case (c, Some(a: ActiveGroup)) => makeRcgRow(c, a.group) }
        .toVector
        .sortBy(_.codeStr)
    }

    private def calcInitiallySelectedGroups(p              : Project,
                                            directSelGroups: Set[ReqCodeId],
                                            deletableGroups: DeletableGroups,
                                            selectedReqs   : Iterable[ReqId]): Set[ReqCodeId] = {

      val indirectGroupIds: Set[ReqCodeId] =
        deletableGroups.iterator
          .map(_.id)
          .filterNot(directSelGroups.contains)
          .toSet

      def codesOfSelectedReqs: Iterator[Code] =
        selectedReqs.iterator.flatMap(id => p.content.reqCodes.activeReqCodesByReqId(id).iterator)

      val postDeletionTrie: Trie =
        p.content.reqCodes.trie @-- codesOfSelectedReqs

      val select: Set[ReqCodeId] =
        indirectGroupIds.filter(id => isUselessLookingDown(postDeletionTrie, p.content.reqCodes.reqCode(id)))

      directSelGroups | select
    }

  }

}
