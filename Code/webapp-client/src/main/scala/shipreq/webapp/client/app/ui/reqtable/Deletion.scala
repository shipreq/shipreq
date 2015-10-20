package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import monocle.macros.Lenses
import scala.annotation.tailrec
import scala.collection.TraversableOnce
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd.DeleteReqs
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.{VUCA, Selection, ProjectWidgets}
import shipreq.webapp.client.app.ui.Style.reqtable.{deleteRestore => *}
import shipreq.webapp.client.app.ui.reqtable.edit.RichTextEditor
import shipreq.webapp.client.lib.ui.UI
import MTrie.Ops

object Deletion {

  case class Props(project        : Project,
                   widgets        : ProjectWidgets,
                   projectText    : PlainText.ForProject,
                   textSearch     : TextSearch,
                   perform        : DeleteReqs => Callback,
                   cancel         : Callback,
                   deletableReqs  : DeletableReqs,
                   deletableGroups: DeletableGroups,
                   initialState   : State)

  def makeProps(props1     : Props1,
                widgets    : ProjectWidgets,
                projectText: PlainText.ForProject,
                textSearch : TextSearch,
                perform    : DeleteReqs => Callback,
                cancel     : Callback): Props = {
    import props1._
    Props(project, widgets, projectText, textSearch, perform, cancel, deletableReqs, deletableGroups, initialState)
  }

  case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])

  case class GroupRow(group    : LiveReqCodeGroup,
                      codeStr  : String,
                      subReqs  : Set[(ReqId, String)],
                      subGroups: Set[(ReqCodeId, String)]) {
    @inline def id = group.id

    def liveSubs(r: ReqId => Live, g: ReqCodeId => Live): Iterator[String] =
      subReqs  .iterator.filter(t => r(t._1) :: Live).map(_._2) ++
      subGroups.iterator.filter(t => g(t._1) :: Live).map(_._2)
  }

  type DeletableReqs   = Vector[ReqRow]
  type DeletableGroups = Vector[GroupRow]

  case class Props1(project        : Project,
                    deletableReqs  : DeletableReqs,
                    deletableGroups: DeletableGroups,
                    initialState   : State)

  def initProps1(p              : Project,
                 directSelReqs  : Traversable[Req],
                 directSelGroups: Set[ReqCodeId]): Props1 = {

    val directSelReqIds: Set[ReqId] =
      directSelReqs.map(_.id)(collection.breakOut)

    val directSelRcgCodes: Set[ReqCode.Value] =
      directSelGroups.map(p.reqCodes.reqCode)

    val deletableReqs   = calcDeletableReqs(p, directSelReqs, directSelReqIds)
    val deletableGroups = calcDeletableGroups(p, directSelRcgCodes, deletableReqs)

    val initSelReqs   = calcInitiallySelectedReqs(p, deletableReqs, directSelReqIds)
    val initSelGroups = calcInitiallySelectedGroups(p, directSelGroups, deletableGroups, initSelReqs)

    val state = State(Selection(initSelReqs), Selection(initSelGroups), "")
    Props1(p, deletableReqs, deletableGroups, state)
  }

  // ===================================================================================================================
  // Requirement logic
  // ===================================================================================================================

  private def calcDeletableReqs(p: Project, directSel: Traversable[Req], directSelReqIds: Set[ReqId]): DeletableReqs = {
    val lookupReq = p.reqs.reqs.need _
    val imps_>    = p.implications.srcToTgt
    val imps_<    = p.implications.tgtToSrc

    val reqOrder = Ordering.by((_: Req).pubid)(p.config.pubidOrdering)

    def sortReqs(a: Array[Req]): Unit =
      java.util.Arrays.sort(a, reqOrder)

    var reqRows = Vector.newBuilder[ReqRow]

    val reqFilter: Req => Boolean =
      _.live(p.config.customReqTypes) :: Live

    def go(reqs: TraversableOnce[Req], level: Int): Unit = {

      // Sort current tier of reqs
      val reqArray = reqs.toIterator.filter(reqFilter).toArray
      sortReqs(reqArray)

      for (r <- reqArray) {

        // Gather implied-by
        val impByArray: Array[Req] = imps_<(r.id).iterator.map(lookupReq).filter(reqFilter).toArray
        sortReqs(impByArray)
        val impBy = impByArray.toVector

        // Add row
        reqRows += ReqRow(r, level, impBy)

        // Add implied reqs
        val kids: List[Req] =
          imps_>(r.id).iterator
            .filterNot(directSelReqIds.contains)
            .map(lookupReq)
            .toList
        go(kids, level + 1)
      }
    }

    go(directSel, 0)

    reqRows.result()
  }

  private def calcInitiallySelectedReqs(p: Project, deletableReqs: DeletableReqs, directSel: Set[ReqId]): Set[ReqId] = {

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
      (Dead <~ select.contains(r.id)) && r.live(p.config.customReqTypes)

    // Decide which implied reqs to recommend cascading deletion
    // (I'm sure there's a smarter way but this will do)
    var changed = true
    while (changed) {
      changed = false
      for (t <- cascadePending if t.pending) {
        t.pending = false
        if (t.imp.forall(liveGivenState(_) :: Dead)) {
          changed = true
          select += t.req.id
        }
      }
    }

    select
  }

  // ===================================================================================================================
  // ReqCode Group logic
  // ===================================================================================================================
  import ReqCode.{Value => Code, ActiveGroup, ActiveReq, Inactive, Trie}

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
      b ++= deletableReqs.iterator.flatMap(r => p.reqCodes.activeReqCodesByReqId(r.req.id).iterator)
      b ++= directSelRcgCodes
      b.result().toList
    }

    // Step 1. After all deletions have occurred, under everything selected(able?), what's useless?

    val trie1: Trie =
      p.reqCodes.trie @-- externallyDeletable

    val step1: Set[Code] =
      codesOfUselessChildGroups(trie1, externallyDeletable, Set.empty)

    // Step 2. After all deletions have occurred, for everything selected(able?), which parents are now useless?

    val trie2: Trie =
      trie1 @-- step1

    val step2: Set[Code] =
      codesOfUselessParentGroups(trie2, externallyDeletable, step1)

    // Done. Build results.

    def makeRcgRow(c: Code, g: LiveReqCodeGroup): GroupRow = {
      var subReqs = Set.newBuilder[(ReqId, String)]
      var subGrps = Set.newBuilder[(ReqCodeId, String)]
      def subCodeStr(c2: Code) = "." + PlainText.reqCode(c2)

      p.reqCodes.trie.dropPath(c).foreachPathAndValue {
        case (k, a: ActiveReq)   => subReqs += ((a.reqId, subCodeStr(k)))
        case (k, a: ActiveGroup) => subGrps += ((a.id, subCodeStr(k)))
        case (_, _: Inactive)    => ()
      }
      GroupRow(g, PlainText.reqCode(c), subReqs.result(), subGrps.result())
    }

    step2.iterator
      .map(c => (c, p.reqCodes.get(c)))
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
      selectedReqs.iterator.flatMap(id => p.reqCodes.activeReqCodesByReqId(id).iterator)

    val postDeletionTrie: Trie =
      p.reqCodes.trie @-- codesOfSelectedReqs

    val select: Set[ReqCodeId] =
      indirectGroupIds.filter(id => isUselessLookingDown(postDeletionTrie, p.reqCodes.reqCode(id)))

    directSelGroups | select
  }

  // ===================================================================================================================
  // Logic End
  // ===================================================================================================================

  @Lenses
  case class State(selectedReqs: Selection[ReqId], selectedGroups: Selection[ReqCodeId], reason: String)

  class Backend($: BackendScope[Props, State]) {
    // Not worried about concurrent project updates.
    // Usage is that Props are only assigned once to create the component - all subsequent updates are by state

    val project = $.props.map(_.project).runNow()
    val widgets = $.props.map(_.widgets).runNow()
    val customReqTypes = project.config.customReqTypes

    val visibleRCGs: Set[ReqCodeId] = $.props.runNow().deletableGroups.map(_.id)(collection.breakOut)

    val setReqSel = ReusableFn($ _setStateL State.selectedReqs)
    val setRcgSel = ReusableFn($ _setStateL State.selectedGroups)

    val reasonEditorProps: State => RichTextEditor.DeletionReason.Props = {
      implicit def autoPx[A](a: A): Px[A] = Px.const(a)
      val p = $.props.runNow()
      val e = RichTextEditor.DeletionReason.prepare(p.project, p.projectText, p.widgets, p.textSearch)
      val u = $ _setStateL State.reason
      s => e(VUCA.vu(s.reason, u))
    }

    val cancelButton: ReactElement =
      <.button(^.onClick --> $.props.flatMap(_.cancel), "Cancel")

    private val renderImpliedByItemMemo = Live.memo { live =>
      val style: Req => TagMod = _ => *.impliedByItem(live)
      Memo.by((_: Req).id)(widgets.reqRefBasic(_, identity, style))
    }

    // -----------------------------------------------------------------------------------------------------------------
    def renderReqs(p: Props, s: State): TagMod = {
      val selAll = s.selectedReqs.updateBy(setReqSel)

      // Copy-paste with initProps()
      def liveGivenState(r: Req): Live =
        (Dead <~ s.selectedReqs.selected.contains(r.id)) && r.live(customReqTypes)

      def renderImpliedByItem(req: Req): ReactElement =
        renderImpliedByItemMemo(liveGivenState(req))(req)

      def reqRow(rr: ReqRow): TagMod = {
        import rr._
        val live = liveGivenState(req)

        val sel = if (indent == 0) None else Some(selAll(req.id))

        val td = <.td(*.row(live), sel.map(_.onClick))

        val reqTitle =
          <.span(
            *.reqDesc,
            PlainText.pubid(project, req.pubid) + ": ",
            widgets reqTitle req)

        val impBy =
          if (impliedBy.isEmpty)
            EmptyTag
          else
            <.span(
              <.span(*.impliedByPrefix, "⇐"),
              UI.vector(impliedBy, UI.sepComma)(renderImpliedByItem))

        <.tr(
          td(<.span(*.indent(indent)), sel.fold(UI.checkboxAlwaysOn)(_.checkbox), reqTitle),
          td(impBy))
      }

      <.table(
        <.tbody(
          p.deletableReqs.map(reqRow): _*))
    }

    // -----------------------------------------------------------------------------------------------------------------
    def renderGroups(p: Props, s: State): TagMod = {
      val selAll = s.selectedGroups.updateBy(setRcgSel).legal(visibleRCGs)

      val reqLive: ReqId => Live =
        Dead <~ s.selectedReqs.selected.contains(_)

      val groupLive: ReqCodeId => Live =
        Dead <~ s.selectedGroups.selected.contains(_)

      def groupRow(r: GroupRow): TagMod = {
        val liveCodes: Vector[String] = {
          val ls = r.liveSubs(reqLive, groupLive)
          (SortedSet.empty[String] ++ ls).toVector
        }

        val sel = selAll(r.id)

        val td = <.td(*.row(groupLive(r.id)), sel.onClick)

        def subCodes: TagMod =
          TagMod(
            *.subCodeCount(Live <~ liveCodes.nonEmpty),
            liveCodes.length,
          ^.title := liveCodes.mkString("\n"))

        <.tr(
          td(sel.checkbox),
          td(r.codeStr),
          td(widgets reqCodeGroupTitle r.group),
          td(subCodes))
      }

      def selAllBox: TagMod =
        if (p.deletableGroups.length < 2)
          EmptyTag
        else
          selAll.total.checkboxAndOnClick

      <.table(
        <.thead(
          <.tr(
            <.th(selAllBox),
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th("Sub-Codes"))),
        <.tbody(
          p.deletableGroups.map(groupRow): _*))
    }

    // -----------------------------------------------------------------------------------------------------------------
    def render(p: Props, s: State): ReactElement = {

      def reqSection =
        <.section(
          <.div(*.section, "Requirements to delete"),
          renderReqs(p, s))

      def groupSection: TagMod =
        if (p.deletableGroups.isEmpty)
          EmptyTag
        else
          <.section(
            <.div(*.section, UiText.reqCodeGroups + " to delete"),
            renderGroups(p, s))

      val reason = reasonEditorProps(s)
      def reasonSection =
        <.section(
          <.div(*.section, "Reason for deletion"),
          reason.render)

      val commit: Option[Callback] =
        for {
          reqs          ← NonEmptySet.option(s.selectedReqs.selected)
          reqCodeGroups = s.selectedGroups.selected
          dr            ← reason.parseResult.toOption
        } yield
        p perform DeleteReqs(reqs, reqCodeGroups, dr)

      def deleteButton =
        <.button(
          ^.disabled := commit.isEmpty,
          ^.onClick -->? commit,
          "Delete")

      <.div(
        reqSection,
        groupSection,
        reasonSection,
        deleteButton,
        cancelButton)
    }
  }

  val Component = ReactComponentB[Props]("Deletion")
    .initialState_P(_.initialState)
    .renderBackend[Backend]
    .build
}
