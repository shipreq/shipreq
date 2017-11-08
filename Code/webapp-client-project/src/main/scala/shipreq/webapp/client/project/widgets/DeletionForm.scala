package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.univeq._
import monocle.macros.Lenses
import scala.annotation.tailrec
import scala.collection.TraversableOnce
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.{UiText, data}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.PreviewFeature
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.protocol.UpdateContentCmd.DeleteReqs
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table}
import shipreq.webapp.client.project.app.Style.{deletionForm => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature.Selection
import MTrie.Ops

object DeletionForm {

  type DeletableReqs   = Vector[ReqRow]
  type DeletableGroups = Vector[GroupRow]

  final case class Data(project        : Project,
                        deletableReqs  : DeletableReqs,
                        deletableGroups: DeletableGroups,
                        initialState   : State) {

    val optionalReqIds: Set[ReqId] =
      deletableReqs.iterator.filter(_.indent !=* 0).map(_.req.id).toSet
  }

  @Lenses
  final case class State(selectedReqs  : Selection[ReqId],
                         selectedGroups: Selection[ReqCodeId],
                         reason        : String)

  final case class Props(data      : Data,
                         widgets   : ProjectWidgets.NoCtx,
                         textSearch: TextSearch,
                         perform   : DeleteReqs => Callback,
                         cancel    : Callback)

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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████
  // Data Logic

  object Data {

    /** Completely ignores code groups.
      * Clearer UX to separate Req deletion (with implication intelligence) from Code Group deletion.
      */
    def forReqs(project: Project, directlySelectedReqs: NonEmptySet[ReqId]): Data = {
      val deletableReqs = calcDeletableReqs(project, directlySelectedReqs)
      val initSelReqs   = calcInitiallySelectedReqs(project, deletableReqs, directlySelectedReqs).whole
      val state         = State(Selection(initSelReqs), Selection(Set.empty), "")
      Data(project, deletableReqs, Vector.empty, state)
    }

    private[widgets] def forReqsAndCodeGroups__TEST_ONLY(project               : Project,
                                                         directlySelectedReqs  : NonEmptySet[ReqId],
                                                         directlySelectedGroups: Set[ReqCodeId]): Data = {
      val directlySelectedCodeGroups: Set[ReqCode.Value] =
        directlySelectedGroups.map(project.content.reqCodes.reqCode)

      val deletableReqs   = calcDeletableReqs(project, directlySelectedReqs)
      val deletableGroups = calcDeletableGroups(project, directlySelectedCodeGroups, deletableReqs)

      val initSelReqs   = calcInitiallySelectedReqs(project, deletableReqs, directlySelectedReqs).whole
      val initSelGroups = calcInitiallySelectedGroups(project, directlySelectedGroups, deletableGroups, initSelReqs)

      val state = State(Selection(initSelReqs), Selection(initSelGroups), "")
      Data(project, deletableReqs, deletableGroups, state)
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
        (Dead when select.contains(r.id)) & r.live(p.config.reqTypes)

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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  final class Backend($: BackendScope[Props, State]) {
    val setReqSel = Reusable.fn($ setStateFnL State.selectedReqs)
    val setReason = Reusable.fn($ setStateFnL State.reason)

    def reasonEditorProps(p: Props, s: State): RichTextEditor.DeletionReason.Props =
      RichTextEditor.DeletionReason.Props(
        project          = p.data.project,
        plainTextNoCtx   = p.widgets.plainText,
        textSearch       = p.textSearch,
        projectWidgets   = p.widgets,
        edit             = StateSnapshot.withReuse(s.reason)(setReason),
        asyncStatus      = None,
        abort            = None,
        commitFn         = None,
        commitVerb       = "",
        preview          = PreviewFeature.ReadWrite.Single.alwaysShow,
        preEditValue     = None,
        extraKbShortcuts = KeyboardTheme.Shortcuts.empty,
        showInstructions = true)

    def renderReqTable(p: Props, s: State): VdomElement = {
      val project        = p.data.project
      val customReqTypes = project.config.reqTypes
      val selection      = s.selectedReqs.updateBy(setReqSel).legal(p.data.optionalReqIds)

      val header: VdomTag =
        <.thead(
          <.tr(
            <.th(^.rowSpan := 2, *.reqTableSelCol, selection.total.checkboxAndOnClick),
            <.th(^.rowSpan := 2, UiText.ColumnNames.pubid),
            <.th(^.rowSpan := 2, UiText.ColumnNames.title),
            <.th(^.colSpan := 2, *.reqTableHeaderImpsTop, UiText.ColumnNames.implications(Backwards))),
          <.tr(
            <.th(
              *.reqTableHeaderImpsBottomLeft,
              Icon.TrashOutline.withColour(Colour.Red).tag(*.reqTableHeaderImpsIcon)),
            <.th(
              *.reqTableHeaderImpsBottomRight,
              Icon.Unhide.tag(*.reqTableHeaderImpsIcon))))

      val liveGivenState: Req => Live =
        r => (Dead when s.selectedReqs.selected.contains(r.id)) & r.live(customReqTypes)

      val renderImpliedByItem =
        p.widgets.PubidFormat(Plain, *.reqTableImps(_), liveFn = liveGivenState)

      def reqRow(rr: ReqRow): VdomTag = {
        val req: Req = rr.req
        val id: ReqId = rr.req.id
        val live: Live = liveGivenState(req)

        val sel: TagMod =
          if (selection.legal contains id)
            selection(rr.req.id).checkboxAndOnClick
          else
            Widgets.checkboxReadOnly(On)

        val pubidStr: String =
          PlainText.pubid(req.pubid, project)

        val indentedPubid: TagMod =
          if (rr.indent ==* 0)
            <.div(*.pubid(live), pubidStr)
          else
            TagMod(
              <.div(^.width := *.indentWidth(rr.indent)),
              <.div(*.reqTableTreeIndicator, "↳"),
              <.div(*.pubid(live), pubidStr))

        val imps: Live.Values[VdomTag] =
          Live.Values
            .partition[Vector, Req](rr.impliedBy)(liveGivenState)
            .map(renderImpliedByItem.reqs)

        <.tr(
          *.reqTableRow(live),
          ^.key := id.value,
          <.td(*.reqTableSelCol, sel),
          <.td(*.reqTablePubidCell, indentedPubid),
          <.td(*.reqTableTitle(live), p.widgets reqTitle rr.req),
          <.td(*.reqTableImpsCell, imps(Dead)),
          <.td(*.reqTableImpsCell, imps(Live)))
      }

      Table.celledCompactUnstackable(
        *.reqTable,
        header,
        <.tbody(p.data.deletableReqs.toVdomArray(reqRow)))
    }

    val cancelButton: VdomTag =
      Button(
        tipe = Button.Type.BasicIconAndText(Icon.Remove, UiText.buttonAbortChange),
        colour = Colour.Black
      ).tag(*.cancelButton, ^.onClick --> $.props.flatMap(_.cancel))

    def render(p: Props, s: State): VdomElement = {
      assert(p.data.deletableGroups.isEmpty,
        "Since proper UI/UX implementation, DeletionForm no longer accepts deletable code-groups")

      val reasonTextProps = reasonEditorProps(p, s)

      val deletionReason: VdomTag =
        <.section(
          <.h4(*.deletionReasonHeader, UiText.ColumnNames.deletionReason + ":"),
          RichTextEditor.DeletionReason.Component(reasonTextProps))

      val commit: Option[Callback] =
        for {
          reqs   ← NonEmptySet.option(s.selectedReqs.selected)
          reason ← reasonTextProps.validated.toOption
        } yield p.perform(DeleteReqs(reqs, Set.empty, reason))

      val deleteButton: VdomTag =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Trash, UiText.Life.delete),
          state = Button.State.enabledWhen(commit.isDefined),
          colour = Colour.Red
        ).tag(^.onClick -->? commit)

      <.main(
        *.main,
        TestMarker.deletionForm.tagMod,
        <.h2("You are about to delete the following requirements:"),
        <.section(
          <.div(*.reqHelp, "In addition to those you selected, implied requirements are also presented with exclusively-implied requirements auto-selected for deletion."),
          renderReqTable(p, s)),
        <.div(*.bottomSections,
          <.div(*.bottomSectionL, deletionReason),
          <.div(*.bottomSectionR, cancelButton, <.br, deleteButton)))
    }
  }

  val Component = ScalaComponent.builder[Props]("Deletion")
    .initialStateFromProps(_.data.initialState)
    .renderBackend[Backend]
    .build
}
