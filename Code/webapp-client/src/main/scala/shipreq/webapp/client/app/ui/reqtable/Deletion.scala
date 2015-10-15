package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import monocle.macros.Lenses
import shipreq.webapp.base.UiText
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scalacss.ScalaCssReact._
import scalajs.js
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Grammar, PlainText}
import shipreq.webapp.client.app.ui.{Selection, ProjectWidgets}
import shipreq.webapp.client.app.ui.Style.reqtable.{deleteRestore => *}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.On
import MTrie.Ops

object Deletion {

  case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])

  case class RcgRow(group    : LiveReqCodeGroup,
                    codeStr  : String,
                    subReqs  : Set[(ReqId, String)],
                    subGrps  : Set[(ReqCodeId, String)]) {
    @inline def id = group.id

    def liveSubs(r: ReqId => Live, g: ReqCodeId => Live): Iterator[String] =
      subReqs.iterator.filter(t => r(t._1) :: Live).map(_._2) ++
      subGrps.iterator.filter(t => g(t._1) :: Live).map(_._2)
  }

  case class Props1(project      : Project,
                    reqRows      : Vector[ReqRow],
                    deletableRCGs: Vector[RcgRow],
                    initialState : State)

  def initProps1(project             : Project,
                 directlySelectedReqs: Traversable[Req],
                 directlySelectedRcgs: Set[ReqCodeId]): Props1 = {

    val (reqRows, reqSelection) = findDeletableReqs(project, directlySelectedReqs)
    val rcgRows = findDeletableRcgRows(project, directlySelectedRcgs, reqRows)

    val rcgSelection = Selection(initallySelectedRCGs(project, directlySelectedRcgs, rcgRows, reqSelection))

    val state = State(reqSelection, rcgSelection)
    Props1(project, reqRows, rcgRows, state)
  }

  // ===================================================================================================================
  // Requirements
  // ===================================================================================================================

  private def findDeletableReqs(project: Project, directlySelectedReqs: Traversable[Req]): (Vector[ReqRow], Set[ReqId]) = {

    val lookupReq = project.reqs.reqs.need _
    val imps_>    = project.implications.srcToTgt
    val imps_<    = project.implications.tgtToSrc

    val directlySelectedReqIds: Set[ReqId] =
      directlySelectedReqs.map(_.id)(collection.breakOut)

    var selectedReqIds: Set[ReqId] = directlySelectedReqIds
//    var deletableReqs: Set[ReqId] = directlySelectedReqIds

    // Means we don't know yet whether the deletion should be cascaded by default to this item
    class CascadePending(val req: Req, val imp: Vector[Req], var pending: Boolean)
    var cascadePending = new js.Array[CascadePending]

    val reqOrder = Ordering.by((_: Req).pubid)(project.config.pubidOrdering)
    def sortReqs(a: Array[Req]): Unit =
      java.util.Arrays.sort(a, reqOrder)

    // Add rows for reqs to delete, considering other reqs implied by those being deleted
    var reqRows = Vector.newBuilder[ReqRow]
    def addReqRows(reqs: TraversableOnce[Req], level: Int): Unit = {
      val reqArray = reqs.toArray
      sortReqs(reqArray)
      for (r <- reqArray) {

        // Gather implied-by
        val impByArray: Array[Req] = imps_<(r.id).map(lookupReq)(collection.breakOut)
        sortReqs(impByArray)
        val impBy = impByArray.toVector

        // Add row
        reqRows += ReqRow(r, level, impBy)
//        deletableReqs += r.id
        if (level != 0)
          cascadePending push new CascadePending(r, impBy, true)
//        rc.activeReqCodesByReqId(r.id) foreach processReqCode

        // Add implied reqs
        val kids: List[Req] =
          imps_>(r.id).iterator
            .filterNot(directlySelectedReqIds.contains)
            .map(lookupReq)
            .toList
        addReqRows(kids, level + 1)
      }
    }
    addReqRows(directlySelectedReqs, 0)

    // Copy-paste with Backend#render
    def liveGivenState(r: Req): Live =
      (Dead <~ selectedReqIds.contains(r.id)) && r.live(project.config.customReqTypes)

    // Decide which implied reqs to recommend cascading deletion
    // (I'm sure there's a smarter way but this will do)
    var changed = true
    while (changed) {
      changed = false
      for (t <- cascadePending if t.pending) {
        t.pending = false
        if (t.imp.forall(liveGivenState(_) :: Dead)) {
          changed = true
          selectedReqIds += t.req.id
        }
      }
    }

    (reqRows.result(), selectedReqIds)
  }

  // ===================================================================================================================
  // RCG
  // ===================================================================================================================
  import ReqCode.{Value => Code, Data, ActiveGroup, ActiveReq, Inactive, Trie}

  def isUselessLookingDown(t: Trie, c: Code): Boolean = {
      def isTrieUseless(s: Trie): Boolean =
        s.valuesIterator.forall(isNodeUseless)

      def isNodeUseless(node: Trie.Node): Boolean =
        node.fold(
          b => b.value.forall(n => isDataUseless(n.value)) && isTrieUseless(b.next),
          v => isDataUseless(v.value))

      def isDataUseless(d: Data): Boolean =
        d match {
          case _: ActiveReq                 => false
          case _: ActiveGroup | _: Inactive => true
        }

      t.getNode(c).forall(isNodeUseless)
    }

    def allChildrenRCGs(t: Trie, c: Code): Set[Code] =
      t.dropPath(c).flatStream
        .collect { case (code, _: ActiveGroup) => c ++ code }
        .toSet

    @tailrec
    def findAllUselessRCGsLookingDown(trie: Trie, queue: List[Code], acc: Set[Code]): Set[Code] =
      queue match {
        case h :: t =>
          if (isUselessLookingDown(trie, h))
            findAllUselessRCGsLookingDown(trie, t, acc + h ++ allChildrenRCGs(trie, h))
          else
            findAllUselessRCGsLookingDown(trie, t, acc)
        case Nil => acc
      }

    @tailrec
    def findAllUselessParentRCGs(trie: Trie, queue: List[Code], acc: Set[Code]): Set[Code] =
      queue match {
        case h :: t =>
          if (isUselessLookingDown(trie, h))
            h.initNonEmpty match {
              case Some(p) => findAllUselessParentRCGs(trie, p :: t, acc + h)
              case None    => findAllUselessParentRCGs(trie, t, acc + h)
            }
          else
            findAllUselessParentRCGs(trie, t, acc)
        case Nil => acc
      }

  private def findDeletableRcgRows(project               : Project,
                                directlySelectedRcgIds: Set[ReqCodeId],
                                reqRows               : Vector[ReqRow]
                                 ): Vector[RcgRow]  = {
    // case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])
    

//    def fmtCodes(codes: Traversable[Code]): String =
//      codes.toList.map(PlainText.reqCode).sorted
//        //.mkString(", ")
//        .map("\n  - " + _).mkString("")


    def makeRcgRow(c: Code, g: LiveReqCodeGroup): RcgRow = {
      val subReqs = Set.newBuilder[(ReqId, String)]
      val subGrps = Set.newBuilder[(ReqCodeId, String)]
      def codestr(c2: Code) = PlainText.reqCode(c ++ c2)
      project.reqCodes.trie.dropPath(c).foreachPathAndValue {
        case (p, a: ActiveReq)   => subReqs += ((a.reqId, codestr(p)))
        case (p, a: ActiveGroup) => subGrps += ((a.id, codestr(p)))
        case (_, _: Inactive)    => ()
      }
      RcgRow(g, PlainText.reqCode(c), subReqs.result(), subGrps.result())
    }

    val directlySelectedRcgCodes: Set[Code] =
      directlySelectedRcgIds.map(project.reqCodes.reqCode)

    /** By "externally" I mean external to this fn/logic. All deletable if this fn did nothing. */
    val allExternallyDeletable: Set[Code] = {

      val idsByReqs = reqRows.iterator
        .flatMap(r => project.reqCodes.activeReqCodesByReqId(r.req.id).iterator)
        //.map(project.reqCodes(_).activeId.get)

      //directlySelectedRcgs ++ idsByReqs

      //idsByReqs ++ direct
      val b = Set.newBuilder[Code]
      b ++= idsByReqs
      b ++= directlySelectedRcgCodes
      b.result()
    }

    val removeEverythingPossible: Trie =
      project.reqCodes.trie @-- allExternallyDeletable

    // 1) after all deletions have occurred, under everything selected(able?), what's useless?

    // Children RCGs with no live chidren
    // Deletable RCGs with no live chidren
    val step1: Set[Code] =
      findAllUselessRCGsLookingDown(removeEverythingPossible, allExternallyDeletable.toList, Set.empty)

    // 2) after all deletions have occurred, for   everything selected(able?), which parents are now useless
    val afterStep1: Trie =
      removeEverythingPossible @-- step1

    val step2: Set[Code] =
      findAllUselessParentRCGs(afterStep1, allExternallyDeletable.toList, Set.empty)

    val combined = step1 | step2

    val rows = combined.iterator
      .map(c => (c, project.reqCodes.get(c)))
      .collect { case (c, Some(a: ActiveGroup)) => makeRcgRow(c, a.group) }
      .toVector
      .sortBy(_.codeStr)

    rows
  }

  private def initallySelectedRCGs(project     : Project,
                                   directlySelectedRcgs: Set[ReqCodeId],
                                   rcgRows     : Vector[RcgRow],
                                   selectedReqs: Set[ReqId]
                                    ): Set[ReqCodeId]  = {

    val codesOfSelectedReqs = selectedReqs.iterator
      .flatMap(id => project.reqCodes.activeReqCodesByReqId(id).iterator)

    val t = project.reqCodes.trie @-- codesOfSelectedReqs

    val allDeletableRcgIds: Set[ReqCodeId] =
      rcgRows.map(_.id)(collection.breakOut)

    val indirectRcgIds =
      allDeletableRcgIds &~ directlySelectedRcgs

    val select = indirectRcgIds.filter(id => isUselessLookingDown(t, project.reqCodes.reqCode(id)))

    directlySelectedRcgs | select
  }

    // ===================================================================================================================
  // End
  // ===================================================================================================================

  case class Props(project      : Project,
                   widgets      : ProjectWidgets,
                   cancel       : Callback,
                   reqRows      : Vector[ReqRow],
                   deletableRCGs: Vector[RcgRow],
                   initialState : State)

  def makeProps(props1: Props1,
                widgets      : ProjectWidgets,
                cancel       : Callback): Props = {
    import props1._
    Props(project, widgets, cancel, reqRows, deletableRCGs, initialState)
  }


  // ===================================================================================================================

  @Lenses
  case class State(selectedReqIds: Set[ReqId], selectedRCGs: Selection[ReqCodeId])

  val alwaysOn = UI.checkbox(On)(^.readOnly := true, ^.disabled := true)

  class Backend($: BackendScope[Props, State]) {
    // Not worried about concurrent project updates.
    val project = $.props.map(_.project).runNow()
    val widgets = $.props.map(_.widgets).runNow()
    val customReqTypes = project.config.customReqTypes

    val visibleRCGs: Set[ReqCodeId] = $.props.runNow().deletableRCGs.map(_.id)(collection.breakOut)

    val setRcgSel = $ _setStateL State.selectedRCGs

    val cancelButton: ReactElement =
      <.button(^.onClick --> $.props.flatMap(_.cancel), "Cancel")

    private val renderImpliedByItemMemo = Live.memo { live =>
      val style: Req => TagMod = _ => *.impliedByItem(live)
      Memo.by((_: Req).id)(widgets.reqRefBasic(_, identity, style))
    }

    def render(p: Props, s: State): ReactElement = {
      val selectedReqIds = s.selectedReqIds
      val selectedRCGs = s.selectedRCGs

      // Copy-paste with initProps()
      def liveGivenState(r: Req): Live =
        (Dead <~ selectedReqIds.contains(r.id)) && r.live(customReqTypes)

      def renderImpliedByItem(req: Req): ReactElement =
        renderImpliedByItemMemo(liveGivenState(req))(req)

      def renderRow(rr: ReqRow): TagMod = {
        import rr._
        val live = liveGivenState(req)

        val checkboxAndToggle: (ReactTag, TagMod) =
          if (indent == 0)
            (alwaysOn, EmptyTag)
          else {
            val update = $.modState(State.selectedReqIds set Util.togglePresence(selectedReqIds)(req.id))
            val toggle = ^.onClick --> update
            val chkbox = UI.checkbox(On <~ selectedReqIds.contains(req.id))(^.onChange --> update)
            (chkbox, toggle)
          }
        val checkbox      = checkboxAndToggle._1
        val toggleOnClick = checkboxAndToggle._2

        val td = <.td(*.reqRow(toggleOnClick eq EmptyTag, live), toggleOnClick)

        val reqTitle =
          <.span(
            *.reqItem,
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
          td(<.span(*.indent(indent)), checkbox, reqTitle),
          td(impBy))
      }

      val vs = selectedRCGs.visible(visibleRCGs)

      def omfomf(r: RcgRow): TagMod = {

        val liveCodes: Vector[String] = {
          val ls = r.liveSubs(Dead <~ selectedReqIds.contains(_), Dead <~ selectedRCGs.selected.contains(_))
          (SortedSet.empty[String] ++ ls).toVector
        }

        <.tr(
          <.td(selectedRCGs.oneCheckbox(r.id, setRcgSel)),
          <.td(r.codeStr),
          <.td(widgets reqCodeGroupTitle r.group),
          <.td(liveCodes.length, ^.title := liveCodes.mkString("\n")))
      }

      <.div(
        <.div("Reqs to delete"),
        <.table(<.tbody(p.reqRows.map(renderRow): _*)),

        <.div("RCGs to delete"),
        <.table(
          <.thead(<.tr(
            <.th(vs totalCheckbox setRcgSel),
            <.th(UiText.ColumnNames.code),
            <.th(UiText.ColumnNames.title),
            <.th("Sub-Codes"))),
          <.tbody(p.deletableRCGs.map(omfomf): _*)),

        <.div("Reason"),

        <.div("Delete"),
        cancelButton)
    }
  }

  val Component = ReactComponentB[Props]("Deletion")
    .initialState_P(_.initialState)
    .renderBackend[Backend]
    .build
}
