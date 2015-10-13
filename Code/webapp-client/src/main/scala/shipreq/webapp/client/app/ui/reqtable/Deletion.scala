package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalajs.js
import shipreq.base.util.{Memo, Util}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.reqtable.{deleteRestore => *}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.On

object Deletion {

  case class ReqRow(req: Req, indent: Int, impliedBy: Vector[Req])

  case class Props(project     : Project,
                   widgets     : ProjectWidgets,
                   cancel      : Callback,
                   reqRows     : Vector[ReqRow],
                   initialState: State)

  def initProps(project     : Project,
                widgets     : ProjectWidgets,
                cancel      : Callback,
                selectedReqs: Traversable[Req]): Props = {

    val customReqTypes = project.config.customReqTypes
    val lookupReq      = project.reqs.reqs.need _
    val imps_>         = project.implications.srcToTgt
    val imps_<         = project.implications.tgtToSrc

    val directlySelectedReqIds: Set[ReqId] =
      selectedReqs.map(_.id)(collection.breakOut)

    var selectedReqIds: Set[ReqId] =
      directlySelectedReqIds

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
        if (level != 0)
          cascadePending push new CascadePending(r, impBy, true)

        // Add implied reqs
        val kids: List[Req] =
          imps_>(r.id).iterator
            .filterNot(directlySelectedReqIds.contains)
            .map(lookupReq)
            .toList
        addReqRows(kids, level + 1)
      }
    }
    addReqRows(selectedReqs, 0)

    // Copy-paste with Backend#render
    def liveGivenState(r: Req): Live =
      (Dead <~ selectedReqIds.contains(r.id)) && r.live(customReqTypes)

    // Decide which implied reqs to recommend cascading deletion
    // (I'm sure there's a smarter way but this will do)
    var changed = true
    while (changed) {
      changed = false
      for (t <- cascadePending)
        if (t.pending && t.imp.forall(liveGivenState(_) :: Dead)) {
          changed = true
          t.pending = false
          selectedReqIds += t.req.id
        }
    }

    val state = State(selectedReqIds)
    Props(project, widgets, cancel, reqRows.result(), state)
  }

  // ===================================================================================================================

  @Lenses
  case class State(selectedReqIds: Set[ReqId])

  val alwaysOn = UI.checkbox(On)(^.readOnly := true, ^.disabled := true)

  class Backend($: BackendScope[Props, State]) {
    // Not worried about concurrent project updates.
    val project = $.props.map(_.project).runNow()
    val widgets = $.props.map(_.widgets).runNow()
    val customReqTypes = project.config.customReqTypes

    val cancelButton: ReactElement =
      <.button(^.onClick --> $.props.flatMap(_.cancel), "Cancel")

    private val renderImpliedByItemMemo = Live.memo { live =>
      val style: Req => TagMod = _ => *.impliedByItem(live)
      Memo.by((_: Req).id)(widgets.reqRefBasic(_, identity, style))
    }

    def render(p: Props, s: State): ReactElement = {
      val selectedReqIds = s.selectedReqIds

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

      <.div(
        <.div("Reqs to delete"),
        <.table(<.tbody(p.reqRows.map(renderRow): _*)),
        <.div("RCGs to delete"),
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
