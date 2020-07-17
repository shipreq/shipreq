package shipreq.webapp.client.project.feature.deletion

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.semantic.{Colour, Icon, Table}
import shipreq.webapp.client.project.app.Style.{deletionRestorationForms => *}
import shipreq.webapp.client.project.feature.Selection
import shipreq.webapp.client.project.feature.deletion.DeletionRestorationLogic.{ActionableReqs, ReqRow}
import shipreq.webapp.client.project.widgets.{ProjectWidgets, Widgets}

private[deletion] object SharedUI {

  def reqTable(mode          : DeleteOrRestore,
               project       : Project,
               widgets       : ProjectWidgets.NoCtx,
               actionableReqs: ActionableReqs,
               selectedReqs  : Selection[ReqId],
               selection     : Selection.LegalWithUpdateFn[ReqId],
               rowStyle      : Live => TagMod): VdomElement = {

    val header: VdomTag =
      <.thead(
        <.tr(
          <.th(^.rowSpan := 2, *.reqTableSelCol, selection.total.checkboxAndOnClick),
          <.th(^.rowSpan := 2, SpecialBuiltInField.Pubid.name),
          <.th(^.rowSpan := 2, SpecialBuiltInField.Title.name),
          <.th(^.colSpan := 2, *.reqTableHeaderImpsTop, SpecialBuiltInField.ImplyBackward.name)),
        <.tr(
          <.th(
            *.reqTableHeaderImpsBottomLeft,
            Icon.TrashOutline.withColour(Colour.Red).tag(*.reqTableHeaderImpsIcon)),
          <.th(
            *.reqTableHeaderImpsBottomRight,
            Icon.Unhide.tag(*.reqTableHeaderImpsIcon))))

    val liveGivenState: Req => Live =
      r => mode.toState when selectedReqs.selected.contains(r.id)

    val renderImpliedByItem =
      widgets.PubidFormat(Plain, *.reqTableImps(_), liveFn = liveGivenState)

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
        rowStyle(live),
        ^.key := id.value,
        <.td(*.reqTableSelCol, sel),
        <.td(*.reqTablePubidCell, indentedPubid),
        <.td(*.reqTableTitle(live), widgets reqTitle rr.req),
        <.td(*.reqTableImpsCell, imps(Dead)),
        <.td(*.reqTableImpsCell, imps(Live)))
    }

    Table.celledCompactUnstackable(
      *.reqTable,
      header,
      <.tbody(actionableReqs.toVdomArray(reqRow)))
  }

}
