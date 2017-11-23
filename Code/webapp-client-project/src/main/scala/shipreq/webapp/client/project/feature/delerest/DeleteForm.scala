package shipreq.webapp.client.project.feature.delerest

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.deletion._
import shipreq.webapp.base.feature.PreviewFeature
import shipreq.webapp.base.lib.KeyboardTheme
import shipreq.webapp.base.protocol.UpdateContentCmd.DeleteReqs
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table}
import shipreq.webapp.client.project.app.Style.{deletionForm => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature.Selection
import shipreq.webapp.client.project.widgets.{ProjectWidgets, RichTextEditor, Widgets}

object DeleteForm {
  import DeletionLogic._

  final case class Props(data      : Data,
                         widgets   : ProjectWidgets.NoCtx,
                         textSearch: TextSearch,
                         perform   : DeleteReqs => Callback,
                         cancel    : Callback) {
    def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(selectedReqs  : Selection[ReqId],
                         selectedGroups: Selection[ReqCodeId],
                         reason        : String)
  object State {
    def init(p: Props): State =
      apply(
        Selection(p.data.initialReqs),
        Selection(p.data.initialGroups),
        "")
  }

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
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build

}
