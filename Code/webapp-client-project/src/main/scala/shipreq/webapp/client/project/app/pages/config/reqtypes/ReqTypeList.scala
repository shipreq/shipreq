package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.client.project.app.Style.{reqTypeConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.ProjectWidgets

private[reqtypes] object ReqTypeList {

  final case class Props(reqTypes       : ReqTypes,
                         filterDead     : FilterDead,
                         selected       : Option[ReqTypeId],
                         select         : Option[ReqTypeId ~=> Callback],
                         pw             : ProjectWidgets.NoCtx,
                         enabled        : Enabled,
                         onClickAnywhere: Option[Reusable[Callback]],
                         usage          : Usage,
                        ) {

    def reqTypesInScope: NonEmptyVector[ReqType] =
      filterDead match {
        case HideDead => reqTypes.liveSortedByMnemonic
        case ShowDead => reqTypes.allSortedByMnemonic
      }

    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def rowKey(f: ReqType): Key =
    f.fold(_.mnemonic.value, _.id.value)

  private val tableHeader =
    <.thead(
      <.tr(
        <.th(FieldNames.mnemonic),
        <.th(FieldNames.name),
        <.th(Shared.implication),
        <.th("Usage", *.listTableUsage(Live)),
      ))

  private def render(p: Props): VdomNode = {

    val modificationEnabled: Enabled =
      p.enabled & Enabled.when(p.select.isDefined)

    def rowState(id: ReqTypeId): *.RowState =
      if (p.selected.exists(_ ==* id))
        *.RowState.Selected
      else if (modificationEnabled is Disabled)
        *.RowState.Disabled
      else
        *.RowState.Enabled

    def renderRow(rt: ReqType) = {
      import rt.{live, reqTypeId => id}

      var mnemonics: TagMod =
        rt.mnemonic.value

      if (p.filterDead.is(ShowDead) && rt.oldMnemonics.nonEmpty)
        mnemonics = TagMod(mnemonics, ", ", Shared.renderOldMnemonics(rt))

      val select: ReactEvent => Option[Callback] =
        e => p.select.map(_(id).asEventDefault(e).void)

      val td = <.td(*.listTableCell(live))

      <.tr(
        *.listTableRow((rowState(id), live)),
        ^.key := rowKey(rt),
        ^.onClick ==>? select,

        td(mnemonics),
        td(rt.name),
        td(rt.implication.toText),

        <.td(
          *.listTableUsage(live),
          p.usage.reqTypeLink(rt.reqTypeId, p.filterDead)))
    }


    <.table(
      *.listTable,
      p.onClickAnywhere.whenDefined(^.onClick --> _),
      tableHeader,
      <.tbody(
        p.reqTypesInScope.whole.toVdomArray(renderRow)))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}