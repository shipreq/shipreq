package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.{Colour => _, _}
import shipreq.webapp.base.ui.semantic.{Icon, Message}
import shipreq.webapp.client.project.app.Style.{issueConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.Usage
import shipreq.webapp.client.project.widgets.ProjectWidgets

private[issues] object CustomIssueTypeList {

  final case class Props(customIssueTypes: CustomIssueTypeIMap,
                         filterDead      : FilterDead,
                         selected        : Option[CustomIssueTypeId],
                         select          : Option[CustomIssueTypeId ~=> Callback],
                         pw              : ProjectWidgets.NoCtx,
                         enabled         : Enabled,
                         onClickAnywhere : Option[Reusable[Callback]],
                         usage           : Usage,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val tableHeader =
    <.thead(
      <.tr(
        <.th(FieldNames.hashRefKey),
        <.th(FieldNames.desc),
        <.th("Usage", *.listTableUsage(Live)),
      ))

  private def renderEmpty: VdomNode =
    Message(
      Message.Style(Message.Type.Info),
      Icon.InfoCircle,
      "No issue types",
      "Create new issue types using the button above.")

  private def renderTable(p: Props, rows: Iterator[CustomIssueType]): VdomNode = {

    val modificationEnabled: Enabled =
      p.enabled & Enabled.when(p.select.isDefined)

    def rowState(id: CustomIssueTypeId): *.RowState =
      if (p.selected.exists(_ ==* id))
        *.RowState.Selected
      else if (modificationEnabled is Disabled)
        *.RowState.Disabled
      else
        *.RowState.Enabled

    def renderRow(i: CustomIssueType): VdomTag = {

      val select: ReactEvent => Option[Callback] =
        e => p.select.map(_(i.id).asEventDefault(e).void)

      val td = <.td(*.listTableCell(i.live))

      <.tr(
        *.listTableRow((rowState(i.id), i.live)),
        ^.key := i.id.value,
        ^.onClick ==>? select,

        td(i.key.with_#),
        td(i.desc.whenDefined),

        <.td(
          *.listTableUsage(i.live),
          p.usage.customIssueTypeLink(i.id, p.filterDead)))
    }

    <.table(
      *.listTable,
      p.onClickAnywhere.whenDefined(^.onClick --> _),
      tableHeader,
      <.tbody(
        rows.toVdomArray(renderRow)))
  }

  private def render(p: Props): VdomNode = {
    val rows =
      MutableArray(p.filterDead.filterFn.iteratorBy(p.customIssueTypes.valuesIterator)(_.live))
        .sortBy(_.key.value)

    if (rows.isEmpty)
      renderEmpty
    else
      renderTable(p, rows.iterator())
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}