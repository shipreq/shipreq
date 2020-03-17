package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd.FieldUpdateOrder
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.app.pages.root.SpecialRouterCtl
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object FieldList {

  final case class Props(config              : ProjectConfig,
                         filterDead          : FilterDead,
                         selected            : Option[FieldId],
                         select              : Option[FieldId ~=> Callback],
                         pw                  : ProjectWidgets.NoCtx,
                         updateOrder         : Reusable[FieldUpdateOrder => Callback],
                         enabled             : Enabled,
                         onClickAnywhere     : Option[Reusable[Callback]],
                         //usage               : LiveDeadStatMap[FieldId, Int],
                         router              : SpecialRouterCtl,
                        ) {

    def fieldIds: Vector[FieldId] =
      filterDead match {
        case HideDead => config.liveOrderedFieldIds
        case ShowDead => config.fields.order
      }

    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def fieldKey(f: FieldId): Key =
    f match {
      case id: CustomFieldId             => id.value
      case StaticField.NormalAltStepTree => "n"
      case StaticField.ExceptionStepTree => "e"
      case StaticField.ImplicationGraph  => "i"
      case StaticField.StepGraph         => "s"
    }

  final class Backend($: BackendScope[Props, Unit]) {

    private val dnd = DragToReorderFeature[FieldId](
      getData             = $.props.map(_.fieldIds),
      updateData          = u => $.props.flatMap(_.updateOrder(FieldUpdateOrder(u.source, u.relPos))),
      updateUI            = $.forceUpdate,
      dragOutsideToRemove = false,
      addKeysToChildren   = false,
    )

    private val tableHeader =
      <.thead(
        <.tr(
          <.th(^.width := "1px"),
          <.th("Name"),
          <.th("Type"),
          <.th("Details"),
          <.th("Usage", *.fieldListTableUsage),
        ))

    private val _dragHandle: Enabled => Live => VdomTag =
      Enabled.memo(e =>
        Live.memo(l =>
          DragToReorderFeature.dragHandle(*.dragHandle((e, l)))))

    private def dragHandle(item: DragToReorderFeature.Item[Any], enabled: Enabled, live: Live): TagMod =
      (enabled & Disabled.when(live is Dead)) match {
        case Enabled  => _dragHandle(Enabled)(live)(item.source)
        case Disabled => _dragHandle(Disabled)(live)
      }

    def render(p: Props): VdomNode = {

      val modificationEnabled: Enabled =
        p.enabled & Enabled.when(p.select.isDefined)

      val dragInProgress: Boolean =
        DragToReorderFeature.dragInProgress()

      def rowState(id: FieldId): *.RowState =
        if (dragInProgress)
          *.RowState.Dragging
        else if (p.selected.exists(_ ==* id))
          *.RowState.Selected
        else if (modificationEnabled is Disabled)
          *.RowState.Disabled
        else
          *.RowState.Enabled

      def renderField(item: DragToReorderFeature.Item[FieldId]) = {
        val id = item.data
        val field = p.config.fields.need(id)
        val live = field.live(p.config)

        <.tr(
          *.fieldListTableRow(((rowState(id), item.status), live)),
          item.target,
          ^.key := fieldKey(id),
          ^.onClick -->? p.select.map(_(id)),

          <.td(
            *.fieldListTableDrag,
            dragHandle(item, modificationEnabled, live)),

          <.td(
            *.fieldListTableName(live),
            p.config.fieldName(id)),

          <.td(
            field.fieldType.name),

          <.td(
            "TODO"),

          <.td(
            *.fieldListTableUsage,
            "TODO"),
        )
      }

      <.table(
        *.fieldListTable,
        p.onClickAnywhere.whenDefined(^.onClick --> _),
        tableHeader,
        <.tbody(
          dnd.container,
          dnd.items().toVdomArray(renderField)))
    }
  }

  val Component = ScalaComponent.builder[Props]("FieldList")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}