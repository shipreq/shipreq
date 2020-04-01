package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.DragToReorderFeature
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd.FieldUpdateOrder
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.Usage
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
                         usage               : Usage,
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
          <.th("Usage", *.fieldListTableUsage(Live)),
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

    private val na = TagMod(
      *.fieldListTableUsage(Dead),
      <.span(*.`N/A`, "–"))

    private val ruleSep =
      <.span(*.detailRuleSep, "—")

    private def renderDetailRule(key: TagMod, value: VdomNode, validity: Validity = Valid): VdomNode =
      <.div(*.detailRule(validity),
        <.span(*.detailRuleKey, key),
        ruleSep,
        value)

    private val impossible: Impossible => VdomNode =
      _.impossible

    private def renderDetailRules[A](p: Props, rules0: FieldReqTypeRules.ByResolution[A])(renderDefault: A => VdomNode): VdomNode = {

      val rules: FieldReqTypeRules.ByResolution[A] =
        p.filterDead match {
          case ShowDead => rules0
          case HideDead => rules0.filterLiveReqTypes(p.config.reqTypes)
        }

      val renderRes: FieldReqTypeRules.Resolution[A] => VdomNode = {
        case FieldReqTypeRules.Resolution.NotApplicable => "Not applicable"
        case FieldReqTypeRules.Resolution.Mandatory     => "Mandatory"
        case FieldReqTypeRules.Resolution.Optional      => "Optional"
        case FieldReqTypeRules.Resolution.DefaultTo(a)  => <.span("Default to ", renderDefault(a))
      }

      val validity =
        Invalid.when(
          rules.otherwise.applicability.is(NotApplicable) &&
          rules.perRes.valuesIterator.forall(_.forall(p.config.reqTypes.live(_, Dead) is Dead)))

      if (rules.perRes.isEmpty) {
        renderDetailRule("All", renderRes(rules.otherwise), validity)

      } else {

        val specific: TagMod =
          MutableArray {
            rules.perRes.iterator.map { case (res, ids) =>

              val types =
                MutableArray(ids.iterator.flatMap(p.config.reqTypes.get)).sortBy(_.mnemonic.value)

              val sortKey: String =
                types.mkString(", ")

              val key: TagMod =
                types
                  .iterator
                  .map[VdomNode] { rt =>
                    val m = rt.mnemonic.value
                    if (rt.live is Live) m else <.span(*.fieldListDetailDead, m)
                  }.mkTagMod(", ")

              val value: VdomNode =
                renderRes(res)

              (sortKey, renderDetailRule(key, value))
            }
          }.sortBy(_._1).iterator.map(_._2).toTagMod

        val other =
          renderDetailRule("Other", renderRes(rules.otherwise), validity)

        <.div(specific, other)
      }
    }

    private val detailAllVisible = renderDetailRule("All", "Visible")
    private val detailUcOptional = renderDetailRule(StaticReqType.UseCase.mnemonic.value, "Optional")
    private val detailUcVisible  = renderDetailRule(StaticReqType.UseCase.mnemonic.value, "Visible")

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

        val detail =
          field match {
            case StaticField.ImplicationGraph  => detailAllVisible
            case StaticField.NormalAltStepTree => detailUcOptional
            case StaticField.ExceptionStepTree => detailUcOptional
            case StaticField.StepGraph         => detailUcVisible
            case f: CustomField.Text           => renderDetailRules(p, f.fieldReqTypeRulesByResolution)(impossible)
            case f: CustomField.Implication    => renderDetailRules(p, f.fieldReqTypeRulesByResolution)(impossible)
            case f: CustomField.Tag            => renderDetailRules(p, f.fieldReqTypeRulesByResolution)(p.pw.tagSimple(_, includeDesc = true))
          }

        val usage: TagMod =
          id match {
            case _: StaticField =>
              na
            case id: CustomFieldId =>
              TagMod(
                *.fieldListTableUsage(live),
                p.usage.fieldLink(id, p.filterDead))
          }

        val select: ReactEvent => Option[Callback] =
          e => p.select.map(_(id).asEventDefault(e).void)

        <.tr(
          *.fieldListTableRow(((rowState(id), item.status), live)),
          item.target,
          ^.key := fieldKey(id),
          ^.onClick ==>? select,

          <.td(
            *.fieldListTableDrag(live),
            dragHandle(item, modificationEnabled, live)),

          <.td(
            *.fieldListTableName(live),
            p.config.fieldName(id)),

          <.td(
            *.fieldListTableCell(live),
            field.fieldType.name),

          <.td(
            *.fieldListTableCell(live),
            detail),

          <.td(
            usage),
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