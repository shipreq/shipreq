package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data._
import shipreq.webapp.base.ui.semantic.{Icon, Input, Message, Segment, Table}
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

private[fields] object DerivativeTagsEditor {

  final case class Props(tagGroupId: Option[TagGroupId],
                         filterDead: FilterDead,
                         cfg       : ProjectConfig,
                         pw        : ProjectWidgets.NoCtx,
                         state     : StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  // ===================================================================================================================

  @Lenses
  final case class State(on   : On,
                         rules: DerivativeTagRuleEditor.State) {

    def potentialChange(cfg: ProjectConfig, fieldId: Option[CustomField.Tag.Id]): PotentialChange[Unit, DerivativeTags] = {
      val field          = fieldId.map(cfg.fields.custom(_))
      val group          = field.map(_.tagId)
      val oldOption      = field.map(_.derivativeTags)
      val old            = oldOption.getOrElse(DerivativeTags.emptyDisabled)
      val groupTags      = cfg.tags.tagGroupTagsFDV(group)
      val validatedRules = rules.validate(groupTags)
      val pcRules        = rules.potentialChange(validatedRules)
      val pcEnabled      = PotentialChange.Success(Enabled.when(on is On)).ignoreOption(oldOption.map(_.enabled))
      pcEnabled.merge(pcRules)(old.enabled, old.rules)(DerivativeTags.apply)
    }
  }

  object State {
    def empty: State =
      State(
        on = Off,
        rules = DerivativeTagRuleEditor.State.empty)

    def init(dt: DerivativeTags, group: TagGroupId, tags: Tags): State =
      State(
        on = On when dt.enabled.is(Enabled),
        rules = DerivativeTagRuleEditor.State.init(dt, group, tags))
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {

    private val ssRuleEditor =
      StateSnapshot.withReuse.zoomL(State.rules).prepareViaProps($)(_.state)

    private val container      = Segment.tag
    private val matrixCellSame = TagMod(*.derivativeTagMatrixSame)
    private val matrixCellNone = TagMod(*.derivativeTagMatrixNone, "(both)", ^.title := "These two tags will not be combined.\nThey will both remain as is.")

    private val featureDesc = {
      val style = Message.Style(Message.Type.Info)
      val desc = TagMod(
        "Keeping tags up-to-date is normally tedious and error prone as the volume of requirements increases.",
        " By enabling derivative tags, ShipReq will automatically assign tags to requirements based on the tags of related requirements.",
        " You specify how pairs of tags should be combined and ShipReq does the rest.",
      )
      Message(style, Icon.Sitemap, desc)
    }

    private def renderToggle(p: Props, enabled: Enabled): VdomNode =
      Input.Checkbox.fromStateSnapshot(
        ss      = p.state.zoomStateL(State.on),
        label   = "Derivative tags",
        enabled = enabled,
      )

    private def renderMatrix(p: Props, tagGroupId: TagGroupId): Option[VdomNode] = {
      import p.pw

      val s         = p.state.value
      val groupTags = p.cfg.tags.tagGroupTagsFDV(tagGroupId)
      val tags      = groupTags(p.filterDead)
      val vali      = s.rules.validate(groupTags)
      val rules     = DerivativeTags(Enabled, vali.validRules)
      val head      = VdomArray.empty()
      val body      = VdomArray.empty()

      Option.unless(tags.isEmpty) {

        for (rowTag <- tags.tags) {
          val rowAbb     = tags.abbreviations(rowTag)
          val rowKey     = ^.key := rowTag.id.value
          val rowTagVdom = pw.tagWithCustomName(rowTag, rowAbb)
          head += <.th(rowKey, rowTagVdom)

          val row = VdomArray.empty()
          row += <.td(^.key := "h", pw.tagSimple(rowTag, includeDesc = true))
          for (colTag <- tags.tags) {

            val content: TagMod =
              if (colTag.id ==* rowTag.id)
                TagMod(matrixCellSame, rowTagVdom)
              else {
                rules.combineOption(rowTag.id, colTag.id) match {
                  case Some(tagId) =>
                    val tag = p.cfg.tags.needApplicableTag(tagId)
                    val abb = tags.abbreviations(tag)
                    pw.tagWithCustomName(tag, abb)
                  case None =>
                    matrixCellNone
                }
              }

            row += <.td(^.key := colTag.id.value, content)
          }
          body += <.tr(rowKey, row)
        }

        Table.celledCompactDefinition(
          <.thead(<.tr(<.th, head)),
          <.tbody(body))
      }
    }

    private def renderRuleEditor(p: Props, tagGroupId: TagGroupId): VdomNode = {
      val ss = ssRuleEditor(p.state.value)
      DerivativeTagRuleEditor.Props(ss, tagGroupId, p.cfg.tags).render
    }

    private def renderBeforeGroupChosen(p: Props): VdomNode =
      container(
        renderToggle(p, Disabled),
        featureDesc)

    private def renderWhenOff(p: Props): VdomNode =
      container(
        renderToggle(p, Enabled),
        featureDesc)

    private def renderWhenOn(p: Props, id: TagGroupId): VdomNode = {
      val tag = p.cfg.tags.needTagGroup(id)
      container(
        renderToggle(p, Enabled),
        featureDesc,
        <.div(tag.name, " combination rules:"),
        renderMatrix(p, id).whenDefined,
        renderRuleEditor(p, id))
    }

    def render(p: Props): VdomNode =
      p.tagGroupId match {
        case Some(tagGroupId) =>
          p.state.value.on match {
            case On  => renderWhenOn(p, tagGroupId)
            case Off => renderWhenOff(p)
          }
        case None =>
          renderBeforeGroupChosen(p)
      }
  }

  // ===================================================================================================================

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive
  implicit val reusabilityState: Reusability[State] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
