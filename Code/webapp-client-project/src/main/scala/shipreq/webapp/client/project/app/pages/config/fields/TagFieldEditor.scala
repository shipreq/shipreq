package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import scala.collection.immutable.ArraySeq
import shipreq.base.util.PotentialChange
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomTagFieldGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.widgets.{Dropdown, Form}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.{ProjectWidgets, ReqTypeRulesEditor}

object TagFieldEditor {
  import DataImplicits._

  private def legalDefaultIterator(state: State, cfg: ProjectConfig): Iterator[ApplicableTagId] = {
    val tagGroupIdOption: Option[TagGroupId] =
      state match {
        case s: State.ForUpdate => Some(cfg.fields.custom(s.id).tagId)
        case s: State.ForCreate => s.tagId
      }

    tagGroupIdOption match {
      case None     => Iterator.empty
      case Some(id) =>
        val flatRows = cfg.tags.flatRowsWithRoot(id, HideDead)
        MutableArray(flatRows.iterator.map(_.tag).filterSubType[ApplicableTag])
          .sortBy(_.name)
          .iterator
          .map(_.id)
    }
  }

  final case class Props(state     : StateSnapshot[State],
                         cfg       : ProjectConfig,
                         filterDead: FilterDead,
                         enabled   : Enabled,
                         pw        : ProjectWidgets.NoCtx,
                         router    : Routes.RouterCtl) {

    lazy val legalDefaults: ArraySeq[ApplicableTagId] =
      legalDefaultIterator(state.value, cfg).to(ArraySeq).distinct

    private lazy val legalTagGroups: Set[TagGroupId] =
      cfg.tags.liveTagGroupIds -- cfg.fields.customTagFields.iterator.map(_.tagId).filterSubType[TagGroupId]

    private[TagFieldEditor] lazy val reqTypeItems: ArraySeq[Dropdown.Item[TagGroupId]] =
      MutableArray(legalTagGroups.iterator.map(cfg.tags.needTagGroup))
        .sortBy(_.name)
        .iterator
        .map(t => Dropdown.Item(t.id.value.toString, t.name, t.id))
        .to(ArraySeq)

    val isPossible: Boolean =
      state.value match {
        case _: State.ForUpdate => true
        case _: State.ForCreate => reqTypeItems.nonEmpty
      }

    lazy val validatorState: DataValidators.field.State =
      state.value.validatorState(cfg)

    @inline def render: VdomElement = Component(this)
  }

  sealed trait State {
    def idOption: Option[CustomField.Tag.Id]

    val rules: ReqTypeRulesEditor.ApplicableTagDefault.State

    final def validatorState(cfg: ProjectConfig): DataValidators.field.State =
      DataValidators.field.State.from(idOption, cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields]
  }

  object State {

    @Lenses
    final case class ForUpdate(id   : CustomField.Tag.Id,
                               rules: ReqTypeRulesEditor.ApplicableTagDefault.State) extends State {

      override def idOption = Some(id)

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val ds = legalDefaultIterator(this, cfg).toSet

        val pass1 =
          PotentialChange.needFromOption(rules.validation(cfg.reqTypes).resultWhenValid(ds))

        pass1.flatMap { rules =>
          val old = cfg.fields.custom(id)
          val b = CustomTagFieldGD.valueBuilder()
          b.addIfChanged(CustomTagFieldGD.FieldReqTypeRules)(old.fieldReqTypeRules, rules)
          PotentialChange.fromOption(b.nev()).map { newValues =>
            UpdateConfigCmd.CustomFieldUpdateTag(id, newValues)
          }
        }
      }
    }

    @Lenses
    final case class ForCreate(tagId: Option[TagGroupId],
                               rules: ReqTypeRulesEditor.ApplicableTagDefault.State) extends State {

      override def idOption = None

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val vs = validatorState(cfg)
        val ds = legalDefaultIterator(this, cfg).toSet

        val pass1 =
          for {
            a <- PotentialChange.fromDisjunction(DataValidators.field.tagGroup(vs).unnamed(tagId).leftMap(_ => ()))
            b <- PotentialChange.needFromOption(rules.validation(cfg.reqTypes).resultWhenValid(ds))
          } yield (a, b)

        pass1.flatMap { case (tagId, rules) =>
          val cmd = UpdateConfigCmd.CustomFieldCreateTag(tagId, rules)
          PotentialChange.Success(cmd)
        }
      }
    }

    val rules: Lens[State, ReqTypeRulesEditor.ApplicableTagDefault.State] =
      Lens[State, ReqTypeRulesEditor.ApplicableTagDefault.State](_.rules)(r => {
        case s: ForCreate => s.copy(rules = r)
        case s: ForUpdate => s.copy(rules = r)
      })

    def initCreate: ForCreate =
      ForCreate(None, ReqTypeRulesEditor.State.empty)

    def initUpdate(id: CustomField.Tag.Id, cfg: ProjectConfig): ForUpdate = {
      val f = cfg.fields.custom(id)
      ForUpdate(id, ReqTypeRulesEditor.State.init(cfg, f.fieldReqTypeRulesByResolution))
    }

    def init(id: Option[CustomField.Tag.Id], cfg: ProjectConfig): State =
      id.fold[State](initCreate)(initUpdate(_, cfg))
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomNode =
      if (p.isPossible) {

        val renderDefault: ApplicableTagId ~=> VdomNode =
          Reusable.implicitly(p.pw).map(pw => id => pw.tagSimple(id, includeDesc = true))

        val rules =
          ReqTypeRulesEditor.ApplicableTagDefault.Component(
            ReqTypeRulesEditor.Props(
              state         = p.state.zoomStateL(State.rules),
              reqTypes      = p.cfg.reqTypes,
              renderDefault = renderDefault,
              defaults      = p.legalDefaults,
              filterDead    = p.filterDead,
              enabled       = p.enabled))

        p.state.value match {

          case _: State.ForUpdate =>
            <.div(rules)

          case s: State.ForCreate =>

            val sourceSelect =
              Dropdown.Props.Optional[TagGroupId](
                items    = p.reqTypeItems,
                selected = s.tagId.map(p.cfg.tags.needTagGroup(_).name),
                enabled  = p.enabled)(
                onChange = o => p.state.setState(s.copy(Some(o.value)))
              ).render

            val sourceField =
              Form.Field.ofEditor(sourceSelect)
                .inline
                .withLabel(FieldNames.tagFieldSource)
                .withValidated(DataValidators.field.tagGroup.unnamedFn(p.validatorState)(s.tagId))
                .withValidationUX(ValidationUX.Highlight)
                .withEnabled(p.enabled)

            <.div(Form(sourceField)(ValidationUX.Full), rules)
        }
      } else {

        // Not possible
        val msg = Shared.noSourcesMsg(FieldNames.tagFieldSource, "tag", p.router, Routes.Page.CfgTags)
        <.div(msg) // Wrap in div for tests
      }
  }

  implicit val reusabilityStateC: Reusability[State.ForCreate] = Reusability.derive
  implicit val reusabilityStateU: Reusability[State.ForUpdate] = Reusability.derive
  implicit val reusabilityState : Reusability[State          ] = Reusability.derive
  implicit val reusabilityProps : Reusability[Props          ] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}