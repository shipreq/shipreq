package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import scala.collection.immutable.ArraySeq
import shipreq.base.util.PotentialChange
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomImpFieldGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.widgets.{Dropdown, Form}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ReqTypeRulesEditor

object ImpFieldEditor {
  import DataImplicits._

  final case class Props(state     : StateSnapshot[State],
                         cfg       : ProjectConfig,
                         filterDead: FilterDead,
                         enabled   : Enabled,
                         router    : Routes.RouterCtl) {

    private lazy val legalReqTypes: Set[ReqTypeId] =
      cfg.reqTypes.liveIds -- cfg.fields.customImpFields.iterator.map(_.reqTypeId)

    private[ImpFieldEditor] lazy val reqTypeItems: ArraySeq[Dropdown.Item[ReqTypeId]] =
      cfg.reqTypes.sortIdsByMnemonic(legalReqTypes)
        .map(rt => Dropdown.Item(rt.mnemonic.value, PlainText.reqTypeFull(rt), rt.reqTypeId))
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
    def idOption: Option[CustomField.Implication.Id]
    val rules: ReqTypeRulesEditor.NoDefault.State

    final def validatorState(cfg: ProjectConfig): DataValidators.field.State =
      DataValidators.field.State.from(idOption, cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields]
  }

  object State {

    @Lenses
    final case class ForUpdate(id   : CustomField.Implication.Id,
                               rules: ReqTypeRulesEditor.NoDefault.State) extends State {

      override def idOption = Some(id)

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val pass1 =
          PotentialChange.needFromOption(rules.validation(cfg.reqTypes).resultWhenValidI)

        pass1.flatMap { rules =>
          val old = cfg.fields.custom(id)
          val b = CustomImpFieldGD.valueBuilder()
          b.addIfChanged(CustomImpFieldGD.FieldReqTypeRules)(old.fieldReqTypeRules, rules)
          PotentialChange.fromOption(b.nev()).map { newValues =>
            UpdateConfigCmd.CustomFieldUpdateImp(id, newValues)
          }
        }
      }
    }

    @Lenses
    final case class ForCreate(reqType: Option[ReqTypeId],
                               rules  : ReqTypeRulesEditor.NoDefault.State) extends State {

      override def idOption = None

      override def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
        val vs = validatorState(cfg)

        val pass1 =
          for {
            a <- PotentialChange.fromDisjunction(DataValidators.field.impSource(vs).unnamed(reqType).leftMap(_ => ()))
            b <- PotentialChange.needFromOption(rules.validation(cfg.reqTypes).resultWhenValidI)
          } yield (a, b)

        pass1.flatMap { case (reqTypeId, rules) =>
          val cmd = UpdateConfigCmd.CustomFieldCreateImp(reqTypeId, rules)
          PotentialChange.Success(cmd)
        }
      }
    }

    val rules: Lens[State, ReqTypeRulesEditor.NoDefault.State] =
      Lens[State, ReqTypeRulesEditor.NoDefault.State](_.rules)(r => {
        case s: ForCreate => s.copy(rules = r)
        case s: ForUpdate => s.copy(rules = r)
      })

    def initCreate: ForCreate =
      ForCreate(None, ReqTypeRulesEditor.State.empty)

    def initUpdate(id: CustomField.Implication.Id, cfg: ProjectConfig): ForUpdate = {
      val f = cfg.fields.custom(id)
      ForUpdate(id, ReqTypeRulesEditor.State.init(cfg, f.fieldReqTypeRulesByResolution))
    }

    def init(id: Option[CustomField.Implication.Id], cfg: ProjectConfig): State =
      id.fold[State](initCreate)(initUpdate(_, cfg))
  }

  // ===================================================================================================================

  private def render(p: Props): VdomNode =
    if (p.isPossible) {

      val rules =
        ReqTypeRulesEditor.NoDefault.Component(
          ReqTypeRulesEditor.Props.noDefaults(
            state      = p.state.zoomStateL(State.rules),
            reqTypes   = p.cfg.reqTypes,
            filterDead = p.filterDead,
            enabled    = p.enabled))

      p.state.value match {

        case _: State.ForUpdate =>
          <.div(rules)

        case s: State.ForCreate =>

          val sourceSelect =
            Dropdown.Props.Optional[ReqTypeId](
              items    = p.reqTypeItems,
              selected = s.reqType.flatMap(p.cfg.reqTypes.get).map(_.mnemonic.value),
              enabled  = p.enabled)(
              onChange = o => p.state.setState(s.copy(Some(o.value)))
            ).render

          val sourceField =
            Form.Field.ofEditor(sourceSelect)
              .inline
              .withLabel(FieldNames.impFieldSource)
              .withValidated(DataValidators.field.impSource.unnamedFn(p.validatorState)(s.reqType))
              .withValidationUX(ValidationUX.Highlight)
              .withEnabled(p.enabled)

          <.div(Form(sourceField)(ValidationUX.Full), rules)
      }
    } else {

      // Not possible
      val msg = Shared.noSourcesMsg(FieldNames.impFieldSource, "implication", p.router, Routes.Page.CfgReqTypes)
      <.div(msg) // Wrap in div for tests
    }

  implicit val reusabilityStateC: Reusability[State.ForCreate] = Reusability.derive
  implicit val reusabilityStateU: Reusability[State.ForUpdate] = Reusability.derive
  implicit val reusabilityState : Reusability[State          ] = Reusability.derive
  implicit val reusabilityProps : Reusability[Props          ] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}