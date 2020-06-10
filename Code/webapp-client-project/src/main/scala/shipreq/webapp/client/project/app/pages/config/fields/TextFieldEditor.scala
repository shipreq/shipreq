package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.PotentialChange
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomTextFieldGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ReqTypeRulesEditor
import shipreq.webapp.base.ui.widgets.Form

object TextFieldEditor {
  import DataImplicits._

  final case class Props(state     : StateSnapshot[State],
                         cfg       : ProjectConfig,
                         filterDead: FilterDead,
                         enabled   : Enabled) {

    val validatorState: DataValidators.field.State =
      state.value.validatorState(cfg)

    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(idOption: Option[CustomField.Text.Id],
                         name    : String,
                         rules   : ReqTypeRulesEditor.NoDefault.State) {

    def validatorState(cfg: ProjectConfig): DataValidators.field.State =
      DataValidators.field.State.from(idOption, cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyFields] = {
      val vs = validatorState(cfg)

      val pass1 =
        for {
          a <- PotentialChange.fromDisjunction(DataValidators.field.name(vs).unnamed(name).leftMap(_ => ()))
          b <- PotentialChange.needFromOption(rules.validation(cfg.reqTypes).resultWhenValidI)
        } yield (a, b)

      pass1.flatMap { case (name, rules) =>
        idOption match {

          case Some(id) =>
            val old = cfg.fields.custom(id)
            val b = CustomTextFieldGD.valueBuilder()
            b.addIfChanged(CustomTextFieldGD.Name             )(old.name             , name)
            b.addIfChanged(CustomTextFieldGD.FieldReqTypeRules)(old.fieldReqTypeRules, rules)
            PotentialChange.fromOption(b.nev()).map { newValues =>
              UpdateConfigCmd.CustomFieldUpdateText(id, newValues)
            }

          case None =>
            val cmd = UpdateConfigCmd.CustomFieldCreateText(name, rules)
            PotentialChange.Success(cmd)
        }
      }
    }
  }

  object State {
    def empty: State =
      apply(None, "", ReqTypeRulesEditor.State.empty)

    def init(id: CustomField.Text.Id, cfg: ProjectConfig): State = {
      val f = cfg.fields.custom(id)
      apply(Some(id), f.name, ReqTypeRulesEditor.State.init(cfg, f.fieldReqTypeRulesByResolution))
    }

    def init(id: Option[CustomField.Text.Id], cfg: ProjectConfig): State =
      id.fold(empty)(init(_, cfg))
  }

  // ===================================================================================================================

  private def render(p: Props): VdomNode = {

    val nameField =
      Form.Field.text
        .withLabel("Name")
        .withState(p.state.zoomStateL(State.name))
        .withValidator(DataValidators.field.name.unnamedFn(p.validatorState))
        .withEnabledAndAutoFocus(p.enabled)

    val rules =
      ReqTypeRulesEditor.NoDefault.Component(
        ReqTypeRulesEditor.Props.noDefaults(
          state         = p.state.zoomStateL(State.rules),
          reqTypes      = p.cfg.reqTypes,
          filterDead    = p.filterDead,
          enabled       = p.enabled))

    <.div(
      Form(nameField)(ValidationUX.Full),
      rules)
  }

  implicit val reusabilityState: Reusability[State] = Reusability.derive
  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}