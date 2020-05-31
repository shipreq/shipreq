package shipreq.webapp.client.project.app.pages.config.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomIssueTypeGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.client.project.lib.DataReusability._

private[issues] object CustomIssueTypeEditor {

  final case class Props(state  : StateSnapshot[State],
                         project: ProjectConfig,
                         enabled: Enabled,
                        ) {

    val validatorState: DataValidators.customIssueType.State =
      state.value.validatorState(project)

    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(source: Option[CustomIssueType],
                         key   : String,
                         desc  : String,
                        ) {

    def validatorState(cfg: ProjectConfig): DataValidators.customIssueType.State =
      DataValidators.customIssueType.State.fromConfig(source.map(_.id), cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyCustomIssueTypes] = {
      val vs = validatorState(cfg)

      val validated =
        DataValidators.customIssueType.all(vs)(
          (key, desc))

      PotentialChange
        .fromDisjunction(validated.leftMap(_ => ()))
        .flatMap { case (key, desc) =>
          source match {

            case Some(s) =>
              val b = CustomIssueTypeGD.valueBuilder()
              b.addIfChanged(CustomIssueTypeGD.Key )(s.key , key)
              b.addIfChanged(CustomIssueTypeGD.Desc)(s.desc, desc)

              PotentialChange.fromOption(b.nev()).map { newValues =>
                UpdateConfigCmd.CustomIssueTypeUpdate(s.id, newValues)
              }

            case None =>
              val cmd = UpdateConfigCmd.CustomIssueTypeCreate(key, desc)
              PotentialChange.Success(cmd)
          }
        }
    }
  }

  object State {
    def initById(id: Option[CustomIssueTypeId], customIssueTypes: CustomIssueTypeIMap): Option[State] =
      id match {
        case Some(i) => initById(i, customIssueTypes)
        case None    => Some(initNew)
      }

    def initById(id: CustomIssueTypeId, customIssueTypes: CustomIssueTypeIMap): Option[State] =
      customIssueTypes.get(id).map(init(_))

    def init(rtOption: Option[CustomIssueType]): State =
      rtOption.fold(initNew)(init(_))

    def init(i: CustomIssueType): State =
      State(
        source = Some(i),
        key    = i.key.value,
        desc   = i.desc.getOrElse(""),
      )

    def initNew: State =
      State(
        source = None,
        key    = "",
        desc   = "",
      )
  }

  implicit val reusabilityState: Reusability[State ] = Reusability.byRef || Reusability.derive
  implicit val reusabilityProps: Reusability[Props ] = Reusability.byRef || Reusability.derive

  // ===================================================================================================================

  private implicit def vux = ValidationUX.Full

  private def render(p: Props): VdomNode = {

    val key =
      Form.Field.text
        .withLabel(FieldNames.hashRefKey)
        .withState(p.state.zoomStateL(State.key))
        .withValidator(DataValidators.customIssueType.key.unnamedFn(p.validatorState))
        .withEnabledAndAutoFocus(p.enabled)

    val desc =
      Form.Field.text
        .withEditor(AutosizeTextarea.editor)
        .withLabel(FieldNames.desc)
        .withState(p.state.zoomStateL(State.desc))
        .withValidator(DataValidators.customIssueType.desc.unnamedFn(p.validatorState))
        .withEnabled(p.enabled)

    Form(key, desc)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}