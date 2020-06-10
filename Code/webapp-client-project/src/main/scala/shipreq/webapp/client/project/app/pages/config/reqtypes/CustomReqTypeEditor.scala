package shipreq.webapp.client.project.app.pages.config.reqtypes

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.CustomReqTypeGD
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.protocol.websocket.UpdateConfigCmd
import shipreq.webapp.base.ui.AutosizeTextarea
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.client.project.app.Style.{reqTypeConfig => *}
import shipreq.webapp.client.project.lib.DataReusability._

private[reqtypes] object CustomReqTypeEditor {

  final case class Props(filterDead: FilterDead,
                         state     : StateSnapshot[State],
                         project   : ProjectConfig,
                         enabled   : Enabled,
                        ) {

    val validatorState: DataValidators.reqType.State =
      state.value.validatorState(project)

    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(source  : Option[CustomReqType],
                         mnemonic: String,
                         name    : String,
                         desc    : String,
                         imp     : Mandatory,
                        ) {

    def validatorState(cfg: ProjectConfig): DataValidators.reqType.State =
      DataValidators.reqType.State.fromConfig(source.map(_.id), cfg)

    def updateCmd(cfg: ProjectConfig): PotentialChange[Unit, UpdateConfigCmd.ToModifyReqTypes] = {
      val vs = validatorState(cfg)

      val validated =
        DataValidators.reqType.all(vs)(
          (mnemonic, name, desc, imp))

      PotentialChange
        .fromDisjunction(validated.leftMap(_ => ()))
        .flatMap { case (mnemonic, name, desc, imp) =>
          source match {

            case Some(s) =>
              val b = CustomReqTypeGD.valueBuilder()
              b.addIfChanged(CustomReqTypeGD.Mnemonic   )(s.mnemonic   , mnemonic)
              b.addIfChanged(CustomReqTypeGD.Name       )(s.name       , name)
              b.addIfChanged(CustomReqTypeGD.Description)(s.description, desc)
              b.addIfChanged(CustomReqTypeGD.Implication)(s.implication, imp)

              PotentialChange.fromOption(b.nev()).map { newValues =>
                UpdateConfigCmd.CustomReqTypeUpdate(s.id, newValues)
              }

            case None =>
              val cmd = UpdateConfigCmd.CustomReqTypeCreate(mnemonic, name, desc, imp)
              PotentialChange.Success(cmd)
          }
        }
    }
  }

  object State {
    def initById(id: Option[CustomReqTypeId], reqTypes: ReqTypes): Option[State] =
      id match {
        case Some(i) => initById(i, reqTypes)
        case None    => Some(initNew)
      }

    def initById(id: CustomReqTypeId, reqTypes: ReqTypes): Option[State] =
      reqTypes.custom.get(id).map(init(_))

    def init(rtOption: Option[CustomReqType]): State =
      rtOption.fold(initNew)(init(_))

    def init(rt: CustomReqType): State =
      State(
        source   = Some(rt),
        mnemonic = rt.mnemonic.value,
        name     = rt.name,
        desc     = rt.description.getOrElse(""),
        imp      = rt.implication,
      )

    def initNew: State =
      State(
        source  = None,
        mnemonic = "",
        name     = "",
        desc     = "",
        imp      = Optional,
      )
  }

  implicit val reusabilityProps: Reusability[Props ] = Reusability.byRef || Reusability.derive
  implicit val reusabilityState: Reusability[State ] = Reusability.byRef || Reusability.derive

  // ===================================================================================================================

  private implicit def vux = ValidationUX.Full

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val mnemonic =
        Form.Field.text
          .withLabel(FieldNames.mnemonic)
          .withState(p.state.zoomStateL(State.mnemonic))
          .withValidator(DataValidators.reqType.mnemonic.unnamedFn(p.validatorState))
          .withEnabledAndAutoFocus(p.enabled)
          .modEditor(f => e => f(TagMod(e, *.editorMnemonic)))

      def pastMnemonicContent =
        s.source.map { rt =>
          <.div(
            *.editorPastMnemonics,
            Shared.renderOldMnemonics(rt))
        }

      def pastMnemonics =
        Form.Field
          .ofEditor(pastMnemonicContent.whenDefined)
          .withLabel(FieldNames.pastMnemonics)
          .withEnabled(p.enabled)

      val mnemonics =
        p.filterDead match {
          case HideDead => mnemonic
          case ShowDead => Form.Field.two(mnemonic, pastMnemonics)
        }

      val name =
        Form.Field.text
          .withLabel(FieldNames.name)
          .withState(p.state.zoomStateL(State.name))
          .withValidator(DataValidators.reqType.name.unnamedFn(p.validatorState))
          .withEnabled(p.enabled)

      val imp =
        Form.Field.booleanSelect(Mandatory)(_.toText)
          .withLabel(Shared.implication)
          .withState(p.state.zoomStateL(State.imp))
          .withEnabled(p.enabled)

      val desc =
        Form.Field.text
          .withEditor(AutosizeTextarea.editor)
          .withLabel(FieldNames.desc)
          .withState(p.state.zoomStateL(State.desc))
          .withValidator(DataValidators.reqType.desc.unnamedFn(p.validatorState))
          .withEnabled(p.enabled)

      Form(mnemonics, name, imp, desc)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}