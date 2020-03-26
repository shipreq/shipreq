package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic.{Form, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.{widgets => *}

@UsesSemanticUiManually
object ApplicableReqTypeEditor {

  final case class Props(state     : StateSnapshot[State],
                         previous  : ApplicableReqTypes,
                         reqTypes  : ReqTypes,
                         filterDead: FilterDead,
                         enabled   : Enabled) {

    @inline def render: VdomElement = Component(this)

    lazy val deadPrevious = previous.filterReqTypes(Dead, reqTypes)
    lazy val validator    = DataValidators.reqTypeSeqStr(reqTypes).unnamed
    lazy val validated    = validator(state.value.text)

    lazy val reqTypesInText: Set[String] =
      validator.corrector(state.value.text).iterator.flatMap(_.toOption).toSet
  }

  type State = DropdownAndTextEditor.State[Applicability]

  object State {

    def empty: State =
      DropdownAndTextEditor.State(
        selected = ApplicableReqTypes.empty.applicability,
        text     = "")

    def init(art: ApplicableReqTypes, reqTypes: ReqTypes): State =
      DropdownAndTextEditor.State(
        selected = art.applicability,
        text     = reqTypes.makeSeqStr(art.filterReqTypes(Live, reqTypes).reqTypes))
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.BackendI {

    private val label = <.label(UiText.FieldNames.applicableReqTypes)

    private val renderItem: Applicability => TagMod = {
      case Applicable    => "Whitelist"
      case NotApplicable => "Blacklist"
    }

    private val items: NonEmptyVector[Applicability] =
      NonEmptyVector(Applicable, NotApplicable)

    private val inputDomRef = Ref[html.Input]

    private lazy val inputTagMod: TagMod =
      TagMod(
        ^.onBlur --> autoCompleteBlur,
        ^.placeholder := "Req types...",
        ^.spellCheck := false,
      )

    private val clear =
      <.div(^.clear.right)

    def render(p: Props): VdomNode = {
      val s = p.state.value

      val error: Option[VdomTag] =
        GeneralTheme.renderSimpleInvalidity(p.validated)

      val input =
        DropdownAndTextEditor.Props(
          items           = items,
          renderItem      = renderItem,
          state           = p.state,
          textLiveCorrect = DataValidators.reqTypeSeqStr.unnamed.corrector.live,
          enabled         = p.enabled,
          dropdownTagMod  = *.applicableReqTypesDropdown,
          inputTagMod     = inputTagMod,
          inputDomRef     = inputDomRef,
        ).render

      val dead = {
        Option.when(p.filterDead.is(ShowDead) && p.deadPrevious.nonEmpty && p.deadPrevious.applicability ==* s.selected)(
          <.div(
            *.applicableReqTypesEditorDeadRow,
            "Deleted req types: ",
            <.span(
              *.applicableReqTypesEditorDeadReqTypes,
              MutableArray(p.deadPrevious.reqTypes).map(p.reqTypes.need(_).mnemonic.value).sort.mkString(", "))))
      }

      val footer =
        <.div(
          *.applicableReqTypesEditorFooter,
          dead.whenDefined,
          error.whenDefined(<.div(*.applicableReqTypesErrMsg, _)),
        )

      Form.Field.around(label, input, footer, clear)
        .withEnabled(p.enabled)
        .withValidity(Valid when error.isEmpty)
        .render(ValidationUX.Highlight)
    }

    private val pxReqTypes: Px[ReqTypes] =
      Px.props($).map(_.reqTypes).withReuse.autoRefresh

    private val pxReqTypesInText: Px[Set[String]] =
      Px.props($).map(_.reqTypesInText).withReuse.autoRefresh

    private val pxAutoComplete: Px[AutoComplete.Strategies] =
      for {
        rt <- pxReqTypes
        ex <- pxReqTypesInText
      } yield AutoComplete.Project.reqTypeMnemonics(rt, ex)

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      inputDomRef.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))
  }

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]("ApplicableReqTypeEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(AutoComplete.install(autoCompletableInput))
    .build
}