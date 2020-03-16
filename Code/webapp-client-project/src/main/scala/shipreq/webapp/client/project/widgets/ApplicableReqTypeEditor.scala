package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.ValidationUX
import shipreq.webapp.base.ui.GeneralTheme
import shipreq.webapp.base.ui.semantic.{Form, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.{widgets => *}

@UsesSemanticUiManually
object ApplicableReqTypeEditor {

  sealed trait Props {
    @inline final def render: VdomElement = Component(this)
  }

  // ===================================================================================================================

  object FullFormField {
    final case class Props(state     : StateSnapshot[State],
                           previous  : ApplicableReqTypes,
                           reqTypes  : ReqTypes,
                           filterDead: FilterDead,
                           enabled   : Enabled) extends ApplicableReqTypeEditor.Props {

      lazy val deadPrevious = previous.filterReqTypes(Dead, reqTypes)
      lazy val validator    = DataValidators.reqTypeSeqStr(reqTypes).unnamed
      lazy val validated    = validator(state.value.text)
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

    implicit val reusabilityProps: Reusability[Props] = Reusability.derive
  }

  // ===================================================================================================================

  /* TBD: Just req types
     - used in field cfg
     - just an <input>, no applicability
     - validation - all reqTypes valid and live (same as above)
                  - also: fail if defined elsewhere
     - dead types displayed elsewhere
   */

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {

    private val label = <.label(UiText.FieldNames.applicableReqTypes)

    private val renderItem: Applicability => TagMod = {
      case Applicable    => "Whitelist"
      case NotApplicable => "Blacklist"
    }

    private val items: NonEmptyVector[Applicability] =
      NonEmptyVector(Applicable, NotApplicable)

    private val inputTagMod: TagMod =
      TagMod(^.placeholder := "Req types...")

    private val clear =
      <.div(^.clear.right)

    private def renderFullFormField(p: FullFormField.Props): VdomNode = {
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

    def render(p: Props): VdomNode =
      p match {
        case a: FullFormField.Props => renderFullFormField(a)
      }
  }

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]("ApplicableReqTypeEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}