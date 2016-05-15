package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._
import scalaz.\/
import scalaz.syntax.traverse._
import scalaz.std.option.optionInstance
import scalaz.std.string.stringInstance
import scalaz.std.vector._
import shipreq.base.util.{Ref => _, _}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.base.validation.{VFailure, ValidUpdateVR, ValidationResult}
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature._
import Text.Equality._
import EditValidationFeature.{Result => EV}
import RichTextEditor.hardcodedLive
import Text.UseCaseStep.OptionalText
import UseCaseStepFlowText.TextAndFlow

object UseCaseStepEditor {

  type InitialValue = TextAndFlow[OptionalText, Set[UseCaseStepId]]

  type Validated = TextAndFlow[ValidUpdateVR[OptionalText], ValidUpdateVR[SetDiff.NE[UseCaseStepId]]]

  /** Extra properties to apply to the tag. */
  type Extra = Validated ~=> TagMod

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : ReusableVar[String],
                   preview       : PreviewFeature.ForChild,
                   preEditValue  : Option[InitialValue],
                   extra         : Extra) {

    private val rawElems: Seq[UseCaseStepFlowText.Elem[String, String]] =
      UseCaseStepFlowText.parse(edit.value)

    private val rawTextFlow: TextAndFlow[String, Vector[String]] =
      UseCaseStepFlowText.separateTextAndFlow(rawElems)

    val parsed: TextAndFlow[OptionalText, Vector[String \/ UseCaseStepId]] =
      rawTextFlow.bimap(
        Text.UseCaseStep.parse(project),
        _.map(UseCaseStepFlowText.parseStep(project.reqs)))

    val valResult: TextAndFlow[ValidationResult[OptionalText], ValidationResult[Set[UseCaseStepId]]] =
      parsed.bimap(
        Validators.genericRichText(plainText, _),
        _.map(ValidationResult.from_\/(_)(txt => VFailure.looseMsg("Invalid step: " + txt)))
          .sequenceU
          .map(_.toSet))

    val editValResult: TextAndFlow[EV[OptionalText], EV[SetDiff.NE[UseCaseStepId]]] =
      valResult.composeF(preEditValue)(
        EditValidationFeature.compareOption(_)(_),
        EditValidationFeature.compareSetOption(_)(_))

    val validated: Validated =
      editValResult.bimap(_.value, _.value)

    val validity: Validity =
      validated.fold(_.validity)(_ & _.validity)

    val showPreview: Boolean =
      validated.fold(_.isChanged)(_ || _.isChanged)

    def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  private val editorRef = Ref[dom.html.TextArea]("i")

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(Text.UseCaseStep)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.bs($).propsA(_.project)
    private val pxPlainText  = Px.bs($).propsA(_.plainText)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(Text.UseCaseStep))

    val updateState: ReactEventTA => Callback =
      e => $.props >>= (p =>
        p.edit.set(liveCorrect(e.target.value)) >> p.preview.onEdit)

    def render(p: Props) = {
      def editor =
        <.textarea(
          *.cellEditor(p.validity),
          p.extra(p.validated),
          ^.ref       := editorRef,
          ^.value     := p.edit.value,
          ^.onBlur   --> p.preview.onBlur,
          ^.onFocus  --> p.preview.onFocus,
          ^.onChange ==> updateState)

      def preview =
        p.preview.reactCollapse(p.showPreview)(
          <.div(
            ^.ref := "p",
            "Preview",
            <.div(
              *.textEditPreview,
              p.projectWidgets.useCaseStepE(hardcodedLive, p.parsed))))

      <.div(
        editor,
        p.editValResult.text           .renderFailure,
        p.editValResult.flow(Forwards) .renderFailure,
        p.editValResult.flow(Backwards).renderFailure,
        preview)
    }
  }

  val Component =
    ReactComponentB[Props]("UseCaseStepEditor")
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}
