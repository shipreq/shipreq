package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.\/
import scalaz.syntax.traverse._
import scalaz.std.option.optionInstance
import scalaz.std.string.stringInstance
import scalaz.std.vector._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.base.validation.{VFailure, ValidUpdateVR, ValidationResult}
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.KeyboardTheme
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature._
import EditValidationFeature.{Result => EV}
import RichTextEditor.hardcodedLive
import Text.Equality._
import Text.UseCaseStep.{OptionalText, lineCardinality}
import UseCaseStepFlowText.TextAndFlow

object UseCaseStepEditor {

  type InitialValue = TextAndFlow[OptionalText, Set[UseCaseStepId]]

  type Validated = TextAndFlow[ValidUpdateVR[OptionalText], ValidUpdateVR[SetDiff.NE[UseCaseStepId]]]

  type ValidatedChanges = ValidUpdateVR[UseCaseStepGD.NonEmptyValues]

  type CommitFn = UseCaseStepGD.NonEmptyValues ~=> Callback

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : StateSnapshot[String],
                   asyncStatus   : Option[EditorStatus.Async],
                   abort         : Callback,
                   commit        : CommitFn,
                   preview       : PreviewFeature.ForChild,
                   preEditValue  : Option[InitialValue]) {

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

    val validatedChanges: ValidatedChanges =
      validated.fold(_.getFailure)(_ orElse _.getFailure) match {
        case None =>
          var vs = UseCaseStepGD.emptyValues
          for (v <- validated.text          ) vs += UseCaseStepGD.Title  (v)
          for (v <- validated flow Forwards ) vs += UseCaseStepGD.FlowOut(v)
          for (v <- validated flow Backwards) vs += UseCaseStepGD.FlowIn (v)
          ValidUpdate.nonEmpty(vs)
        case Some(failure) =>
          ValidUpdate.Failure(failure)
      }

    val validity: Validity =
      validated.fold(_.validity)(_ & _.validity)

    val showPreview: Boolean =
      validated.fold(_.isChanged)(_ || _.isChanged)

    val status: EditorStatus =
      asyncStatus getOrElse EditorStatus.validUpdateV(validatedChanges)(commit, abort)

    def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.never // TODO Reusability.caseClass

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(Text.UseCaseStep)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(Text.UseCaseStep))

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
          KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality)

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >> p.preview.onEdit))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(_.preview.onFocus),
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    private val editorRef = ScalaComponent.mutableRefTo(AutosizeTextarea.Component)

    def render(p: Props) = {
      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(
          lineCardinality,
          p.status.getCommit,
          p.abort,
          Some(RichTextEditorHelp.modal.show))

      def richText =
        p.projectWidgets.useCaseStepE(hardcodedLive, p.parsed)

      def preview =
        RichTextEditor.renderPreview(p.preview, p.showPreview, richText)

      EditTheme.renderEditor(p.status, editor, richText, instructions, preview)
    }

    def getTextarea(): html.TextArea =
      editorRef.value.getDOMNode.domCast
  }

  val Component =
    ScalaComponent.build[Props]("UseCaseStepEditor")
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.setState))
      .build
}
