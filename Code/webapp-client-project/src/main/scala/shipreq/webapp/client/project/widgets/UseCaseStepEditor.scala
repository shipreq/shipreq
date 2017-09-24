package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
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
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.lib.{KeyHandler, KeyboardTheme}
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{AsyncFeature, EditorStatus, PreviewFeature}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.project.lib.DataReusability._
import RichTextEditor.hardcodedLive
import Text.Equality._
import Text.UseCaseStep.{OptionalText, lineCardinality}
import UseCaseStepFlowText.TextAndFlow

object UseCaseStepEditor {

  type InitialValue = TextAndFlow[OptionalText, Set[UseCaseStepId]]

  type Validated = TextAndFlow[
    PotentialChange[Invalidity, OptionalText],
    PotentialChange[Invalidity, SetDiff.NE[UseCaseStepId]]]

  type ValidatedChanges = PotentialChange[Invalidity, UseCaseStepGD.NonEmptyValues]

  type CommitFn = UseCaseStepGD.NonEmptyValues ~=> Callback

  final case class ShiftProps(asyncState: AsyncFeature.Read.D0[Any],
                              run       : LeftRight.Values[Option[Callback]]) {
    def apply(d: LeftRight): Option[Callback] =
      if (asyncState.isEmpty) run(d) else None
  }

  final case class Props(project        : Project,
                         plainTextNoCtx : PlainText.ForProject.NoCtx,
                         textSearch     : TextSearch,
                         projectWidgets : ProjectWidgets.AnyCtx,
                         edit           : StateSnapshot[String],
                         asyncStatus    : Option[EditorStatus.Async],
                         abort          : Callback,
                         commit         : CommitFn,
                         shift          : ShiftProps,
                         preview        : PreviewFeature.ReadWrite.Single,
                         preEditValue   : Option[InitialValue]) {

    private val rawElems: Seq[UseCaseStepFlowText.Elem[String, String]] =
      UseCaseStepFlowText.parse(edit.value)

    private val rawTextFlow: TextAndFlow[String, Vector[String]] =
      UseCaseStepFlowText.separateTextAndFlow(rawElems)

    private val ucNum: Option[ReqTypePos] =
      projectWidgets.ctx.ucNum(project)

    val parsed: TextAndFlow[OptionalText, Vector[String \/ UseCaseStepId]] =
      rawTextFlow.bimap(
        Text.UseCaseStep.parse(project, ucNum),
        _.map(UseCaseStepFlowText.parseStep(project.reqs, ucNum)))

    val valResult: TextAndFlow[Invalidity \/ OptionalText, Invalidity \/ Set[UseCaseStepId]] =
      parsed.bimap(
        DataValidators.genericRichText(plainTextNoCtx).audit(_),
        _.map(_.leftMap(txt => Invalidity("Invalid step: " + txt)))
          .sequence[Invalidity \/ ?, UseCaseStepId](implicitly, Invalidity.applicative)
          .map(_.toSet))

    val validated: Validated =
      valResult.composeF(preEditValue)(
        PotentialChange.fromDisjunction(_).ignoreOption(_),
        PotentialChange.fromDisjunction(_).setDiffOption(_))

    val validatedChanges: ValidatedChanges =
      validated.fold(_.getFailure)(_ orElse _.getFailure) match {
        case None =>
          var vs = UseCaseStepGD.emptyValues
          for (v <- validated.text          ) vs += UseCaseStepGD.Title  (v)
          for (v <- validated flow Forwards ) vs += UseCaseStepGD.FlowOut(v)
          for (v <- validated flow Backwards) vs += UseCaseStepGD.FlowIn (v)
          PotentialChange.nonEmpty(vs)
        case Some(failure) =>
          PotentialChange.Failure(failure)
      }

    val validity: Validity =
      validated.fold(_.validity)(_ & _.validity)

    val wantPreview: Boolean =
      parsed.flow.exists(_.nonEmpty) || Text.isRich(parsed.text)

    val status: EditorStatus =
      asyncStatus.getOrElse(
        EditorStatus.fromValidatedChange(validatedChanges)(v => Some(commit(v)), Some(abort)))

    def render: VdomElement = Component(this)
  }

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.caseClass

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(Text.UseCaseStep)

  private val shiftKeyCriterion: LeftRight.Values[KeyHandler.Criterion] =
    LeftRight.Values { d =>
      import KeyHandler._
      val keyCode = d match {
        case LeftRight.Left  => KeyCode.Left
        case LeftRight.Right => KeyCode.Right
      }
      Criterion(EventType.KeyDown, keyCode, ModKey.Alt)
    }
  private val shiftKeyDesc: LeftRight => String = {
    case LeftRight.Left  => "alt-left"
    case LeftRight.Right => "alt-right"
  }

  private val rightLeft: List[LeftRight] =
    LeftRight.Right :: LeftRight.Left :: Nil

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.projectWidgets.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.Project.richText(Text.UseCaseStep))

    val textareaConst: TagMod = {
      def shiftStepKeyHandler(d: LeftRight): KeyHandler =
        shiftKeyCriterion(d).handle(
          $.props.flatMap(_.shift(d).getOrEmpty)
        ).asEventDefault

      val keys = (
        LeftRight.mapReduce(shiftStepKeyHandler)(_ + _)
          + KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort))
          + KeyboardTheme.commitCO($.props.map(_.status.getCommit), lineCardinality))

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >>
            p.preview.onEdit(p.wantPreview)))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(p => p.preview.onFocus(p.wantPreview)),
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    private def instructions(p: Props) = {
      // Usual clauses
      var clauses = KeyboardTheme.Instructions.clausesForTextEditor(
        lineCardinality,
        commit = p.status.getCommit,
        abort = Some(p.abort))

      // Shift left/right clauses
      for {
        d  <- rightLeft
        cb <- p.shift(d)
      } clauses ::=
        KeyboardTheme.Instructions.Clause.keyToAction(shiftKeyDesc(d))(UiText.useCaseStepShift(d).toLowerCase, cb)

      KeyboardTheme.Instructions(clauses, help = Some(RichTextEditorHelp.modal.show))
    }

    def render(p: Props) = {
      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def richText =
        p.projectWidgets.useCaseStepTextAndMaybeInvalidFlow(p.parsed, hardcodedLive)

      def preview =
        EditTheme.renderPreview(p.preview, p.wantPreview, richText)

      EditTheme.renderEditor(p.status, editor, richText, instructions(p), preview)
    }
  }

  val Component =
    ScalaComponent.builder[Props]("UseCaseStepEditor")
      .renderBackend[Backend]
      .configure(
//        Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .build
}
