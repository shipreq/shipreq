package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalaz.\/
import scalaz.std.option.optionInstance
import scalaz.std.string.stringInstance
import scalaz.std.vector._
import scalaz.syntax.traverse._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.UseCaseStepGD
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{AsyncFeature, EditorStatus, PreviewFeature}
import shipreq.webapp.base.lib.{KeyHandler, KeyboardTheme}
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.client.project.feature.EditorFeature.PotentialValueAcceptor
import shipreq.webapp.client.project.lib.DataReusability._

object UseCaseStepEditor {
  import RichTextEditor.hardcodedLive
  import Text.Equality._
  import Text.UseCaseStep.{OptionalText, lineCardinality}
  import UseCaseStepFlowText.TextAndFlow

  type InitialValue = TextAndFlow[OptionalText, Set[UseCaseStepId]]

  type Validated = TextAndFlow[
    PotentialChange[Invalidity, OptionalText],
    PotentialChange[Invalidity, SetDiff.NE[UseCaseStepId]]]

  type ValidatedChanges = PotentialChange[Invalidity, UseCaseStepGD.NonEmptyValues]

  type CommitFn = UseCaseStepGD.NonEmptyValues ~=> Callback

  final case class Props(project        : Project,
                         plainTextNoCtx : PlainText.ForProject.NoCtx,
                         textSearch     : TextSearch,
                         projectWidgets : ProjectWidgets.AnyCtx,
                         edit           : StateSnapshot[String],
                         asyncStatus    : Option[EditorStatus.Async],
                         abort          : Callback,
                         commit         : CommitFn,
                         shiftRunner    : Option[AsyncFeature.Runner.D0O[LeftRight, Any]],
                         addStepRunner  : Option[AsyncFeature.Runner.D0O[Unit, Any]],
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
        _.map(UseCaseStepFlowText.parseStep(project.content.reqs.useCaseStepLabelLookup, ucNum)))

    val valResult: TextAndFlow[Invalidity \/ OptionalText, Invalidity \/ Set[UseCaseStepId]] =
      parsed.bimap(
        DataValidators.genericRichText(plainTextNoCtx).audit(_),
        _.map(_.leftMap(Invalidity(_)))
          .sequence[Invalidity \/ *, UseCaseStepId](implicitly, Invalidity.applicative)
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

    val saveAndAdd: Option[Callback] =
      for {
        c <- status.getCommit
        a <- addStepRunner.flatMap(_.runOption(()))
      } yield c >> a

    def render: VdomElement = Component(this)
  }

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.derive

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(Text.UseCaseStep)

  val potentialValueAcceptor: PotentialValueAcceptor[String] =
    PotentialValueAcceptor.correct(liveCorrect)

  def saveAndAddKeyCriterion = KeyHandler.Criterion.AltEnter

  private val shiftKeyCriterion: LeftRight.Values[KeyHandler.Criterion] =
    LeftRight.Values { d =>
      import KeyHandler._
      val (desc, keyCode) = d match {
        case LeftRight.Left  => ("alt-left", KeyCode.Left)
        case LeftRight.Right => ("alt-right", KeyCode.Right)
      }
      Criterion(desc, EventType.KeyDown, keyCode, ModKey.Alt)
    }

  private val rightLeft: List[LeftRight] =
    LeftRight.Right :: LeftRight.Left :: Nil

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.projectWidgets.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      for {
        p  <- pxProject
        pt <- pxPlainText
        ts <- pxTextSearch
      } yield {
        val naTags = p.config.naTags(StaticReqType.UseCase)
        AutoComplete.Project.richText(Text.UseCaseStep)(p, naTags, pt, ts)
      }

    val textareaConst: TagMod = {

      def shiftStepKeyHandler(d: LeftRight): KeyHandler =
        shiftKeyCriterion(d).handle(
          $.props.flatMap(_.shiftRunner.fold(Callback.empty)(_.runOrDoNothing(d))))

      val keys = (
        LeftRight.mapReduce(shiftStepKeyHandler)(_ + _)
          + saveAndAddKeyCriterion.handle($.props.flatMap(_.saveAndAdd.getOrEmpty))
          + KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort))
          + KeyboardTheme.commitCO($.props.map(_.status.getCommit)))

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >>
            p.preview.onEdit(p.wantPreview)))

      TagMod(
        ^.autoFocus := true,
        ^.onBlur   --> (autoCompleteBlur >> $.props.flatMap(_.preview.onBlur)),
        ^.onChange ==> updateState,
        ^.onFocus  --> $.props.flatMap(p => p.preview.onFocus(p.wantPreview)),
        RichTextEditor.minRows(lineCardinality),
        keys)
    }

    private def instructions(p: Props) = {
      import KeyboardTheme.Instructions

      // Usual clauses
      var clauses = Instructions.Clauses.forTextEditor(
        lineCardinality,
        commit = p.status.getCommit,
        commitVerb = Instructions.defaultCommitVerb,
        abort = Some(p.abort))

      // Save-and-add
      p.saveAndAdd.foreach(cb =>
        clauses ::= Instructions.Clause.keyToAction(saveAndAddKeyCriterion.desc)("save and add next step", cb))

      // Shift left/right clauses
      for {
        d  <- rightLeft
        cb <- p.shiftRunner.flatMap(_.runOption(d))
      } clauses ::=
        Instructions.Clause.keyToAction(shiftKeyCriterion(d).desc)(UiText.useCaseStepShift(d).toLowerCase, cb)

      Instructions(clauses, help = Some(RichTextEditorHelp.modalFor(Text.UseCaseStep).show))
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

    val onMount: Callback =
      EditTheme.onTextareaEditorMount(editorRef).toCallback
  }

  val Component =
    ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(
        //Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .componentDidMount(_.backend.onMount)
      .build
}
