package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.AutosizeTextarea
import shipreq.webapp.client.base.ui.semantic.{Colour, Icon, Label}
import shipreq.webapp.client.project.app.Style, Style.{widgets => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import RichTextEditor.hardcodedLive

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {

  type CommitFn    = text.OptionalText ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : ReusableVar[String],
                   asyncStatus   : Option[EditorStatus.Async],
                   abortCommit   : AbortCommit,
                   preview       : PreviewFeature.ForChild,
                   preEditValue  : Option[text.OptionalText]) {

    val richText    = text.parse(project)(edit.value)
    val parseResult = Validators.genericRichText(plainText, richText)
    val validated   = EditValidationFeature.compareOption(parseResult)(preEditValue)
    def abort       = abortCommit.fold(Callback.empty)(_.abort)
    def commit      = (t: text.OptionalText) => abortCommit.fold(Callback.empty)(_ commit t)
    val status      = asyncStatus getOrElse EditorStatus.validUpdateV(validated)(commit, abort)
    def showPreview = validated.value.isChanged

    def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.never // TODO Reusability.caseClass

  private val editorRef = Ref.to(AutosizeTextarea.Component, "i")

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.bs($).propsA(_.project)
    private val pxPlainText  = Px.bs($).propsA(_.plainText)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(text))

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), text.lineCardinality)

      val updateState: ReactEventTA => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.set(liveCorrect(e.target.value)) >> p.preview.onEdit))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(_.preview.onFocus),
        RichTextEditor.minRows(text.lineCardinality),
        keys)
    }

    def getTextarea() =
      editorRef($).get.getDOMNode()

    def render(p: Props) = {

      def editor(validity: Validity): ReactElement =
        AutosizeTextarea.withRef(editorRef)(
          *.textEditor(p.validated.validity),
          ^.value := p.edit.value,
          textareaConst)

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(
          text.lineCardinality,
          p.status.getCommit,
          p.abort,
          Some(RichTextEditorHelp.modal.show))

      def richText =
        p.projectWidgets.format(hardcodedLive, p.richText)

      RichTextEditor.genericRender(p.status, editor, instructions, p.preview, p.showPreview, richText)
    }
  }

  val Component =
    ReactComponentB[Props]("RichTextEditor:" + name)
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.install(
          _.backend.getTextarea(),
          (p, b) => b.pxAutoComplete.value(),
          (p, b) => p.edit.set))
      .build
}

// ===================================================================================================================

object RichTextEditor {
  val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, "")
  }

  def liveCorrect(text: Text.Generic): EndoFn[String] =
    text.lineCardinality match {
      case SingleLine => RichTextEditor.correctSingleLineText
      case MultiLine  => identity
    }

  val minRows = LineCardinality.memo[TagMod] {
    case SingleLine => ^.rows := 1
    case MultiLine  => ^.rows := 3
  }

  def genericRender(status        : EditorStatus,
                    editor        : Validity => ReactElement,
                    instructions  : => ReactTag,
                    previewFeature: PreviewFeature.ForChild,
                    showPreview   : Boolean,
                    richText      : => ReactTag) = {
    def preview =
      previewFeature.reactCollapse(showPreview)(
        <.div(*.richTextPreview, ^.ref := "p",
          <.div(*.richTextPreviewHeader, "Preview"),
          <.div(*.richTextPreviewBody, richText)))

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        <.div(
          editor(Valid),
          instructions,
          preview)

      case EditorStatus.Invalid(err) =>
        <.div(
          editor(Invalid), // TODO add error background
          *.errorPointingUp(err),
          preview)

      case EditorStatus.AsyncError(err, _, _) =>
        <.div(
          editor(Valid),
          *.errorPointingUp(err),
          preview)

      case EditorStatus.InTransit =>
        <.div(*.textEditor(Style.EditorState.InTransit),
          <.div(Icon.CircleNotched.loading.tag),
          <.div(*.textEditorInTransitValue, richText))
    }
  }

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline def hardcodedLive = Live

  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor("UCT", Text.UseCaseTitle)

  object ReqCodeGroupTitle extends RichTextEditor("RCGT", Text.ReqCodeGroupTitle)

  object CustomTextField extends RichTextEditor("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)
}
