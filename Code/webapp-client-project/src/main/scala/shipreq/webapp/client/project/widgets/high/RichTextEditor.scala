package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.Validity
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.client.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.app.Style.{widgets => *}
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
                   edit          : StateSnapshot[String],
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

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(text))

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), text.lineCardinality)

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >> p.preview.onEdit))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(_.preview.onFocus),
        RichTextEditor.minRows(text.lineCardinality),
        keys)
    }

    val editorRef = ScalaComponent.mutableRefTo(AutosizeTextarea.Component)

    def getTextarea(): html.TextArea =
      editorRef.value.getDOMNode.domCast

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(
          text.lineCardinality,
          p.status.getCommit,
          p.abort,
          Some(RichTextEditorHelp.modal.show))

      def richText =
        p.projectWidgets.format(hardcodedLive, p.richText)

      def preview =
        RichTextEditor.renderPreview(p.preview, p.showPreview, richText)

      EditTheme.renderEditor(p.status, editor, richText, instructions, preview)
    }
  }

  val Component =
    ScalaComponent.build[Props]("RichTextEditor:" + name)
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.installBP(_.backend.getTextarea(), _.pxAutoComplete.value(), _.edit.setState))
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

  def renderPreview(pf: PreviewFeature.ForChild, show: => Boolean, view: => VdomNode): VdomNode =
    pf.reactCollapse(show)(
      <.div(*.richTextPreview,
        <.div(*.richTextPreviewHeader, "Preview"),
        <.div(*.richTextPreviewBody, view)))

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline def hardcodedLive = Live

  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor("UCT", Text.UseCaseTitle)

  object ReqCodeGroupTitle extends RichTextEditor("RCGT", Text.ReqCodeGroupTitle)

  object CustomTextField extends RichTextEditor("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)
}
