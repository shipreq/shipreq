package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.base.util.{PotentialChange, Validity}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.lib.{KeyboardTheme, AbortCommit => AbortCommit2}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.{AutosizeTextarea, EditTheme}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {

  type CommitFn    = text.OptionalText ~=> Callback
  type AbortCommit = Option[AbortCommit2[Callback, CommitFn]]

  case class Props(project         : Project,
                   plainText       : PlainText.ForProject,
                   textSearch      : TextSearch,
                   projectWidgets  : ProjectWidgets,
                   edit            : StateSnapshot[String],
                   asyncStatus     : Option[EditorStatus.Async],
                   abortCommit     : AbortCommit,
                   preview         : PreviewFeature.ReadWrite.Single,
                   preEditValue    : Option[text.OptionalText],
                   showInstructions: Boolean) {

    val richText    = text.parse(project)(edit.value)
    val parseResult = DataValidators.genericRichText(plainText).audit(richText)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreOption(preEditValue)
    def abort       = abortCommit.map(_.abort)
    def commit      = (t: text.OptionalText) => abortCommit.map(_ commit t)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)
    val wantPreview = Text isRich richText

    def render: VdomElement = Component(this)
  }

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.caseClass

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.Project.richText(text))

    val textareaConst: TagMod = {
      val keys =
        KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), text.lineCardinality)

      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >>
            p.preview.onEdit(p.wantPreview)))

      TagMod(
        ^.autoFocus := true,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(p => p.preview.onFocus(p.wantPreview)),
        RichTextEditor.minRows(text.lineCardinality),
        keys)
    }

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement =
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, textareaConst))

      def instructions: TagMod =
        TagMod.when(p.showInstructions)(
          KeyboardTheme.instructionsForCommitAbort(
            text.lineCardinality,
            p.status.getCommit,
            p.abort,
            Some(RichTextEditorHelp.modal.show)))

      def richText: VdomTag =
        p.projectWidgets.format(hardcodedLive, p.richText)

      def preview: VdomNode =
        EditTheme.renderPreview(p.preview, p.wantPreview, richText)

      EditTheme.renderEditor(p.status, editor, richText, instructions, preview)
    }
  }

  val Component =
    ScalaComponent.builder[Props]("RichTextEditor:" + name)
      .renderBackend[Backend]
      .configure(
//        Reusability.shouldComponentUpdate,
        AutoComplete.install)
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

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline def hardcodedLive = Live

  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor("UCT", Text.UseCaseTitle)

  object CodeGroupTitle extends RichTextEditor("CGT", Text.CodeGroupTitle)

  object CustomTextField extends RichTextEditor("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)
}
