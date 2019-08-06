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
import shipreq.webapp.base.lib.{KeyHandlers, KeyboardTheme}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.EditTheme
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {

  sealed trait Props {
    val project         : Project
    val plainTextNoCtx  : PlainText.ForProject.NoCtx
    val textSearch      : TextSearch
    val projectWidgets  : ProjectWidgets.AnyCtx
    val edit            : StateSnapshot[String]
    val asyncStatus     : Option[EditorStatus.Async]
    val abort           : Option[Callback]
    val commitVerb      : String
    val preview         : PreviewFeature.ReadWrite.Single
    val extraKbShortcuts: KeyboardTheme.Shortcuts
    val showInstructions: Boolean
    val status          : EditorStatus
    val wantPreview     : Boolean
  }

  // ===================================================================================================================

  case class Optional(project         : Project,
                      plainTextNoCtx  : PlainText.ForProject.NoCtx,
                      textSearch      : TextSearch,
                      projectWidgets  : ProjectWidgets.AnyCtx,
                      edit            : StateSnapshot[String],
                      asyncStatus     : Option[EditorStatus.Async],
                      abort           : Option[Callback],
                      commitFn        : Option[Optional.CommitFn],
                      commitVerb      : String,
                      preview         : PreviewFeature.ReadWrite.Single,
                      preEditValue    : Option[text.OptionalText],
                      extraKbShortcuts: KeyboardTheme.Shortcuts,
                      showInstructions: Boolean) extends Props {

    val ucNum       = projectWidgets.ctx.ucNum(project)
    val richText    = text.parse(project, ucNum)(edit.value)
    val parseResult = DataValidators.genericRichText(plainTextNoCtx).audit(richText)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreOption(preEditValue)
    def commit      = (t: text.OptionalText) => commitFn.map(_ apply t)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)
    val wantPreview = Text isRich richText

    def render: VdomElement = Component(this)
  }

  object Optional {
    type CommitFn = text.OptionalText ~=> Callback
  }

  // ===================================================================================================================

  case class NonEmpty(project         : Project,
                      plainTextNoCtx  : PlainText.ForProject.NoCtx,
                      textSearch      : TextSearch,
                      projectWidgets  : ProjectWidgets.AnyCtx,
                      edit            : StateSnapshot[String],
                      asyncStatus     : Option[EditorStatus.Async],
                      abort           : Option[Callback],
                      commitFn        : Option[NonEmpty.CommitFn],
                      commitVerb      : String,
                      preview         : PreviewFeature.ReadWrite.Single,
                      preEditValue    : Option[text.NonEmptyText],
                      extraKbShortcuts: KeyboardTheme.Shortcuts,
                      showInstructions: Boolean) extends Props {

    val ucNum       = projectWidgets.ctx.ucNum(project)
    val richTextO   = text.parseNonEmpty(project, ucNum)(edit.value)
    val parseResult = DataValidators.genericRichTextNonEmpty(text, plainTextNoCtx).audit(richTextO)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreOption(preEditValue)
    def commit      = (t: text.NonEmptyText) => commitFn.map(_ apply t)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abort)
    val wantPreview = richTextO.exists(Text isRich _.whole)

    def render: VdomElement = Component(this)
  }

  object NonEmpty {
    type CommitFn = text.NonEmptyText ~=> Callback
  }

  // ===================================================================================================================

//  implicit val reusabilityProps: Reusability[Props] =
//    Reusability.never // TODO Reusability.derive

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.EditorBackend {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.projectWidgets.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.Project.richText(text))

    private val keyHandlerBase =
      KeyHandlers.base(
        KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), text.lineCardinality))

    val textareaConst: TagMod = {
      val updateState: ReactEventFromTextArea => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >>
            p.preview.onEdit(p.wantPreview)))

      TagMod(
        ^.autoFocus := true,
        ^.onBlur   --> (autoCompleteBlur >> $.props.flatMap(_.preview.onBlur)),
        ^.onChange ==> updateState,
        ^.onFocus  --> $.props.flatMap(p => p.preview.onFocus(p.wantPreview)),
        RichTextEditor.minRows(text.lineCardinality))
    }

    def render(p: Props) = {

      def editor(validity: Validity): VdomElement = {
        val keys = keyHandlerBase(p.extraKbShortcuts.keyHandlers)
        val base = TagMod(textareaConst, keys)
        editorRef.component(EditTheme.autosizeTextareaProps(validity, p.edit.value, base))
      }

      def instructions: TagMod =
        TagMod.when(p.showInstructions)(
          KeyboardTheme.Instructions(
            p.extraKbShortcuts.instructions ::: KeyboardTheme.Instructions.Clauses.forTextEditor(
              text.lineCardinality,
              commit = p.status.getCommit,
              commitVerb = p.commitVerb,
              abort = p.abort),
            help = Some(RichTextEditorHelp.modal.show)))

      def richText: VdomTag =
        p match {
          case p2: Optional => p.projectWidgets.text(p2.richText, hardcodedLive, Mandatory.Not)
          case p2: NonEmpty => p.projectWidgets.text(text.toOptional(p2.richTextO), hardcodedLive, Mandatory)
        }

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

  object ManualIssue extends RichTextEditor("MI", Text.ManualIssue)
}
