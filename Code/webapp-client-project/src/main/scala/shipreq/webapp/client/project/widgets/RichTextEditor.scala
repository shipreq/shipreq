package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{PotentialChange, Validity}
import shipreq.webapp.base.data
import shipreq.webapp.base.data.derivation.NaTags
import shipreq.webapp.base.data.{Optional => _, _}
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.feature.{EditControlsFeature, EditorStatus, PreviewFeature}
import shipreq.webapp.base.jsfacade.ScrollIntoViewIfNeeded
import shipreq.webapp.base.lib.{ConfirmJs, KeyHandlers, KeyboardTheme, TaskRepeater}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.base.ui.OptionalFullscreen
import shipreq.webapp.base.util.PreProcessor
import shipreq.webapp.client.project.feature.EditorFeature.PotentialValueAcceptor
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.RichTextEditor.hardcodedLive

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {
  import RichTextEditor.State

  sealed trait Props {
    val project           : Project
    val naTags            : NaTags
    val plainTextNoCtx    : PlainText.ForProject.NoCtx
    val textSearch        : TextSearch
    val projectWidgets    : ProjectWidgets.AnyCtx
    val edit              : StateSnapshot[String]
    val asyncStatus       : Option[EditorStatus.Async]
    val abort             : Option[Callback]
    val abortConfirmation : Option[ConfirmJs]
    val commitVerb        : String
    val preview           : PreviewFeature.ReadWrite.Single
    val extraKbShortcuts  : KeyboardTheme.Shortcuts
    val showInstructions  : Boolean
    val status            : EditorStatus
    val wantPreview       : Boolean
    val autoFocus         : Boolean
    val editorStyle       : EditControlsFeature.Style
    val optionalFullscreen: Option[OptionalFullscreen]

    def validated: PotentialChange[Any, Any]
    val richText: text.OptionalText

    final lazy val abortWithConfirmation: Option[Callback] =
      abort.map { actuallyAbort =>
        abortConfirmation.filter(_ => validated.isChanged) match {
          case None          => actuallyAbort
          case Some(confirm) =>
            for {
              yes <- confirm("Are you sure you want to discard your unsaved changes?")
              _   <- actuallyAbort.when(yes)
            } yield ()
        }
      }
  }

  // ===================================================================================================================

  case class Optional(project           : Project,
                      naTags            : NaTags,
                      plainTextNoCtx    : PlainText.ForProject.NoCtx,
                      textSearch        : TextSearch,
                      projectWidgets    : ProjectWidgets.AnyCtx,
                      edit              : StateSnapshot[String],
                      asyncStatus       : Option[EditorStatus.Async],
                      abort             : Option[Callback],
                      abortConfirmation : Option[ConfirmJs],
                      autoFocus         : Boolean,
                      commitFn          : Option[Optional.CommitFn],
                      commitVerb        : String,
                      editorStyle       : EditControlsFeature.Style,
                      preview           : PreviewFeature.ReadWrite.Single,
                      preEditValue      : Option[text.OptionalText],
                      extraKbShortcuts  : KeyboardTheme.Shortcuts,
                      showInstructions  : Boolean,
                      optionalFullscreen: Option[OptionalFullscreen]) extends Props {

    val ucNum       = projectWidgets.ctx.ucNum(project)
    val richText    = text.parse(project, ucNum)(edit.value)
    val parseResult = DataValidators.genericRichText(plainTextNoCtx).audit(richText)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreOption(preEditValue)
    def commit      = (t: text.OptionalText) => commitFn.map(_ apply t)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abortWithConfirmation)
    val wantPreview = Text isRich richText

    def render: VdomElement = Component(this)
  }

  object Optional {
    type CommitFn = text.OptionalText ~=> Callback
  }

  // ===================================================================================================================

  case class NonEmpty(project           : Project,
                      naTags            : NaTags,
                      plainTextNoCtx    : PlainText.ForProject.NoCtx,
                      textSearch        : TextSearch,
                      projectWidgets    : ProjectWidgets.AnyCtx,
                      edit              : StateSnapshot[String],
                      asyncStatus       : Option[EditorStatus.Async],
                      abort             : Option[Callback],
                      abortConfirmation : Option[ConfirmJs],
                      autoFocus         : Boolean,
                      commitFn          : Option[NonEmpty.CommitFn],
                      commitVerb        : String,
                      editorStyle       : EditControlsFeature.Style,
                      preview           : PreviewFeature.ReadWrite.Single,
                      preEditValue      : Option[text.NonEmptyText],
                      extraKbShortcuts  : KeyboardTheme.Shortcuts,
                      showInstructions  : Boolean,
                      optionalFullscreen: Option[OptionalFullscreen]) extends Props {

    val ucNum       = projectWidgets.ctx.ucNum(project)
    val richTextO   = text.parseNonEmpty(project, ucNum)(edit.value)
    val parseResult = DataValidators.genericRichTextNonEmpty(text, plainTextNoCtx).audit(richTextO)
    val validated   = PotentialChange.fromDisjunction(parseResult).ignoreOption(preEditValue)
    def commit      = (t: text.NonEmptyText) => commitFn.map(_ apply t)
    val status      = asyncStatus getOrElse EditorStatus.fromValidatedChange(validated)(commit, abortWithConfirmation)
    val wantPreview = richTextO.exists(Text isRich _.whole)
    val richText    = text.toOptional(richTextO)

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

  val potentialValueAcceptor: PotentialValueAcceptor[String] =
    PotentialValueAcceptor.correct(liveCorrect)

  private val layoutPosition: EditControlsFeature.Layout => Option[PreviewFeature.Position] =
    text.lineCardinality match {
      case SingleLine => _.position
      case MultiLine  => _.positionIfShown
    }

  final class Backend($: BackendScope[Props, State]) extends AutoComplete.EditorBackend {
    private val pxProject    = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxNaTags     = Px.props($).map(_.naTags).withReuse.autoRefresh
    private val pxPlainText  = Px.props($).map(_.projectWidgets.plainText).withReuse.autoRefresh
    private val pxTextSearch = Px.props($).map(_.textSearch).withReuse.autoRefresh

    override val pxAutoComplete =
      Px.apply4(pxProject, pxNaTags, pxPlainText, pxTextSearch)(AutoComplete.Project.richText(text, _, _, _, _))

    private val scrollIntoView: Callback =
      TaskRepeater.millis(
        task   = $.getDOMNode.map(_.toHtml).asCBO.flatMap(ScrollIntoViewIfNeeded(_)).toCallback,
        gap    = 200,
        window = 500,
      ).run

    private val keyHandlerBase =
      KeyHandlers.base(
        autoCompleteKeyHandlers
          + KeyboardTheme.abortCriterion.handleWhenDefined($.props.map(_.abortWithConfirmation))
          + KeyboardTheme.commitCO($.props.map(_.status.getCommit))
      )

    val textareaConst: TagMod = {
      val onFocus: Callback =
        $.props.flatMap(p => p.preview.onFocus(p.wantPreview)) >> scrollIntoView

      val onChange: ReactEventFromTextArea => Callback =
        e => $.props.flatMap(p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(e.target.value)) >>
            p.preview.onEdit(p.wantPreview)))

      val onBlur: Callback =
        autoCompleteOnBlur >> $.props.flatMap(_.preview.onBlur)

      TagMod(
        ^.onFocus  --> onFocus,
        ^.onChange ==> onChange,
        ^.onBlur   --> onBlur,
        ^.onClick  ==> autoCompleteOnClick,
        RichTextEditor.minRows(text.lineCardinality))
    }

    def render(p: Props, s: State): VdomNode =
      renderFn(p, s)

    private val renderFn: (Props, State) => VdomNode =
      text.lineCardinality match {
        case SingleLine => _render(_, _, None)
        case MultiLine  => (p, s) => _render(p, s, p.optionalFullscreen)
      }

    private def _render(p: Props, s: State, optionalFullscreen: Option[OptionalFullscreen]): VdomNode = {

      def editor(layout: EditControlsFeature.Layout, enabled: Enabled, validity: Validity): VdomElement = {
        val keys = keyHandlerBase(p.extraKbShortcuts.keyHandlers)

        val base = TagMod(
          textareaConst,
          keys,
          ^.autoFocus  := p.autoFocus)

        val autosizeProps = EditControlsFeature.autosizeTextareaProps(
          mode     = layout.mode,
          position = layoutPosition(layout),
          enabled  = enabled,
          validity = validity,
          value    = p.edit.value,
          tagMod   = base,
          font     = s.font,
        )

        editorRef.component(autosizeProps)
      }

      def instructions(fullscreenCtx: Option[OptionalFullscreen.Ctx]): TagMod =
        TagMod.when(p.showInstructions) {

          val textEditorInstructions =
            KeyboardTheme.Instructions.Clauses.forTextEditor(
              text.lineCardinality,
              commit     = p.status.getCommit,
              commitVerb = p.commitVerb,
              abort      = p.abortWithConfirmation)

          val clauses =
            p.extraKbShortcuts.instructions ::: textEditorInstructions

          KeyboardTheme.Instructions(
            clauses    = clauses,
            help       = Some(RichTextEditorHelp.modalFor(text).show),
            fullscreen = fullscreenCtx,
            monospace  = Some(StateSnapshot.zoomL(State.monospace)(s).setStateVia($))
          )
        }

      def richText: VdomTag =
        p match {
          case p2: Optional => p.projectWidgets.text(p2.richText, hardcodedLive, p.naTags, data.Optional)
          case p2: NonEmpty => p.projectWidgets.text(text.toOptional(p2.richTextO), hardcodedLive, p.naTags, Mandatory)
        }

      EditControlsFeature.renderEditor(
        status             = p.status,
        optionalFullscreen = optionalFullscreen,
        editor             = editor,
        readOnlyView       = richText,
        instructions       = instructions,
        style              = p.editorStyle,
        font               = s.font,
        previewRW          = p.preview,
        previewWantOpen    = p.wantPreview,
        previewBody        = richText,
      )
    }

    val onMount: Callback =
      for {
        _ <- EditControlsFeature.onTextareaEditorMount(editorRef, $.props.map(_.autoFocus)).toCallback
        p <- $.props
        _ <- scrollIntoView.when_(p.autoFocus)
      } yield ()
  }

  private def initialState(p: Props) =
    State(
      monospace = p.richText.exists(_.containsType[Atom.CodeBlock#CodeBlock]),
    )

  val Component =
    ScalaComponent.builder[Props]("RichTextEditor:" + name)
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(
        //Reusability.shouldComponentUpdate,
        AutoComplete.install)
      .componentDidMount(_.backend.onMount)
      .build
}

// ===================================================================================================================

object RichTextEditor {

  @Lenses
  final case class State(monospace: Boolean) {
    val font: EditControlsFeature.Font =
      if (monospace)
        EditControlsFeature.Font.Monospace
      else
        EditControlsFeature.Font.Default
  }

  private val preprocessor = {
    val preprocessSL = PreProcessor(PreProcessor.FixChar.singleLine, PreProcessor.CanTrim.no)
    val preprocessML = PreProcessor(PreProcessor.FixChar.multiLine, PreProcessor.CanTrim.no)
    LineCardinality.memo[EndoFn[String]] {
      case SingleLine => preprocessSL(_).asString
      case MultiLine  => preprocessML(_).asString
    }
  }

  def liveCorrect(text: Text.Generic): EndoFn[String] =
    preprocessor(text.lineCardinality)

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
