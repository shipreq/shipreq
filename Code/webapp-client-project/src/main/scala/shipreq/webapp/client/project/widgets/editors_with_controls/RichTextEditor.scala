package shipreq.webapp.client.project.widgets.editors_with_controls

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Enabled, PotentialChange, Validity}
import shipreq.webapp.base.feature.{EditorStatus, ScrollSyncFeature}
import shipreq.webapp.base.lib._
import shipreq.webapp.base.util.{KeyHandlers, PreProcessor, TaskRepeater}
import shipreq.webapp.client.project.feature.EditorFeature.PotentialValueAcceptor
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.client.project.widgets.editors_with_controls.RichTextEditor.hardcodedLive
import shipreq.webapp.member.feature.AutoCompleteFeature._
import shipreq.webapp.member.feature.{EditControlsFeature, PreviewFeature}
import shipreq.webapp.member.jsfacade.{ScrollIntoViewIfNeeded, TextFieldEdit}
import shipreq.webapp.member.project.data
import shipreq.webapp.member.project.data.derivation.NaTags
import shipreq.webapp.member.project.data.{Optional => _, _}
import shipreq.webapp.member.project.text.Atom.TypeGroup
import shipreq.webapp.member.project.text.Text.Equality._
import shipreq.webapp.member.project.text._
import shipreq.webapp.member.ui.OptionalFullscreen

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
    val abortVerb         : String
    val commitVerb        : String
    val preview           : PreviewFeature.ReadWrite.Single
    val extraControls     : EditControlsFeature.ExtraControls
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
                      abortVerb         : String,
                      autoFocus         : Boolean,
                      commitFn          : Option[Optional.CommitFn],
                      commitVerb        : String,
                      editorStyle       : EditControlsFeature.Style,
                      preview           : PreviewFeature.ReadWrite.Single,
                      preEditValue      : Option[text.OptionalText],
                      extraControls     : EditControlsFeature.ExtraControls,
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
    type CommitFn = text.OptionalText => Callback
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
                      abortVerb         : String,
                      autoFocus         : Boolean,
                      commitFn          : Option[NonEmpty.CommitFn],
                      commitVerb        : String,
                      editorStyle       : EditControlsFeature.Style,
                      preview           : PreviewFeature.ReadWrite.Single,
                      preEditValue      : Option[text.NonEmptyText],
                      extraControls     : EditControlsFeature.ExtraControls,
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
    type CommitFn = text.NonEmptyText => Callback
  }

  // ===================================================================================================================

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.byRef // because Props are memo'ised in NewEditor

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

    private val scrollSync        = ScrollSyncFeature()
    private val scrollPaneEditor  = scrollSync.newPane(editorDom.asCallback)
    private val scrollPanePreview = scrollSync.newPane()

    private val editControls =
      EditControlsFeature.Controls[Props](text.lineCardinality)
        .abortWhenDefined(_.abortWithConfirmation, _.abortVerb)
        .commitWhenDefined(_.status.getCommit, _.commitVerb)
        .withHelp(RichTextEditorHelp.modalFor(text).show)
        .addDynamicExtras(_.extraControls)

    private val keyHandlerBase =
      KeyHandlers.base(autoCompleteKeyHandlers)

    val textareaConst: TagMod = {
      val onFocus: Callback =
        $.props.flatMap(p => p.preview.onFocus(p.wantPreview)) >> scrollIntoView

      def setValue(text: String, onSuccess: Callback): Callback =
        $.props.flatMap(p =>
          p.status.wrapEdit(p.edit.setState(liveCorrect(text), onSuccess) >>
            p.preview.onEdit(p.wantPreview)))

      val onChange: ReactEventFromTextArea => Callback =
        e => setValue(e.target.value, scrollPaneEditor.syncOthersToThis)

      val onBlur: Callback =
        autoCompleteOnBlur >> $.props.flatMap(_.preview.onBlur)

      TagMod(
        ^.onKeyDown ==> RichTextEditor.wrapSelectionOnKeyDown(text),
        ^.onFocus   --> onFocus,
        ^.onChange  ==> onChange,
        ^.onBlur    --> onBlur,
        ^.onClick   ==> autoCompleteOnClick,
        scrollPaneEditor.tagMod,
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
      val editControls = {
        val e = this.editControls.withMonospace(StateSnapshot.zoomL(State.monospace)(s).setStateVia($))
        (f: Option[OptionalFullscreen.Ctx]) => e.withFullscreenCtx(f)
      }

      def editor(layout       : EditControlsFeature.Layout,
                 enabled      : Enabled,
                 fullscreenCtx: Option[OptionalFullscreen.Ctx],
                 validity     : Validity): VdomElement = {
        val keys = keyHandlerBase(editControls(fullscreenCtx).keyHandlers(p))

        val base = TagMod(
          textareaConst,
          keys,
          ^.autoFocus := p.autoFocus)

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
          editControls(fullscreenCtx).instructions(p)
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
        previewBody        = TagMod(scrollPanePreview.tagMod, richText),
      )
    }

    def onMount(p: Props): Callback =
      EditControlsFeature.onTextareaEditorMount(editorRef, p.autoFocus) >>
        scrollIntoView.when_(p.autoFocus)
  }

  private def initialState(p: Props) =
    State(
      monospace = p.richText.exists(_.containsType[Atom.CodeBlock#CodeBlock]),
    )

  val Component =
    ScalaComponent.builder[Props]("RichTextEditor:" + name)
      .initialStateFromProps(initialState)
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoComplete.install)
      .componentDidMount($ => $.backend.onMount($.props))
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

  implicit val reusabilityState: Reusability[State] =
    Reusability.derive

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

  def wrapSelectionOnKeyDown(text: Text.Generic): ReactKeyboardEventFromTextArea => Callback = {
    val supportsPlainTextMarkup = text.supports(TypeGroup.PlainTextMarkup)
    e => {
      val key          = e.key
      val textarea     = e.target
      val textSelected = textarea.selectionStart != textarea.selectionEnd
      val modified     = e.altKey || e.ctrlKey || e.metaKey
      Callback {
        if (textSelected && !e.defaultPrevented && !modified) {
          val text = textarea.value

          def wrap(prefix: String, _suffix: String = null): Unit = {
            e.preventDefault()
            val suffix = if (_suffix eq null) prefix else _suffix

            def unwrap(da: Int, db: Int): Option[(String, String, String)] = {
              val a = textarea.selectionStart - da
              val b = textarea.selectionEnd + db
              Option.when(a >= 0 && b <= text.length) {
                val s = text.substring(a, b)
                Option.when(s.startsWith(prefix) && s.endsWith(suffix)) {
                  val pre = text.take(a)
                  val mid = s.drop(prefix.length).dropRight(suffix.length)
                  val pst = text.drop(b)
                  (pre, mid, pst)
                }
              }.flatten
            }

            val unwrapped =
              unwrap(0, 0) orElse unwrap(prefix.length, suffix.length)

            unwrapped match {
              case Some((a, b, c)) =>
                TextFieldEdit.set(textarea, a + b + c)
                textarea.setSelectionRange(a.length, a.length + b.length)
              case None =>
                TextFieldEdit.wrapSelection(textarea, prefix, suffix)
            }
          }

          if (!e.isDefaultPrevented() && supportsPlainTextMarkup)
            key match {
              case "/" | "_" | "*" | "~" => wrap(key + key)
              case "`"                   => wrap(key)
              case _                     =>
            }

          if (!e.isDefaultPrevented())
            key match {
              case "'" | "\""            => wrap(key)
              case "("                   => wrap("(", ")")
              case "{"                   => wrap("{", "}")
              case "<"                   => wrap("<", ">")
              case "["                   => wrap("[", "]")
              case _                     =>
            }
        }
      }
    }
  }


  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor("UCT", Text.UseCaseTitle)

  object CodeGroupTitle extends RichTextEditor("CGT", Text.CodeGroupTitle)

  object CustomTextField extends RichTextEditor("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)

  object ManualIssue extends RichTextEditor("MI", Text.ManualIssue)
}
