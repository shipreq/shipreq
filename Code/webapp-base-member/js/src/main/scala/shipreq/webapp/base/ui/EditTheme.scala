package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.feature.PreviewFeature.Position
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.{BaseStyles => *}

object EditTheme {

  private[this] val editableInline: TagMod =
    TagMod(
      *.inlineEdit,
      ^.title := UiText.doubleClickToEdit)

  def editableInline(startEdit: Callback): TagMod =
    TagMod(editableInline, ^.onDblClick --> startEdit)

  def editableInline(startEdit: Option[Callback]): TagMod =
    startEdit.fold(TagMod.empty)(editableInline(_))

  def autosizeTextareaProps(mode    : Mode,
                            position: Option[Position],
                            validity: Validity,
                            value   : String,
                            tagMod  : TagMod): TagMod =
    TagMod(
      *.textEditor((validity, position, mode)),
      ^.value := value,
      tagMod)

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _], autoFocus: CallbackTo[Boolean]): CallbackOption[Unit] =
    for {
      af <- autoFocus
      _  <- CallbackOption.require(af)
      _  <- onTextareaEditorMount(ref)
    } yield ()

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _]): CallbackOption[Unit] =
    ref.get.flatMap(m => onTextareaEditorMount(m.getDOMNode))

  def onTextareaEditorMount(cd: ComponentDom): CallbackOption[Unit] =
    (for {
      h <- CallbackOption.liftOption(cd.toHtml)
    } yield {
      val t = h.domCast[html.TextArea]
      val l = t.value.length
      t.setSelectionRange(l, l)
    }).toCallback

  val spinner: VdomTag =
    Icon.CircleNotched.loading.tag(^.marginRight := "0")

  sealed trait Mode
  object Mode {
    case object Inline extends Mode
    case object Fullscreen extends Mode

    def derive(fullscreen: Option[OptionalFullscreen.Ctx]): Mode =
      if (fullscreen.exists(_.currentlyFullscreen))
        EditTheme.Mode.Fullscreen
      else
        EditTheme.Mode.Inline

    implicit def univEq: UnivEq[Mode] = UnivEq.derive
    implicit def reusability: Reusability[Mode] = Reusability.by_==
    def values = AdtMacros.adtValues[Mode]
  }

  sealed trait OpenPreview
  object OpenPreview {

    /** Follows the logic described in [[PreviewFeature]] to only show preview when required, and with minimal change */
    case object Minimally extends OpenPreview

    /** Same as [[Minimally]] except that it can be manually toggled on/off. */
    case object MinimallyWithControls extends OpenPreview

    /** Preview always shown. */
    case object Always extends OpenPreview

    /** Preview never shown. */
    case object Never extends OpenPreview

    /** Preview shown anytime `wantOpen` is `true`. */
    case object WhenWanted extends OpenPreview

    /** Preview shown by default, and can be manually toggled on/off. */
    case object ShowWithControls extends OpenPreview

    implicit def univEq: UnivEq[OpenPreview] = UnivEq.derive
    implicit def reusability: Reusability[OpenPreview] = Reusability.by_==
  }

  final case class Style(position: Position, openPreview: OpenPreview)

  object Style {
    val default = Style(Position.Under, OpenPreview.Minimally)

    implicit def univEq: UnivEq[Style] = UnivEq.derive
    implicit def reusability: Reusability[Style] = Reusability.byRef || Reusability.derive
  }

  // ===================================================================================================================

  /** helper for no preview or fullscreen */
  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod): VdomNode =
    renderEditor(
      status          = status,
      editor          = editor,
      readOnlyView    = readOnlyView,
      instructions    = instructions,
      style           = Style.default,
      previewRW       = PreviewFeature.ReadWrite.Single.neverShow,
      previewWantOpen = false,
      previewBody     = EmptyVdom,
    )

  /** helper for no fullscreen */
  def renderEditor(status            : EditorStatus,
                   editor            : Validity => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : => TagMod,
                   style             : Style,
                   previewRW         : => PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode =
    renderEditor(
      status             = status,
      optionalFullscreen = None,
      editor             = (v, _, _) => editor(v),
      readOnlyView       = readOnlyView,
      instructions       = _ => instructions,
      style              = style,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = previewBody,
    )

  def renderEditor(status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Validity, Option[Position], Mode) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : Option[OptionalFullscreen.Ctx] => TagMod,
                   style             : Style,
                   previewRW         : => PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode = {

    def renderPreview(position: Position, mode: Mode) =
      this.renderPreview(
        previewRW   = previewRW,
        position    = position,
        openPreview = style.openPreview,
        mode        = mode,
        wantOpen    = previewWantOpen,
        body        = previewBody,
      )

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        val p              = previewRW
        val instructionsFn = instructions
        val editorFn       = editor
        val position       = p.read.position(style.position)

        val renderFn: RenderCmd => RenderResult =
          cmd => {
            import cmd.mode
            val instructions                  = instructionsFn(cmd.fullscreen)
            def editor(allowPreview: Boolean) = editorFn(Valid, Option.when(allowPreview)(position), mode)
            def preview                       = renderPreview(position, mode)

            implicit def noOuter(v: VdomTag): RenderResult =
              RenderResult(identity, v)

            def previewRight =
              <.div(*.textEditorLeftPreviewRight(mode),
                <.div(editor(allowPreview = true), instructions),
                <.div(preview))

            def previewUnder =
              RenderResult(
                outer = <.div(*.textEditorTopPreviewUnder(mode), <.div(editor(allowPreview = true), instructions), _),
                inner = <.div(preview)
              )

            def noPreview =
              <.div(editor(allowPreview = false), instructions)

            if (cmd.allowPreview)
              position match {
                case Position.Right => previewRight
                case Position.Under => previewUnder
              }
            else
              noPreview
          }

        renderActive(
          render             = renderFn,
          defaultPosition    = style.position,
          optionalFullscreen = optionalFullscreen,
          previewRW          = p,
          previewWantOpen    = previewWantOpen,
          openPreview        = style.openPreview,
        )

      case EditorStatus.Invalid(err) =>
        val mode = Mode.Inline
        val pos = Position.Under
        <.div(
          editor(Invalid, Some(pos), mode), // TODO add error background
          *.errorPointingUp(err),
          renderPreview(pos, mode))

      case EditorStatus.AsyncError(err, _, _) =>
        val mode = Mode.Inline
        val pos = Position.Under
        <.div(
          editor(Valid, Some(pos), mode),
          *.errorPointingUp(err),
          renderPreview(pos, mode))

      case EditorStatus.InTransit =>
        <.div(*.textEditor((*.EditorState.InTransit, None, Mode.Inline)),
          <.div(spinner),
          <.div(*.textEditorInTransitValue, readOnlyView))
    }
  }

  // ===================================================================================================================

  private final case class RenderCmd(allowPreview: Boolean,
                                     mode        : Mode,
                                     fullscreen  : Option[OptionalFullscreen.Ctx])

  private final case class RenderResult(outer: VdomNode => VdomNode,
                                        inner: VdomTag) {
    def self = outer(inner)
  }

  private def renderActive(render            : RenderCmd => RenderResult,
                           defaultPosition   : Position,
                           optionalFullscreen: Option[OptionalFullscreen],
                           previewRW         : => PreviewFeature.ReadWrite.Single,
                           previewWantOpen   : => Boolean,
                           openPreview       : OpenPreview): VdomNode = {

    def content(fullscreen: Option[OptionalFullscreen.Ctx]): VdomNode = {
      val mode = Mode.derive(fullscreen)

      def renderWithManualControls(defaultShow: Boolean) = {
        val show  = previewRW.read.showManuallyControlledPreview(defaultShow)
        val cmd   = RenderCmd(show, mode, fullscreen)
        val rr    = render(cmd)
        val inner = previewRW.manualControls(defaultPosition = defaultPosition, previewIsShown = show)(rr.inner)
        rr.outer(inner)
      }

      mode match {
        case Mode.Inline =>
          openPreview match {
            case OpenPreview.MinimallyWithControls => renderWithManualControls(previewWantOpen)
            case OpenPreview.ShowWithControls      => renderWithManualControls(true)
            case OpenPreview.Minimally
               | OpenPreview.Always
               | OpenPreview.WhenWanted            => render(RenderCmd(true, mode, fullscreen)).self
            case OpenPreview.Never                 => render(RenderCmd(false, mode, fullscreen)).self
          }

        case Mode.Fullscreen =>
          renderWithManualControls(true)
      }
    }

    optionalFullscreen match {
      case None    => content(None)
      case Some(f) => f(ctx => content(Some(ctx)))
    }
  }

  // ===================================================================================================================

  private def renderPreview(previewRW  : => PreviewFeature.ReadWrite.Single,
                            position   : Position,
                            openPreview: OpenPreview,
                            mode       : Mode,
                            wantOpen   : => Boolean,
                            body       : => VdomNode): VdomNode = {
    def render =
      <.div(*.richTextPreview((position, mode)),
        <.div(*.richTextPreviewHeader, "Preview"),
        <.div(*.richTextPreviewBodyOuter,
          <.div(*.richTextPreviewBodyInner(position), body)))

    def manual(defaultShow: Boolean) =
      if (previewRW.read.showManuallyControlledPreview(defaultShow))
        render
      else
        EmptyVdom

    mode match {
      case Mode.Inline =>
        openPreview match {
          case OpenPreview.Minimally             => previewRW.reactCollapse(wantOpen)(render)
          case OpenPreview.MinimallyWithControls => manual(wantOpen)
          case OpenPreview.Always                => render
          case OpenPreview.Never                 => EmptyVdom
          case OpenPreview.WhenWanted            => PreviewFeature.ReadWrite.Single.show(wantOpen).reactCollapse(wantOpen)(render)
          case OpenPreview.ShowWithControls      => manual(true)
        }

      case Mode.Fullscreen =>
        manual(true)
    }
  }
}
