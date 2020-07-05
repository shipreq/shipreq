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
import shipreq.webapp.base.jsfacade.ReactReversePortal
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

  final class Id(node: ReactReversePortal.Node) {
    private val props = ReactReversePortal.Props(node)

    private[EditTheme] def wrap(node: VdomNode): VdomNode =
      ReactReversePortal.InPortal(props)(node)

    private[EditTheme] val render: VdomNode =
      ReactReversePortal.OutPortal(props)
  }

  object Id {
    private[EditTheme] def apply(): Id =
      new Id(ReactReversePortal.Instance.createHtmlPortalNode())
  }

  final class Ids(private[EditTheme] val editor : Id,
                  private[EditTheme] val preview: Id)

  object Ids {
    def apply(): Ids =
      new Ids(Id(), Id())
  }

  /** helper for no preview or fullscreen */
  def renderEditor(ids         : Ids,
                   status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod): VdomNode =
    renderEditor(
      ids             = ids,
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
  def renderEditor(ids               : Ids,
                   status            : EditorStatus,
                   editor            : Validity => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : => TagMod,
                   style             : Style,
                   previewRW         : => PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode =
    renderEditor(
      ids                = ids,
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

  def renderEditor(ids               : Ids,
                   status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Validity, Option[Position], Mode) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : Option[OptionalFullscreen.Ctx] => TagMod,
                   style             : Style,
                   previewRW         : => PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode = {

    def renderPreview(position: Position, mode: Mode): VdomNode =
      ids.preview.wrap(
        this.renderPreview(
          previewRW   = previewRW,
          position    = position,
          openPreview = style.openPreview,
          mode        = mode,
          wantOpen    = previewWantOpen,
          body        = previewBody,
        )
      )

    def renderActive(error: Option[TagMod]) = {
      val p              = previewRW
      val instructionsFn = instructions
      val editorFn       = editor
      val position       = p.read.position(style.position)

      val renderFn: RenderCmd => RenderResult =
        cmd => {
          import cmd.mode
          val instructions                  = instructionsFn(cmd.fullscreen)
          def editor(allowPreview: Boolean) = ids.editor.wrap(editorFn(Valid, Option.when(allowPreview)(position), mode))
          def preview                       = renderPreview(position, mode)

          val errorAndInstructions: TagMod =
            error match {
              case None    => instructions
              case Some(e) => <.div(*.errorAndInstructions(position), *.errorPointingUp(e), instructions)
            }

          def renderWithPreviewRight =
            RenderResult.noOuter(
              <.div(*.textEditorLeftPreviewRight(mode),
                <.div(
                  editor(allowPreview = true),
                  ids.editor.render,
                  errorAndInstructions),
                <.div(preview, ids.preview.render)))

          def renderWithPreviewUnder =
            RenderResult(
              outer = <.div(*.textEditorTopPreviewUnder(mode),
                        <.div(
                          editor(allowPreview = true),
                          ids.editor.render,
                          errorAndInstructions),
                        _),
              inner = <.div(preview, ids.preview.render)
            )

          def renderWithoutPreview =
            RenderResult.noOuter(
              <.div(
                editor(allowPreview = false),
                ids.editor.render,
                errorAndInstructions))

          if (cmd.allowPreview)
            position match {
              case Position.Under => renderWithPreviewUnder
              case Position.Right => renderWithPreviewRight
            }
          else
            renderWithoutPreview
        }

      this.renderActive(
        render             = renderFn,
        defaultPosition    = style.position,
        optionalFullscreen = optionalFullscreen,
        previewRW          = p,
        previewWantOpen    = previewWantOpen,
        openPreview        = style.openPreview,
      )
    }

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        renderActive(None)

      case EditorStatus.Invalid(err) =>
        // This is (still) hardcoded to display the preview under text.
        // Since adding the ability to control the preview and its position, the only places it's used are custom text
        // fields which are never invalid. Therefore I'm choosing to upgrading this logic to consider Style until later
        // when (if) it's actually needed.
        val mode = Mode.Inline
        val pos = Position.Under

        <.div(
          ids.editor.wrap(editor(Invalid, Some(pos), mode)), // TODO add error background
          ids.editor.render,
          *.errorPointingUp(err),
          renderPreview(pos, mode),
          ids.preview.render)

      case EditorStatus.InTransit =>
        // This is correct and guarded by tests in ReqDetailTest that confirm fullscreen is closed on commit, and that
        // the fullscreen button is disabled.
        val mode = Mode.Inline
        <.div(*.textEditor((*.EditorState.InTransit, None, mode)),
          <.div(spinner),
          <.div(*.textEditorInTransitValue, readOnlyView))

      case EditorStatus.AsyncError(err, _, _) =>
        // As described above, this is safe in that we don't have to worry about fullscreen css;
        // it's always Mode.Inline here.
        renderActive(Some(err))
    }
  }

  // ===================================================================================================================

  private final case class RenderCmd(mode        : Mode,
                                     fullscreen  : Option[OptionalFullscreen.Ctx],
                                     allowPreview: Boolean)

  private final case class RenderResult(outer: VdomNode => VdomNode,
                                        inner: VdomTag) {
    def self = outer(inner)
  }

  private object RenderResult {
    def noOuter(inner: VdomTag): RenderResult =
      apply(identity, inner)
  }

  private def renderActive(render            : RenderCmd => RenderResult,
                           defaultPosition   : Position,
                           optionalFullscreen: Option[OptionalFullscreen],
                           previewRW         : => PreviewFeature.ReadWrite.Single,
                           previewWantOpen   : => Boolean,
                           openPreview       : OpenPreview): VdomNode = {

    def content(fullscreen: Option[OptionalFullscreen.Ctx]): VdomNode = {
      val mode = Mode.derive(fullscreen)

      def renderWithManualControls(defaultShow: Boolean, showControlsInitially: Boolean) = {
        val show  = previewRW.read.showManuallyControlledPreview(defaultShow)
        val cmd   = RenderCmd(mode, fullscreen, allowPreview = show)
        val rr    = render(cmd)
        val inner = previewRW.manualControls(
                      defaultPosition       = defaultPosition,
                      previewIsShown        = show,
                      showControlsInitially = showControlsInitially,
                    )(rr.inner)
        rr.outer(inner)
      }

      mode match {
        case Mode.Inline =>
          openPreview match {
            case OpenPreview.MinimallyWithControls => renderWithManualControls(defaultShow = previewWantOpen, showControlsInitially = false)
            case OpenPreview.ShowWithControls      => renderWithManualControls(defaultShow = true, showControlsInitially = true)
            case OpenPreview.Minimally
               | OpenPreview.Always
               | OpenPreview.WhenWanted            => render(RenderCmd(mode, fullscreen, allowPreview = true)).self
            case OpenPreview.Never                 => render(RenderCmd(mode, fullscreen, allowPreview = false)).self
          }

        case Mode.Fullscreen =>
          renderWithManualControls(defaultShow = true, showControlsInitially = false)
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
