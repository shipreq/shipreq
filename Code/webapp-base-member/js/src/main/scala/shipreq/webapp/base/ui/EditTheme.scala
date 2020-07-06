package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.html
import scala.annotation.elidable
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

  final case class Layout(mode    : Mode,
                          preview : Option[Layout.Preview],
                          controls: Option[Layout.Controls]) {

    def position: Option[Position] =
      preview.map(_.position)

    def positionIfShown: Option[Position] =
      preview.filter(_.isShown).map(_.position)
  }

  object Layout {
    final case class Preview(position   : Position,
                             isShown    : Boolean,
                             collapsible: Boolean) {
      @elidable(elidable.INFO)
      override def toString = s"Preview($position, isShown = $isShown, collapsible = $collapsible)"
    }

    final case class Controls(around               : Controls.Around,
                              showControlsInitially: Boolean)
    object Controls {

      sealed trait Around
      object Around {
        case object Editor  extends Around
        case object Preview extends Around
      }
    }
  }

  private def determineLayout(style          : Style,
                              previewRW      : PreviewFeature.ReadWrite.Single,
                              previewWantOpen: => Boolean,
                              fullscreen     : Option[OptionalFullscreen.Ctx]): Layout = {
    import Layout.Controls.Around

    val mode = Mode.derive(fullscreen)

    mode match {
      case Mode.Inline =>
        style.openPreview match {

          case OpenPreview.Minimally =>
            val show    = previewRW.read.showPreview(previewWantOpen)
            val preview = Layout.Preview(position = style.position, isShown = show, collapsible = true)
            Layout(mode, Some(preview), None)

          case OpenPreview.MinimallyWithControls =>
            val show        = previewRW.read.showPreview(previewWantOpen)
            val position    = previewRW.read.position(style.position)
            val around      = if (show) Around.Preview else Around.Editor
            val controls    = Some(Layout.Controls(around, showControlsInitially = false))
            val preview     =
              if (previewRW.read.isManual)
                Option.when(show)(Layout.Preview(position, isShown = show, collapsible = false))
              else
                Some(Layout.Preview(position, isShown = show, collapsible = true))
            Layout(mode, preview, controls)

          case OpenPreview.ShowWithControls =>
            val show     = previewRW.read.showManuallyControlledPreview(true)
            val position = previewRW.read.position(style.position)
            val preview  = Option.when(show)(Layout.Preview(position, isShown = show, collapsible = false))
            val around   = if (show) Around.Preview else Around.Editor
            val controls = Some(Layout.Controls(around, showControlsInitially = true))
            Layout(mode, preview, controls)

          case OpenPreview.WhenWanted =>
            val show    = previewRW.read.showPreview(previewWantOpen)
            val preview = Layout.Preview(style.position, isShown = show, collapsible = false)
            Layout(mode, Some(preview), None)

          case OpenPreview.Always =>
            val preview = Layout.Preview(style.position, isShown = true, collapsible = false)
            Layout(mode, Some(preview), None)

          case OpenPreview.Never =>
            Layout(mode, None, None)
        }

      case Mode.Fullscreen =>
        val defaultShow: Boolean =
          style.openPreview match {
            case OpenPreview.Minimally
               | OpenPreview.MinimallyWithControls
               | OpenPreview.WhenWanted
               | OpenPreview.ShowWithControls
               | OpenPreview.Always                => true
            case OpenPreview.Never                 => false
          }
        val show     = previewRW.read.showManuallyControlledPreview(defaultShow)
        val position = previewRW.read.position(style.position)
        val preview  = Option.when(show)(Layout.Preview(position, isShown = show, collapsible = false))
        val around   = if (show) Around.Preview else Around.Editor
        val controls = Some(Layout.Controls(around, showControlsInitially = false))
        Layout(mode, preview, controls)
    }
  }

  // ===================================================================================================================

  /** no preview or fullscreen */
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

  /** no fullscreen */
  def renderEditor(status            : EditorStatus,
                   editor            : Validity => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : => TagMod,
                   style             : Style,
                   previewRW         : PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode =
    renderEditor(
      status             = status,
      optionalFullscreen = None,
      editor             = (_, v) => editor(v),
      readOnlyView       = readOnlyView,
      instructions       = _ => instructions,
      style              = style,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = previewBody,
    )


  /** full */
  def renderEditor(status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Layout, Validity) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : Option[OptionalFullscreen.Ctx] => TagMod,
                   style             : Style,
                   previewRW         : PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode = {

    def renderActive(error: Option[TagMod]) = {
      def go(fullscreen: Option[OptionalFullscreen.Ctx]): VdomNode = {

        val layout =
          determineLayout(
            style           = style,
            previewRW       = previewRW,
            previewWantOpen = previewWantOpen,
            fullscreen      = fullscreen,
          )

        this.renderActive(
          editorFn        = editor,
          defaultPosition = style.position,
          instructions    = instructions(fullscreen),
          previewRW       = previewRW,
          previewBody     = previewBody,
          layout          = layout,
          error           = error,
        )
      }

      optionalFullscreen match {
        case None    => go(None)
        case Some(f) => f(ctx => go(Some(ctx)))
      }
    }

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        renderActive(None)

      case EditorStatus.Invalid(err) =>
        renderActive(Some(err))

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

  private def renderActive(editorFn       : (Layout, Validity) => VdomElement,
                           defaultPosition: Position,
                           instructions   : TagMod,
                           previewRW      : PreviewFeature.ReadWrite.Single,
                           previewBody    : => VdomNode,
                           layout         : Layout,
                           error          : Option[TagMod]): VdomNode = {
    import Layout._
    import Layout.Controls.Around
    import Position._

    val validity = Valid.when(error.isEmpty)
    val editor   = editorFn(layout, validity)

    def renderPreview(position: Position, mode: Mode) =
      this.renderPreview(position, mode, previewBody)

    def errorAndInstructions(position: Position): TagMod =
      error match {
        case None    => instructions
        case Some(e) => <.div(*.errorAndInstructions(position), *.errorPointingUp(e), instructions)
      }

    layout match {

      // Preview under, no controls
      case Layout(mode, Some(Preview(pos @ Under, previewShown, collapsible)), None) =>
        val preview = renderPreview(pos, mode)

        val collapsiblePreview: VdomNode =
          if (collapsible)
            previewRW.reactCollapse(previewShown)(preview)
          else
            preview

        <.div(*.textEditorTopPreviewUnder(mode),
          <.div(^.key := "e",
            editor,
            errorAndInstructions(pos)),
          <.div(^.key := "p",
            collapsiblePreview))

      // Preview under, with controls
      case Layout(mode, Some(Preview(pos @ Under, previewShown, collapsible)), Some(Controls(around, showControlsInitially))) =>
        val preview = renderPreview(pos, mode)

        val collapsiblePreview: VdomNode =
          if (collapsible)
            previewRW.reactCollapse(previewShown)(preview)
          else
            preview

        val previewControls =
          previewRW.manualControls(
            defaultPosition       = pos,
            previewIsShown        = previewShown,
            showControlsInitially = showControlsInitially,
          )

        around match {

          case Around.Editor =>
            <.div(*.textEditorTopPreviewUnder(mode),
              previewControls(^.key := 1,
                <.div(^.key := "e",
                  editor,
                  errorAndInstructions(pos))),
              <.div(^.key := "2",
                <.div(^.key := "p",
                  collapsiblePreview)))

          case Around.Preview =>
            <.div(*.textEditorTopPreviewUnder(mode),
              <.div(^.key := 1,
                <.div(^.key := "e",
                  editor,
                  errorAndInstructions(pos))),
              previewControls(^.key := 2,
                <.div(^.key := "p",
                  collapsiblePreview)))
        }

      // Preview right, with controls
      case Layout(mode, Some(Preview(pos @ Right, previewShown, _)), Some(Controls(around, showControlsInitially))) =>
        val preview = renderPreview(pos, mode)

        val previewControls =
          previewRW.manualControls(
            defaultPosition       = pos,
            previewIsShown        = previewShown,
            showControlsInitially = showControlsInitially,
          )

        around match {

          case Around.Editor =>
            <.div(*.textEditorLeftPreviewRight(mode),
              previewControls(^.key := 1,
                <.div(^.key := "e",
                  editor,
                  errorAndInstructions(pos))),
              <.div(^.key := "2",
                <.div(^.key := "p",
                  preview)))

          case Around.Preview =>
            <.div(*.textEditorLeftPreviewRight(mode),
              <.div(^.key := 1,
                <.div(^.key := "e",
                  editor,
                  errorAndInstructions(pos))),
              previewControls(^.key := 2,
                <.div(^.key := "p",
                  ^.display.inline,
                  preview)))
        }

      // Preview right, without controls
      case Layout(mode, Some(Preview(pos @ Right, true, _)), None) =>
        val preview = renderPreview(pos, mode)

        <.div(*.textEditorLeftPreviewRight(mode),
          <.div(
            editor,
            errorAndInstructions(pos)),
          <.div(
            preview))

      // Preview manually off
      case Layout(mode, None, Some(Controls(_, showControlsInitially))) =>
        val previewControls =
          previewRW.manualControls(
            defaultPosition       = defaultPosition,
            previewIsShown        = false,
            showControlsInitially = showControlsInitially,
          )

        <.div(*.textEditorTopPreviewUnder(mode),
          previewControls(^.key := 1,
            <.div(^.key := "e",
              editor,
              errorAndInstructions(defaultPosition))))

      // No preview or controls
      case Layout(_, None, None)
         | Layout(_, Some(Preview(Right, false, _)), None) =>
        <.div(
          editor,
          errorAndInstructions(defaultPosition))
    }
  }

  private def renderPreview(position   : Position,
                            mode       : Mode,
                            body       : => VdomNode): VdomNode =
    <.div(*.richTextPreview((position, mode)),
      <.div(*.richTextPreviewHeader, "Preview"),
      <.div(*.richTextPreviewBodyOuter,
        <.div(*.richTextPreviewBodyInner(position), body)))
}
