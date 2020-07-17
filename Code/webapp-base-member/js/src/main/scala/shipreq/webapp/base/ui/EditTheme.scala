package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.feature.PreviewFeature.Position
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.jsfacade.ReactCollapse
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

  sealed trait Font

  object Font {
    case object Default extends Font
    case object Monospace extends Font

    implicit def univEq: UnivEq[Font] = UnivEq.derive
    implicit val reusability: Reusability[Font] = Reusability.derive
    val values = AdtMacros.adtValues[Font]
  }

  def autosizeTextareaProps(mode    : Mode,
                            position: Option[Position],
                            enabled : Enabled,
                            validity: Validity,
                            value   : String,
                            tagMod  : TagMod,
                            font    : Font = Font.Default): TagMod =
    TagMod(
      *.textEditor((validity, position, mode, font)),
      ^.value := value,
      ^.disabled := enabled.is(Disabled),
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

  sealed trait WhenInTransit
  object WhenInTransit {
    case object ReadOnlyViewWithSpinner extends WhenInTransit
    case object DisableEditor           extends WhenInTransit

    implicit def univEq: UnivEq[WhenInTransit] = UnivEq.derive
    implicit def reusability: Reusability[WhenInTransit] = Reusability.by_==
  }

  final case class Style(position     : Position,
                         openPreview  : OpenPreview,
                         whenInTransit: WhenInTransit)

  object Style {

    val default = Style(
      Position.Under,
      OpenPreview.Minimally,
      WhenInTransit.ReadOnlyViewWithSpinner,
    )

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
            val show =
              previewRW.read.status match {
                case Some(_) => previewRW.read.showPreview(previewWantOpen) // using state to avoid jitter while type
                case None    => previewWantOpen
              }
            val preview = Layout.Preview(style.position, isShown = show, collapsible = true)
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

  /** no preview, fullscreen, or font */
  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod): VdomNode =
    renderEditor(
      status          = status,
      editor          = (_, v) => editor(v),
      readOnlyView    = readOnlyView,
      instructions    = instructions,
      style           = Style.default,
      previewRW       = PreviewFeature.ReadWrite.Single.neverShow,
      previewWantOpen = false,
      previewBody     = EmptyVdom,
    )

  /** no fullscreen or font */
  def renderEditor(status            : EditorStatus,
                   editor            : (Enabled, Validity) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : => TagMod,
                   style             : Style,
                   previewRW         : PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode =
    renderEditor(
      status             = status,
      optionalFullscreen = None,
      editor             = (_, e, v) => editor(e, v),
      readOnlyView       = readOnlyView,
      instructions       = _ => instructions,
      style              = style,
      font               = Font.Default,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = previewBody,
    )


  /** full */
  def renderEditor(status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Layout, Enabled, Validity) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : Option[OptionalFullscreen.Ctx] => TagMod,
                   style             : Style,
                   font              : Font,
                   previewRW         : PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode = {

    def renderActive(error: Option[TagMod], enabled: Enabled, modInstructions: TagMod => TagMod) = {
      def go(fullscreen: Option[OptionalFullscreen.Ctx]): VdomNode = {

        val layout =
          determineLayout(
            style           = style,
            previewRW       = previewRW,
            previewWantOpen = previewWantOpen,
            fullscreen      = fullscreen,
          )

        this.renderActive(
          editorFn        = editor(_, enabled, _),
          defaultPosition = style.position,
          instructions    = modInstructions(instructions(fullscreen)),
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
        renderActive(None, Enabled, identity)

      case EditorStatus.Invalid(err) =>
        renderActive(Some(err), Enabled, identity)

      case EditorStatus.InTransit =>
        // This is correct and guarded by tests in ReqDetailTest that confirm fullscreen is closed on commit, and that
        // the fullscreen button is disabled.
        val mode = Mode.Inline

        style.whenInTransit match {
          case WhenInTransit.ReadOnlyViewWithSpinner =>
            <.div(*.textEditor((*.EditorState.InTransit, None, mode, font)),
              <.div(spinner),
              <.div(*.textEditorInTransitValue, readOnlyView))

          case WhenInTransit.DisableEditor =>
            // We're rendering the instructions here with visibility=hidden because
            // 1. We don't want the layout to change. Users shouldn't see the buttons move and up down; everything
            //    should stay where it is.
            // 2. The instructions change and become incorrect. This is because commit and abort callbacks change from
            //    Some to None, but other instructions (like alt-enter to commit and close) remain.
            // 3. Even if they were correct, they're irrelevant. A request is in progress -- they can't follow those
            //    instructions anymore.
            renderActive(None, Disabled, i => <.span(^.visibility.hidden, i))
        }

      case EditorStatus.AsyncError(err, _, _) =>
        // As described above, this is safe in that we don't have to worry about fullscreen css;
        // it's always Mode.Inline here.
        renderActive(Some(err), Enabled, identity)
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
            ReactCollapse(previewShown)(preview)
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
            ReactCollapse(previewShown)(preview)
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
