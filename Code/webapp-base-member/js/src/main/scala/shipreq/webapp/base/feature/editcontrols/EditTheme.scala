package shipreq.webapp.base.feature.editcontrols

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.feature.PreviewFeature.Position
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.jsfacade.ReactCollapse
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.{BaseStyles => *, _}

private[feature] object EditTheme {

  val spinner: VdomTag =
    Icon.CircleNotched.loading.tag(^.marginRight := "0")

  def renderEditor(status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Layout, Enabled, Option[OptionalFullscreen.Ctx], Validity) => VdomElement,
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
          Layout.determine(
            style           = style,
            previewRW       = previewRW,
            previewWantOpen = previewWantOpen,
            fullscreen      = fullscreen,
          )

        this.renderActive(
          editorFn        = editor(_, enabled, fullscreen, _),
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
            <.div(*.textEditor((*.EditorState.InTransit, None, mode, font, Enabled)),
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

  private def renderActive(editorFn       : (Layout, Validity) => VdomElement,
                           defaultPosition: Position,
                           instructions   : TagMod,
                           previewRW      : PreviewFeature.ReadWrite.Single,
                           previewBody    : => VdomNode,
                           layout         : Layout,
                           error          : Option[TagMod]): VdomNode = {
    import Layout.Controls.Around
    import Layout._
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
