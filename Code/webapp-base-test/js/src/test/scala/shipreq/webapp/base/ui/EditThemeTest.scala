package shipreq.webapp.base.ui

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.Validity
import shipreq.webapp.base.feature.PreviewFeature.{Position, Status}
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.test.RenderTestUtil._
import shipreq.webapp.base.test.TestOptionalFullscreen
import shipreq.webapp.base.ui.EditTheme.{OpenPreview, Style}
import utest._

object EditThemeTest extends TestSuite {

  private def renderBasicEditor(status: EditorStatus): VdomNode = {

    def editor(validity: Validity): VdomElement = {
      val autosizeProps = EditTheme.autosizeTextareaProps(
        mode     = EditTheme.Mode.Inline,
        position = Some(EditTheme.Style.default.position),
        validity = validity,
        value    = "{editor.value}",
        tagMod   = EmptyVdom)
      AutosizeTextarea.Component(autosizeProps)
    }

    EditTheme.renderEditor(
      status       = status,
      editor       = editor,
      readOnlyView = "{readOnlyView}",
      instructions = "{instructions}",
    )
  }

  private def renderTextEditor(status         : EditorStatus,
                               style          : Style,
                               previewState   : PreviewFeature.State.Single,
                               previewWantOpen: => Boolean,
                               position       : EditTheme.Layout => Option[Position],
                               fullscreen     : Option[OptionalFullscreen] = None,
                              ): VdomNode = {

    val previewRW: PreviewFeature.ReadWrite.Single =
      PreviewFeature.ReadWrite.Single(
        PreviewFeature.Read.Single(previewState),
        PreviewFeature.Write.Single.doNothing)

    def editor(layout: EditTheme.Layout, validity: Validity): VdomElement = {
      val autosizeProps = EditTheme.autosizeTextareaProps(
        mode     = layout.mode,
        position = position(layout),
        validity = validity,
        value    = "{editor.value}",
        tagMod   = EmptyVdom)
      AutosizeTextarea.Component(autosizeProps)
    }

    EditTheme.renderEditor(
      status             = status,
      optionalFullscreen = fullscreen,
      editor             = editor,
      readOnlyView       = "{readOnlyView}",
      instructions       = _ => "{instructions}",
      style              = style,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = "{previewBody}",
    )
  }

  private def fullscreenOn: Option[OptionalFullscreen] = {
    val t = TestOptionalFullscreen()
    t.currentlyFullscreen = true
    Some(t)
  }

  override def tests = Tests {

    // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████
    "tagEditor" - {

      "valid" - {
        val a = renderBasicEditor(EditorStatus.Valid(None))
        // editor : 1.1.1
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <textarea class="BaseStyles-textEditor-18">
            |      {editor.value}
            |    </textarea>
            |    {instructions}
            |  </div>
            |  <div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "invalid" - {
        val a = renderBasicEditor(EditorStatus.Invalid("{error}"))
        // editor : 1.1.1
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <textarea class="BaseStyles-textEditor-17">
            |      {editor.value}
            |    </textarea>
            |    <div class="BaseStyles-errorAndInstructions-2">
            |      <div class="ui label pointing red ">
            |        {error}
            |      </div>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }
    }

    // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████
    "reqTableTitle" - {

      val style = Style.default
      val position = (_: EditTheme.Layout).position

      "previewClosed" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Closed),
          previewWantOpen = false,
          position        = position,
        )
        // editor : 1.1.1
        // preview: 1.2
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <textarea class="BaseStyles-textEditor-18">
            |      {editor.value}
            |    </textarea>
            |    {instructions}
            |  </div>
            |  <div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewOpen" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.NeedOpen),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.1
        // preview: 1.2(.1)
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <textarea class="BaseStyles-textEditor-18">
            |      {editor.value}
            |    </textarea>
            |    {instructions}
            |  </div>
            |  <div>
            |    <div class="ReactCollapse--collapse" style="height:auto;overflow:initial">
            |      <div class="ReactCollapse--content">
            |        <div class="BaseStyles-richTextPreview-4 ui segments raised">
            |          <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |            Preview
            |          </div>
            |          <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |            <div class="BaseStyles-richTextPreviewBodyInner-2">
            |              {previewBody}
            |            </div>
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }
    }

    // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████
    "reqTableCustomText" - {

      val style = Style(Position.Under, OpenPreview.MinimallyWithControls)
      val position = (_: EditTheme.Layout).positionIfShown

      "previewClosed" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Closed),
          previewWantOpen = false,
          position        = position,
        )
        // editor : 1.1.2.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 transition fade left out" style="visibility:hidden">
            |      <button title="Show preview" class="ui button icon  blue ">
            |        <i class="icon window restore" style="margin:0"></i>
            |      </button>
            |    </div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-12">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div>
            |    <div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewOpen" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.NeedOpen),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-18">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 BaseStyles-previewButtonsWhenUnder transition fade left out" style="visibility:hidden">
            |      <button title="Move right" class="ui button icon  blue ">
            |        <i class="icon angle double right" style="margin:0"></i>
            |      </button>
            |      <button title="Hide preview" class="ui button icon  blue ">
            |        <i class="icon close" style="margin:0"></i>
            |      </button>
            |    </div>
            |    <div>
            |      <div class="ReactCollapse--collapse" style="height:auto;overflow:initial">
            |        <div class="ReactCollapse--content">
            |          <div class="BaseStyles-richTextPreview-4 ui segments raised">
            |            <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |              Preview
            |            </div>
            |            <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |              <div class="BaseStyles-richTextPreviewBodyInner-2">
            |                {previewBody}
            |              </div>
            |            </div>
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewNeededOpen" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.NeededOpen),
          previewWantOpen = false,
          position        = position,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-18">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 BaseStyles-previewButtonsWhenUnder transition fade left out" style="visibility:hidden">
            |      <button title="Move right" class="ui button icon  blue ">
            |        <i class="icon angle double right" style="margin:0"></i>
            |      </button>
            |      <button title="Hide preview" class="ui button icon  blue ">
            |        <i class="icon close" style="margin:0"></i>
            |      </button>
            |    </div>
            |    <div>
            |      <div class="ReactCollapse--collapse" style="height:auto;overflow:initial">
            |        <div class="ReactCollapse--content">
            |          <div class="BaseStyles-richTextPreview-4 ui segments raised">
            |            <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |              Preview
            |            </div>
            |            <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |              <div class="BaseStyles-richTextPreviewBodyInner-2">
            |                {previewBody}
            |              </div>
            |            </div>
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewManualUnder" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Manual(true, Position.Under)),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-18">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 BaseStyles-previewButtonsWhenUnder transition fade left out" style="visibility:hidden">
            |      <button title="Move right" class="ui button icon  blue ">
            |        <i class="icon angle double right" style="margin:0"></i>
            |      </button>
            |      <button title="Hide preview" class="ui button icon  blue ">
            |        <i class="icon close" style="margin:0"></i>
            |      </button>
            |    </div>
            |    <div>
            |      <div class="BaseStyles-richTextPreview-4 ui segments raised">
            |        <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |          Preview
            |        </div>
            |        <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |          <div class="BaseStyles-richTextPreviewBodyInner-2">
            |            {previewBody}
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewManualRight" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Manual(true, Position.Right)),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorLeftPreviewRight-2">
            |  <div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-15">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 BaseStyles-previewButtonsWhenRight transition fade left out" style="visibility:hidden">
            |      <button title="Hide preview" class="ui button icon  blue ">
            |        <i class="icon close" style="margin:0">
            |        </i>
            |      </button>
            |      <button title="Move down" class="ui button icon  blue ">
            |        <i class="icon angle double down" style="margin:0">
            |        </i>
            |      </button>
            |    </div>
            |    <div style="display:inline">
            |      <div class="BaseStyles-richTextPreview-3 ui segments raised">
            |        <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |          Preview
            |        </div>
            |        <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |          <div class="BaseStyles-richTextPreviewBodyInner-1">
            |            {previewBody}
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewManualHide" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Manual(false, Position.Under)),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.2.1
        val e =
          """<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 transition fade left out" style="visibility:hidden">
            |      <button title="Show preview" class="ui button icon  blue ">
            |        <i class="icon window restore" style="margin:0">
            |        </i>
            |      </button>
            |    </div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-12">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |</div>
            |
            |""".stripMargin
        assertRender(a, e)
      }

      "fullscreenPreviewManualRight" - {
        val a = renderTextEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          fullscreen      = fullscreenOn,
          previewState    = Some(Status.Manual(true, Position.Right)),
          previewWantOpen = true,
          position        = position,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """<div class="BaseStyles-textEditorLeftPreviewRight-2">
            |  <div>
            |    <div>
            |      <textarea class="BaseStyles-textEditor-15">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |  </div>
            |  <div class="BaseStyles-previewToggleWrapper1">
            |    <div class="BaseStyles-previewToggleWrapper2 BaseStyles-previewButtonsWhenRight transition fade left out" style="visibility:hidden">
            |      <button title="Hide preview" class="ui button icon  blue ">
            |        <i class="icon close" style="margin:0">
            |        </i>
            |      </button>
            |      <button title="Move down" class="ui button icon  blue ">
            |        <i class="icon angle double down" style="margin:0">
            |        </i>
            |      </button>
            |    </div>
            |    <div style="display:inline">
            |      <div class="BaseStyles-richTextPreview-3 ui segments raised">
            |        <div class="BaseStyles-richTextPreviewHeader ui segment inverted">
            |          Preview
            |        </div>
            |        <div class="BaseStyles-richTextPreviewBodyOuter ui segment">
            |          <div class="BaseStyles-richTextPreviewBodyInner-1">
            |            {previewBody}
            |          </div>
            |        </div>
            |      </div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }
    }

  }
}
