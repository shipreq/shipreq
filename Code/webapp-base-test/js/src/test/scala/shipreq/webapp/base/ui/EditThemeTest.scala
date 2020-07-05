package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test.ReactTestUtils
import japgolly.scalajs.react.vdom.html_<^._
import sourcecode.Line
import utest._
import utest.framework.TestPath
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.Validity
import shipreq.webapp.base.feature.PreviewFeature.{Position, Status}
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.ui.EditTheme.{OpenPreview, Style}

object EditThemeTest extends TestSuite {

  private def renderEditor(status         : EditorStatus,
                           style          : Style,
                           previewState   : PreviewFeature.State.Single,
                           previewWantOpen: => Boolean,
                          ): VdomNode = {

    val previewRW: PreviewFeature.ReadWrite.Single =
      PreviewFeature.ReadWrite.Single(
        PreviewFeature.Read.Single(previewState),
        PreviewFeature.Write.Single.doNothing)

    def editor(validity: Validity,
               position: Option[PreviewFeature.Position],
               mode    : EditTheme.Mode): VdomElement = {

      val autosizeProps = EditTheme.autosizeTextareaProps(
        mode     = mode,
        position = position,
        validity = validity,
        value    = "{editor.value}",
        tagMod   = EmptyVdom)
      AutosizeTextarea.Component(autosizeProps)
    }

    EditTheme.renderEditor(
      status             = status,
      optionalFullscreen = None,
      editor             = editor,
      readOnlyView       = "{readOnlyView}",
      instructions       = _ => "{instructions}",
      style              = style,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = "{previewBody}",
    )
  }

  private def assertRender(actual: VdomNode, expect: String, extraNorm: String => String = identity)
                          (implicit l: Line, p: TestPath): Unit = {
    def norm(s: String): String = {
      def addIndent(): String => String = {
        var indent = 0
        s => {
          val closer = s.startsWith("</")
          if (closer && indent > 0)
            indent -= 1
          val s2 = s.indent(indent << 1)
          if (!closer && s.startsWith("<"))
            indent += 1
          s2
        }
      }

      extraNorm(s.trim)
        .replaceAll("\n +", "")
        .replace("<", "\n<")
        .replace(">", ">\n")
        .linesIterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(addIndent())
        .mkString("\n")
    }

    val a = norm(ReactTestUtils.removeReactInternals(ReactDOMServer.renderToStaticMarkup(actual)))
    val e = norm(expect)
    if (a != e) {
      println(s"${Console.RED_B}${Console.WHITE}${p.value.mkString(".")}${Console.RESET}")
      // println(s"$a\n")
    }
    assertMultiline(a, e)
  }

  // TODO remove classnames in prod

  override def tests = Tests {

    "reqTableTitle" - {
      val style = Style.default

      "previewClosed" - {
        val a = renderEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Closed),
          previewWantOpen = false,
        )
        // editor : 1.1.1
        // preview: 1.2
        val e =
          """
            |<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div data-name="editorDiv">
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
        val a = renderEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.NeedOpen),
          previewWantOpen = true,
        )
        // editor : 1.1.1
        // preview: 1.2(.1)
        val e =
          """
            |<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div data-name="editorDiv">
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

    "reqTableCustomText" - {
      val style = Style(Position.Under, OpenPreview.MinimallyWithControls)

      "previewClosed" - {
        val a = renderEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.Closed),
          previewWantOpen = false,
        )
        // editor : 1.2.1.1
        // preview: 1.2.2
        val e =
          """
            |<div class="BaseStyles-previewToggleWrapper1">
            |  <div class="BaseStyles-previewToggleWrapper2 transition fade left out" style="visibility:hidden">
            |    <button title="Show preview" class="ui button icon  blue ">
            |      <i class="icon window restore" style="margin:0"></i>
            |    </button>
            |  </div>
            |  <div>
            |    <div data-name="editorDiv">
            |      <textarea class="BaseStyles-textEditor-12">
            |        {editor.value}
            |      </textarea>
            |      {instructions}
            |    </div>
            |    <div>
            |    </div>
            |  </div>
            |</div>
            |""".stripMargin
        assertRender(a, e)
      }

      "previewOpen" - {
        val a = renderEditor(
          status          = EditorStatus.Valid(None),
          style           = style,
          previewState    = Some(Status.NeedOpen),
          previewWantOpen = true,
        )
        // editor : 1.1.1.1
        // preview: 1.2.2
        val e =
          """
            |<div class="BaseStyles-textEditorTopPreviewUnder-2">
            |  <div>
            |    <div data-name="editorDiv">
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
    }

  }
}
