package shipreq.webapp.client.project.app.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.base.lib.DomUtil
import shipreq.webapp.client.project.feature.EditorFeature

/** This is effectively a hack so that `$.getDOMNode.map(_.asElement)` provides access to the cell, which can then be used to refocus the
  * cell when the editor closes.
  */
private[reqdetail] object EditableCell {

  trait Props {
    type A
    def cellBase: VdomTag
    def editor: EditorFeature.ReadWrite.ForEditor[A, Any]
    def editorArgs: A
    def view: () => TagMod

    @inline final def render: VdomElement = Component(this)
  }

  object Props {
    def apply(cellBase: VdomTag,
              editor  : EditorFeature.ReadWrite.ForEditor[Unit, Any],
              view    : () => TagMod): Props =
      apply(cellBase, editor, (), view)

    def apply[B](_cellBase  : VdomTag,
                 _editor    : EditorFeature.ReadWrite.ForEditor[B, Any],
                 _editorArgs: B,
                 _view      : () => TagMod): Props =
      new Props {
        override type A         = B
        override def cellBase   = _cellBase
        override def editorArgs = _editorArgs
        override def editor     = _editor
        override def view       = _view
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def render($: ScalaComponent.Lifecycle.RenderScope[Props, Unit, Unit], p: Props): VdomElement = {

    // This is the main point of this component
    val editorOnClose = DomUtil.focusParentOnChildClose($.mountedPure.getDOMNode.map(_.asElement))

    val editor = p.editor.onClose(editorOnClose)

    def onKeyDown: ReactKeyboardEventFromHtml => Callback =
      e => TableNavigationFeature.Keys(e) | EditorFeature.Keys(editor)(e)

    p.cellBase(
      ^.onKeyDown ==> onKeyDown,
      editor.themedRenderOr(p.editorArgs)(p.view()))
  }

  val Component = ScalaComponent.builder[Props]("EditableCell")
    .renderP(render)
    .build
}
