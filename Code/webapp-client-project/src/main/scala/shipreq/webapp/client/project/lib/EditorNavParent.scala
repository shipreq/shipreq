package shipreq.webapp.client.project.lib
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.base.lib.DomUtil
import shipreq.webapp.client.project.feature.EditorFeature

/** This is a wrapper for a (potentially-closed) editor that ensures that when the editor is closes, focus returns to
  * the parent vdom.
  *
  * It integrates the [[EditorFeature]] and the [[TableNavigationFeature]]
  *
  * This is effectively a hack so that `$.getDOMNode.map(_.asElement)` provides access to the parent vdom, which is
  * needed to to refocus it when the editor closes.
  */
object EditorNavParent {

  trait Props {
    type A

    val parent    : VdomTag
    val editor    : EditorFeature.ReadWrite.ForEditor[A, Any]
    val editorArgs: A
    val view      : () => TagMod
    val tableStyle: TableNavigationFeature.TableStyle
    val onKeyDown : ReactKeyboardEventFromHtml => CallbackOption[Unit]

    @inline final def render: VdomElement =
      Component(this)

    @inline final def renderWithKey(k: Key): VdomElement =
      Component.withKey(k)(this)

    final def withEditor(e: EditorFeature.ReadWrite.ForEditor[A, Any]): Props =
      Props(parent, e, editorArgs, view(), onKeyDown)(tableStyle)

    final def modEditor(f: EditorFeature.ReadWrite.ForEditor[A, Any] => EditorFeature.ReadWrite.ForEditor[A, Any]): Props =
      withEditor(f(editor))
  }

  object Props {

    val doNothingOnKeyDown: ReactKeyboardEventFromHtml => CallbackOption[Unit] =
      Function const CallbackOption.fail

    /**
      * @param parent Container vdom without any [[TableNavigationFeature]] integration.
      */
    @inline def apply(parent: VdomTag,
                      editor: EditorFeature.ReadWrite.ForEditor[Unit, Any],
                      view  : => TagMod)
                     (implicit ts: TableNavigationFeature.TableStyle): Props =
      apply(parent, editor, (), view)

    /**
      * @param parent Container vdom without any [[TableNavigationFeature]] integration.
      */
    def apply[A](parent    : VdomTag,
                 editor    : EditorFeature.ReadWrite.ForEditor[A, Any],
                 editorArgs: A,
                 view      : => TagMod)
                (implicit ts: TableNavigationFeature.TableStyle): Props =
      apply(parent, editor, editorArgs, view, doNothingOnKeyDown)(ts)

    /**
      * @param parent Container vdom without any [[TableNavigationFeature]] integration.
      */
    def apply[A](parent    : VdomTag,
                 editor    : EditorFeature.ReadWrite.ForEditor[A, Any],
                 editorArgs: A,
                 view      : => TagMod,
                 onKeyDown : ReactKeyboardEventFromHtml => CallbackOption[Unit]
                )
                (implicit ts: TableNavigationFeature.TableStyle): Props = {
      type _A         = A
      val _parent     = parent
      val _editor     = editor
      val _editorArgs = editorArgs
      val _view       = () => view
      val _onKeyDown  = onKeyDown
      new Props {
        override type A         = _A
        override val parent     = _parent
        override val editorArgs = _editorArgs
        override val editor     = _editor
        override val view       = _view
        override val tableStyle = ts
        override val onKeyDown  = _onKeyDown
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Backend($: BackendScope[Props, Unit]) {

    // This is the main point of this component
    private val editorOnClose: Callback =
      $.getDOMNode.map(_.toHtml).asCBO.flatMapCB(DomUtil.focusParentOnChildClose)

    private def prepare[A](editor: EditorFeature.ReadWrite.ForEditor[A, Any]) =
      editor.onClose(editorOnClose)

    def startEdit[A](editor: EditorFeature.ReadWrite.ForEditor[A, Any]): Option[Callback] =
      prepare(editor).startEdit

    def render(p: Props): VdomElement = {

      val editor =
        prepare(p.editor)

      val onKeyDown: ReactKeyboardEventFromHtml => Callback =
        e => TableNavigationFeature(p.tableStyle).Keys(e) | EditorFeature.Keys(editor)(p.editorArgs)(e) | p.onKeyDown(e)

      p.parent(
        ^.onKeyDown ==> onKeyDown,
        editor.themedRenderOr(p.editorArgs)(p.view()))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build

  type ComponentRef = Ref.ToScalaComponent[Props, Unit, Backend]
}
