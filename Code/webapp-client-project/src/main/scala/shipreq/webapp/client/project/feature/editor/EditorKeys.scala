package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyCode
import shipreq.webapp.base.feature.clipboard.ClipboardKeys
import shipreq.webapp.base.lib.DomUtil._

object EditorKeys {
  import Feature.ReadWrite

  def apply[A](editor: ReadWrite.ForEditor[A, Any])(a: A)(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] = {

    val applicableToOpenAndReplace =
      !(e.altKey || e.ctrlKey || e.metaKey || editor.read.isOpen)

    def copy: CallbackOption[Unit] =
      ClipboardKeys.copy.withFallback(e, editor.clipboardData)

    def paste: CallbackOption[Unit] =
      ClipboardKeys.paste(e)(cd => editor.setPotentialValue(PotentialValue.Clipboard(cd)).getOrEmpty)

    def noModKeys: CallbackOption[Unit] =
      CallbackOption.keyCodeSwitch(e) {

        case KeyCode.F2 | KeyCode.Enter =>
          focusOrStartEditor(editor, e)

        case KeyCode.Backspace =>
          editor.setPotentialValue(PotentialValue.Emptiness).getOrEmpty.when_(applicableToOpenAndReplace)
      }

    def openEditorAndReplaceContentWithKey: CallbackOption[Unit] =
      for {
        _ <- CallbackOption.require(applicableToOpenAndReplace)
        _ <- CallbackOption.unless(e.key.matches("^(?:|[a-zA-Z0-9]{2,})$"))
        _ <- CallbackOption.liftOptionCallback(editor.setPotentialValue(PotentialValue.Text(e.key)))
      } yield ()

    def handlers: CallbackOption[Unit] =
      noModKeys | openEditorAndReplaceContentWithKey

    asEventDefaultWhenTargetsCell(e)(handlers) | copy | paste
  }

  private def focusOrStartEditor(editor: ReadWrite.ForAnyEditor, event: ReactEventFromHtml): CallbackOption[Unit] =
    if (editor.read.isOpen)
      focusChild(event)
    else
      editor.startEdit.getOrEmpty

  private def focusChild(event: ReactEventFromHtml): CallbackOption[Unit] =
    CallbackOption
      .liftOption(focusableChildren(event.currentTarget.domAsHtml).nextOption())
      .map(_.focus())

}
