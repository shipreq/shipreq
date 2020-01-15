package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyCode
import shipreq.webapp.base.feature.clipboard.Clipboard
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.util.Browser

object EditorKeys {
  import Feature.ReadWrite

  def apply[A](editor: ReadWrite.ForEditor[A, Any])(a: A)(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] = {

    val applicableToOpenAndReplace =
      !(e.altKey || e.ctrlKey || e.metaKey || editor.read.isOpen)

    def paste: Callback =
      editor.setPotentialValueAsync(Clipboard.read.map(PotentialValue.Clipboard)).getOrEmpty

    def noModKeys: CallbackOption[Unit] =
      CallbackOption.keyCodeSwitch(e) {

        case KeyCode.F2 =>
          focusOrStartEditor(editor, e)

        case KeyCode.Backspace =>
          editor.setPotentialValue(PotentialValue.Emptiness).getOrEmpty.when_(applicableToOpenAndReplace)
      }

    def shiftKeys: CallbackOption[Unit] =
      CallbackOption.keyCodeSwitch(e, shiftKey = true) {

        case KeyCode.Insert =>
          paste
      }

    def platformDependantKeys: CallbackOption[Unit] =
      Browser.cmdOrCtrlKeyCodeSwitch(e) {

        case KeyCode.V =>
          paste
      }

    def openEditorAndReplaceContentWithKey: CallbackOption[Unit] =
      for {
        _ <- CallbackOption.require(applicableToOpenAndReplace)
        _ <- CallbackOption.unless(e.key.matches("^(?:|[a-zA-Z0-9]{2,})$"))
        _ <- CallbackOption.liftOptionCallback(editor.setPotentialValue(PotentialValue.Text(e.key)))
      } yield ()

    def handlers: CallbackOption[Unit] =
      noModKeys | shiftKeys | platformDependantKeys | openEditorAndReplaceContentWithKey

    (CallbackOption.require(doesEventTargetCell(e)) >> handlers).asEventDefault(e)
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
