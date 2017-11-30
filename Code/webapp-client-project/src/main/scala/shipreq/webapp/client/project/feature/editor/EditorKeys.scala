package shipreq.webapp.client.project.feature.editor

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyCode
import Feature.ReadWrite

object EditorKeys {

  def apply(editor: ReadWrite.ForAnyEditor)(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] = {
    import shipreq.webapp.base.lib.DomUtil._

    def focusChild: CallbackOption[Unit] =
      CallbackOption
        .liftOption(focusableChildren(e.currentTarget.domAsHtml).nextOption())
        .map(_.focus())

    def focusOrStartEditor: CallbackOption[Unit] =
      if (editor.read.editor.isDefined) focusChild else editor.startEdit.getOrEmpty

    def cellEvents: CallbackOption[Unit] =
      CallbackOption.asEventDefault(e,
        CallbackOption.require(doesEventTargetCell(e)) >>
          CallbackOption.keyCodeSwitch(e) {
            case KeyCode.F2 => focusOrStartEditor
          })

    cellEvents
  }

}
