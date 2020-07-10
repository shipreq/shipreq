package shipreq.webapp.base.feature.clipboard

import japgolly.scalajs.react._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.window
import scala.util.Try
import shipreq.webapp.base.lib.DomUtil.asEventDefaultWhenTargetsCell
import shipreq.webapp.base.util.{Browser, TextMod}

object ClipboardKeys {

  object copy {

    private def readGeneric(e: ReactKeyboardEventFromHtml): CallbackTo[ClipboardData] =
      CallbackTo {
        val innerText = Try(e.target.innerText).getOrElse("")
        val data      = TextMod.multiLineWhitespace(innerText)
        ClipboardData(data)
      }

    def apply(e: ReactKeyboardEventFromHtml, getData: CallbackTo[ClipboardData]): CallbackOption[Unit] = {
      def copy: Callback =
        for {
          data <- getData
          _    <- Clipboard.instance.write(data).toCallback
        } yield ()

      /** If the user has selected a portion on the text in the cell, or even the text of a bunch of cells,
        * that text should be copied to the clipboard (i.e. the default behaviour if we don't catch the event).
        */
      def nothingIsSelected(): Boolean =
        window.getSelection().isCollapsed

      asEventDefaultWhenTargetsCell(e)(
        Browser.cmdOrCtrlKeyCodeSwitch(e) {
          case KeyCode.C if nothingIsSelected() => copy
        } |
        CallbackOption.keyCodeSwitch(e, ctrlKey = true) {
          case KeyCode.Insert if nothingIsSelected() => copy
        }
      )
    }

    def generic(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
      apply(e, readGeneric(e))

    def withFallback(e: ReactKeyboardEventFromHtml, data: Option[ClipboardData]): CallbackOption[Unit] =
      data.fold(generic(e))(cd => apply(e, CallbackTo.pure(cd)))
  }

  // ===================================================================================================================

  def paste(e: ReactKeyboardEventFromHtml)(p: ClipboardData => Callback): CallbackOption[Unit] = {
    def paste: Callback =
      Clipboard.instance.read.flatMap(p(_).asAsyncCallback).toCallback

    asEventDefaultWhenTargetsCell(e)(
      Browser.cmdOrCtrlKeyCodeSwitch(e) {
        case KeyCode.V => paste
      } |
      CallbackOption.keyCodeSwitch(e, shiftKey = true) {
        case KeyCode.Insert => paste
      }
    )
  }

}
