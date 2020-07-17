package shipreq.webapp.base.feature.clipboard

import org.scalajs.dom.window.navigator
import scala.scalajs.js

/** Facades for the clipboard-polyfill JS library. */
private[clipboard] object ClipboardJs {

  val instance: js.UndefOr[ClipboardJs] =
    navigator.asInstanceOf[js.Dynamic].clipboard.asInstanceOf[js.UndefOr[ClipboardJs]]

  @js.native
  @nowarn("cat=unused")
  sealed trait DataTransfer extends js.Object {
    def setData(`type`: String, value: String): Unit               = js.native
    def getData(`type`: String)               : js.UndefOr[String] = js.native
  }
}

@js.native
@nowarn("cat=unused")
private[clipboard] sealed trait ClipboardJs extends js.Object {
  import ClipboardJs.DataTransfer

  def read            ()                  : js.Promise[DataTransfer] = js.native
  def readText        ()                  : js.Promise[String]       = js.native
  def write           (data: DataTransfer): js.Promise[Unit]         = js.native
  def writeText       (s: String)         : js.Promise[Unit]         = js.native
  def suppressWarnings()                  : Unit                     = js.native
}
