package shipreq.webapp.base.feature.clipboard

import japgolly.scalajs.react.AsyncCallback

object TestClipboard {

  private final class Mock extends Clipboard {
    var text = ""
    override val read = AsyncCallback.point(ClipboardData(text))
    override def write(d: ClipboardData) = AsyncCallback.point {
      // println("Setting clipboard text to " + d)
      text = d.text
    }
  }

  private val mock = new Mock

  Clipboard.setClipboardImpl(mock)

  def clear(): Unit =
    writeText("")

  def readText(): String =
    mock.text

  def writeText(t: String): Unit =
    mock.text = t
}
