package shipreq.benchmark

import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {

    val body    = document getElementById "body"
    val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

    val async =
      for (bd <- BenchmarkData.load) yield
        BenchmarkGUI.renderMenu(body, baseUrl = baseUrl)(
          BinarySerialisation(bd).guiSuite,
        )

    async.toCallback.runNow()
  }
}