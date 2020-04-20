package shipreq.benchmark

import japgolly.scalajs.benchmark.engine.Options
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import org.scalajs.dom.document
import scala.concurrent.duration._
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {

    val body    = document getElementById "body"
    val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

    val options: Options =
      Options.Default.copy(
        maxTime = 60.seconds,
      )

    val async =
      for (bd <- BenchmarkData.load) yield
        BenchmarkGUI.renderMenu(body, baseUrl = baseUrl, options = options)(
          ApplyEventBM         (bd).guiSuite,
          BinarySerialisationBM(bd).guiSuite,
        )

    async.toCallback.runNow()
  }
}