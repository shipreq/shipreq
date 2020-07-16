package shipreq.benchmark

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.extra.router.BaseUrl
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {

    val body    = document getElementById "body"
    val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

    <.div("Initialising...").renderIntoDOM(body)

    val engineOptions: EngineOptions =
      EngineOptions.default

    val guiOptions: GuiOptions =
      GuiOptions.default

    val async =
      for (bd <- BenchmarkData.load) yield
        BenchmarkGUI.renderMenu(body, baseUrl = baseUrl, engineOptions = engineOptions, guiOptions = guiOptions)(
          ApplyEventBM         (bd).guiSuite,
          BinarySerialisationBM(bd).guiSuite,
          ImpFieldCalcBM       (bd).guiSuite,
        )

    async.toCallback.runNow()
  }
}