package shipreq.benchmark

import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom.document
import scala.scalajs.js.JSApp

object Main extends JSApp {

  def main(): Unit = {
    val body = document getElementById "body"

    BenchmarkGUI.renderMenu(body, baseUrl = BASEURL)(
      Hashing.suite,
      Serialisation.suite)
  }
}