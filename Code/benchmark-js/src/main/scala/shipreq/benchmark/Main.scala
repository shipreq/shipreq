package shipreq.benchmark

import org.scalajs.dom.setTimeout
import scala.scalajs.js.JSApp


object Main extends JSApp {

  def run() = Deserialisiation.run()

  def main(): Unit = {
    val delay = 5
    println(s"Starting in $delay seconds...")
    setTimeout(() => run(), delay * 1000)
  }
}