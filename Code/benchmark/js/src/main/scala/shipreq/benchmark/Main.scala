package shipreq.benchmark

import japgolly.scalajs.react._, vdom.prefix_<^._
import org.scalajs.dom.{console, document, setTimeout}
import org.scalajs.dom.raw.HTMLPreElement
import scala.scalajs.js.JSApp
import shipreq.benchmark.lib.{CompositeSuite, Benchmark, BenchmarkSuite}

object Main extends JSApp {

  case class State(logs: Option[String], results: Vector[String]) {
    def add(msg: String): State = {
      val v = logs.filter(_.nonEmpty).fold("")(_ + "\n")
      copy(logs = Some(v + msg))
    }
    def addResult(result: String): State =
      copy(results = (results :+ result).sorted)
  }

  case class BenchProps(suite: BenchmarkSuite, onStart: Callback) {
    def render: ReactElement = BenchComp(this)
  }

  val BenchComp = ReactComponentB[BenchProps]("B")
    .render_P(p =>
      <.button(
        ^.margin := "2em",
        ^.fontSize := "20px",
        ^.onClick --> p.onStart,
        s"${p.suite.suiteName}: Start"))
    .build

  val ConsoleComp = ReactComponentB[String]("C")
    .renderBackend[ConsoleBackend]
    .build

  class ConsoleBackend($: BackendScope[String, Unit]) {
    val consoleRef = Ref[HTMLPreElement]("refKey")

    def refreshConsole(): Unit =
      for (r <- consoleRef($))
        r.getDOMNode().style.display = "block"

    def render(props: String): ReactElement = {
      setTimeout(refreshConsole _, 10)
      <.pre(
        ^.width := "100%",
        ^.height := "100%",
        ^.display.none,
        ^.ref := consoleRef,
        props)
    }
  }

  val MainComp = ReactComponentB[Vector[BenchmarkSuite]]("M")
    .initialState(State(None, Vector.empty))
    .renderBackend[MainBackend]
    .build

  class MainBackend($: BackendScope[Vector[BenchmarkSuite], State]) {

    def log: Benchmark.Logger =
      msg => $.modState(_ add msg).runNow()

    def logResult: Benchmark.Logger =
      msg => $.modState(_ addResult msg).runNow()

    def start(suite: BenchmarkSuite): Callback =
      $.setState(
        State(Some(""), Vector.empty),
        Callback(suite.run(log, logResult)))

    def render(p: Vector[BenchmarkSuite], s: State): ReactElement =
      s.logs match {
        case Some(l) =>
          // Benchmark started
          ConsoleComp(s.results.mkString("\n") + "\n\n\n" + l)
        case None =>
          // Main screen
          val ss = p.map { suite =>
            BenchProps(suite, start(suite)).render
          }
          <.div(ss)
      }
  }

  def main(): Unit = {
    val bms = Vector(
      BinSerialisation,
      BinDeserialisation,
      Hashing)

    val all = CompositeSuite("All")(bms: _*)

    val main = MainComp(all +: bms)

    React.render(main, document.body)
  }
}