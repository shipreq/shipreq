package shipreq.benchmark

import japgolly.scalajs.react._, vdom.prefix_<^._
import org.scalajs.dom.{console, document, setTimeout}
import org.scalajs.dom.raw.HTMLPreElement
import scala.scalajs.js.JSApp
import shipreq.benchmark.lib.{Benchmark, BenchmarkSuite}

object Main extends JSApp {

  case class State(logs: Option[String]) {
    def add(msg: String): State = {
      val v = logs.filter(_.nonEmpty).fold("")(_ + "\n")
      State(Some(v + msg))
    }
  }

  case class BenchProps(suite: BenchmarkSuite, onStart: () => Unit) {
    def render: ReactElement = BenchComp(this)
  }

  val BenchComp = ReactComponentB[BenchProps]("B")
    .render((p, _) =>
    <.button(
      ^.margin := "2em",
      ^.fontSize := "20px",
      ^.onClick --> p.onStart(),
      s"${p.suite.suiteName}: Start")
    )
    .build

  val ConsoleComp = ReactComponentB[String]("C")
    .stateless
    .backend(new ConsoleBackend(_))
    .render(_.backend.render)
    .build

  class ConsoleBackend($: BackendScope[String, Unit])  {
    val consoleRef = Ref[HTMLPreElement]("refKey")

    def refreshConsole(): Unit =
      for (r <- consoleRef($))
        r.getDOMNode().style.display = "block"

    def render: ReactElement = {
      setTimeout(refreshConsole _, 10)
      <.pre(
        ^.width := "100%",
        ^.height := "100%",
        ^.display.none,
        ^.ref := consoleRef,
        $.props)
    }
  }

  val MainComp = ReactComponentB[Vector[BenchmarkSuite]]("M")
    .initialState(State(None))
    .backend(new MainBackend(_))
    .render(_.backend.render)
    .build

  class MainBackend($: BackendScope[Vector[BenchmarkSuite], State])  {

    def log: Benchmark.Logger =
      msg => $.modState(_ add msg)

    def start(suite: BenchmarkSuite): Unit = {
      //$.setState(State(Some("")), () => suite.run(log))
      $.setState(State(Some("")))
      setTimeout(() => suite.run(log), 100)
    }

    def render: ReactElement = {
      $.state.logs match {
        case Some(l) =>
          ConsoleComp(l)
        case None =>
          val ss = $.props.map { suite =>
            BenchProps(suite, () => start(suite)).render
          }
          <.div(ss)
      }
    }
  }

  def main(): Unit = {
    val bms = Vector(
      Serialisation,
      Deserialisation,
      Hashing)
    React.render(MainComp(bms), document.body)
  }
}