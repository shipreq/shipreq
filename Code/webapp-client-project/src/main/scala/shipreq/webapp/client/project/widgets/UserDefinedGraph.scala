package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.parboiled2._
import shipreq.base.util.{ErrorMsg, Util}
import shipreq.webapp.client.project.app.WebWorkerClient
import shipreq.webapp.client.project.widgets.GraphComponent._
import shipreq.webapp.client.ww.api.WebWorkerCmd
import shipreq.webapp.member.project.data.Svg

object UserDefinedGraph {

  final case class Props(dot: String, webWorker: WebWorkerClient.Instance) extends HasWebWorker {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val WS = CharPredicate.from(_.isWhitespace)
  private val ID = CharPredicate.Visible -- '{'

  private class GraphParser(val input: ParserInput) extends Parser {
    def main: Rule1[(String, String)] =
      rule(
        WS.*
          ~ capture(
          (ignoreCase("strict") ~ WS.+).?
            ~ ignoreCase("di").?
            ~ ignoreCase("graph")
            ~ (WS.+ ~ ID.+).?
        )
          ~ WS.*
          ~ '{'
          ~ WS.*
          ~ capture((!EOI ~ ANY).+)
          ~ EOI
          ~> ((a: String, b: String) => (a, b))
      )
  }

  private[widgets] val defaults = """bgcolor=transparent;rankdir=LR;node[style=filled fillcolor="#e8e8e8"]"""

  private val comment = "(?:#|//).*$".r

  private[widgets] def correct(dot: String): String =
    new GraphParser(dot).main.run().toOption match {
      case Some((head, body)) =>
        s"$head{$defaults;$body"

      case None =>
        val content = dot.linesIterator.map(comment.replaceFirstIn(_, "")).mkString
        val d = Util.countOccurrences(content, "->")
        val u = Util.countOccurrences(content, "--")
        val graph = if (u > d) "graph" else "digraph"
        s"$graph{$defaults;$dot\n}"
    }

  final class Backend($: BackendScope[Props, State]) extends GraphBackend($) {
    override protected def displayMode(p: Props) =
      DisplayMode.FitToWidth

    override def cmd(p: Props): WebWorkerCmd[ErrorMsg \/ Svg] = {
      val dot = correct(p.dot)
      WebWorkerCmd.GraphInline(dot)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(graphConfig)
    .build
}