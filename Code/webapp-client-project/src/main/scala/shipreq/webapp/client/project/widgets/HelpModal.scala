package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.ui.semantic.Modal
import shipreq.webapp.client.project.app.Style.{help => *}

object HelpModal {

  type TR = VdomTagOf[html.TableRow]

  type Groups = NonEmptyVector[Group]

  def Groups(g1: Group, gn: Group*): Groups =
    NonEmptyVector(g1, gn.toVector)

  // -------------------------------------------------------------------------------------------------------------------

  final class Group(title: VdomNode, rows: Vector[TR], enabled: Enabled) {

    val vdom: Vector[TR] = {
      val groupRow = <.tr(<.td(^.colSpan := 2, *.groupHeader(enabled), title))
      groupRow +: rows
    }

    def notApplicable(explanation: TagMod): Group = {
      val newTitle = <.span(title, <.span(*.groupNA, " (not applicable here)"))
      val content  = <.tr(<.td(^.colSpan := 2, *.cellNA, explanation))
      new Group(newTitle, Vector(content), Disabled)
    }
  }

  object Group {
    def apply(title: VdomNode)(r1: Row, rn: Row*): Group =
      new Group(title, (r1 +: rn.toVector).map(_.row), Enabled)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final class Row(val row: TR)

  object Row {
    private val rowText = <.td(*.rowText)
    private val rowExamples = <.td(*.rowExamples)

    def apply(text: TagMod*)(sample1: VdomNode, sampleN: VdomNode*): Row =
      new Row(
        <.tr(
          rowText(text: _*),
          rowExamples((sample1 +: sampleN).iterator.map(s => s: TagMod).intersperse(<.br).toTagMod)
        )
      )
  }

  // ===================================================================================================================

  def apply(modalHeader: VdomNode, groups: Groups): Modal = {
    val tbody = <.tbody(groups.iterator.flatMap(_.vdom).toSeq: _*)
    val table = <.table(*.table, tbody)
    Modal(modalHeader, table)
  }

  val code = <.code(*.code)
}

