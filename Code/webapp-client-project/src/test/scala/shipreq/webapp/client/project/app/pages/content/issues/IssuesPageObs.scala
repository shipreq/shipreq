package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.scalajs.dom.html
import shipreq.webapp.base.feature.tablenav.{VirtualLoc, VirtualTable}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

object IssuesPageObs {
  val SummaryRegex = ".*Showing (\\d+) issue.*".r

  val columnIndex: Column => Int = {
    case Column.IssueCategory => 0
    case Column.IssueClass    => 1
    case Column.Id            => 2
    case Column.Title         => 3
    case Column.FieldName     => 4
    case Column.FieldEditor   => 5
    case Column.Actions       => 6
  }

  final class Cell($: DomZipperJs) {
    private val td = $.domAs[html.TableCell]
    val rowSpan    = td.rowSpan
    val text       = td.innerText.replace('\n', ' ').trim
    val editor     = new OptionalEditorObs($)
    val actions    = $.collect0n("button").map(new Action(_))
  }

  final class Action($: DomZipperJs) {
    val label = $.innerText
    val dom   = $.prepare(_.domAs[html.Button])
  }
}

final class IssuesPageObs($: DomZipperJs) {
  import IssuesPageObs._

  val editables = $.editables0n.doms

  private val tbody = $.child("table").child("tbody").domAsHtml

  private val vtable = VirtualTable.from($.child("table").domAs[html.Table])

  val rowCount = tbody.childElementCount

  val colCount = if (rowCount == 0) 0 else tbody.firstChild.childNodes.length

  private val cells: Vector[Vector[Option[Cell]]] =
    Vector.tabulate(rowCount) { r =>
      Vector.tabulate(colCount) { c =>
        val loc = VirtualLoc(1, r, c, None)
        Option.when(vtable.isRootCell(loc))(new Cell(DomZipperJs(vtable.cellAt(loc).getOrThrow())))
      }
    }

  def apply(r: Int): RowObs = new RowObs(r)
  final class RowObs(r: Int) {
    private val row = cells(r)

    def apply(idx: Int): Cell =
      row(if (idx < 0) colCount + idx else idx)
        .getOrElse(sys.error(s"No root cell @ ($r,$idx)"))

    def apply(col: Column): Cell =
      this(columnIndex(col))
  }

  def columnTexts(c: Column): List[Option[String]] =
    List.tabulate(rowCount)(r => cells(r)(columnIndex(c)).map(_.text))

  // ===================================================================================================================

  val filter = new Filter
  final class Filter {
    private val root = $(Style.issues.pageRow2.selector).child("div", 2 of 2)

    private val f = root("input:text")
    val dom   = f.prepare(_.domAs[html.Input])
    val value = f.value
  }

  // ===================================================================================================================

  val newForm = new NewForm
  final class NewForm {
    private val root = $(Style.issues.newIssueCont.selector)

    val button = root("button").prepare(_.domAs[html.Button])

    val editor = new OptionalEditorObs(root)
  }

  // ===================================================================================================================

  val summary = new Summary
  final class Summary {
    private val root = $(Style.issues.pageRow1.selector).child("div", 2 of 2)

    val text = root.innerText

    val totalIssues: Option[Int] =
      text match {
        case SummaryRegex(d) => Some(d.toInt)
        case _               => None
      }
  }
}
