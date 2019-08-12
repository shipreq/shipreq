package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.stdlib_ext.StdlibExt._
import org.scalajs.dom.html
import shipreq.webapp.base.feature.tablenav.{VirtualLoc, VirtualTable}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

object IssuesPageObs {
  val SummaryRegex = ".*Showing (\\d+) issue.*".r

  final class Cell($: DomZipperJs) {
    def td()         = $.domAs[html.TableCell]
    val rowSpan      = td().rowSpan
    val text         = td().innerText.replace('\n', ' ').trim
    val editorO      = $.collect01("textarea").domsAs[html.TextArea]
    def editor()     = editorO.get
    val editorValue  = editorO.fold("")(_.value)
    val isEditorOpen = editorO.nonEmpty
  }
}

final class IssuesPageObs($: DomZipperJs) {
  import IssuesPageObs.Cell

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
  class RowObs(r: Int) {
    private val row = cells(r)

    def apply(idx: Int): Cell =
      row(if (idx < 0) colCount + idx else idx)
        .getOrElse(sys.error(s"No root cell @ ($r,$idx)"))

    def apply(col: Column): Cell =
      this(col match {
        case Column.IssueCategory => 0
        case Column.IssueClass    => 1
        case Column.Id            => 2
        case Column.Title         => 3
        case Column.FieldName     => 4
        case Column.FieldEditor   => 5
        case Column.Actions       => 6
      })
  }

  def columnTexts(c: Int): List[Option[String]] =
    List.tabulate(rowCount)(r => cells(r)(c).map(_.text))

  // ===================================================================================================================

  val newForm = new NewForm
  final class NewForm {
    private val root = $(Style.issues.newIssueCont.selector)

    val button = root("button").prepare(_.domAs[html.Button])
  }

  // ===================================================================================================================

  val summary = new Summary
  final class Summary {
    private val root = $(Style.issues.pageRow1.selector).child("div", 2 of 2)

    val text = root.innerText

    val totalIssues: Option[Int] =
      text match {
        case IssuesPageObs.SummaryRegex(d) => Some(d.toInt)
        case _                             => None
      }
  }
}
