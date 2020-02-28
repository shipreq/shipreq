package shipreq.webapp.client.project.app.pages.content.reqtable

import org.parboiled2.ParseError
import org.parboiled2.Parser.DeliveryScheme.Throw
import org.scalajs.dom.{document, html}
import shipreq.base.util.{Invalid, Validity}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.clipboard.TestClipboard
import shipreq.webapp.base.lib.DomUtil._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.test._
import TestState._

object ReqTableObs {
  case class CellLoc(row: Int, col: Int)

  lazy val selNA = Style.reqtable.table.`N/A`.selector

  lazy val hasNA: org.scalajs.dom.Node => Boolean =
    Sizzle(selNA, _).nonEmpty

  lazy val rowFilter: Live => html.TableRow => Boolean = {
    Live.memo[html.TableRow => Boolean] { l =>
      val List(on, off) = List(On, Off).map(o => Style.reqtable.table.dataCell((l, o)).className.value)
      _.children.iterator
        .filterNot(hasNA)
        .exists(td => td.classList.contains(on) || td.classList.contains(off))
    }
  }

  val parseContentSummary = "^Showing(\\d+)rows?:(\\d+)(\\(.+\\))?(\\+.+)?$".r

  def calc(s: String): Int = {
    val s2 = if (s.head == '+') s.tail else s
    try
      new Calculator(s2).InputLine.run()
    catch {
      case p: ParseError => fail(s"Failed to parse [$s2] -- ${p.getMessage}")
    }
  }

  case class ContentSummary(rows: Int, reqs: Int, reqBreakdown: Option[String], other: Option[String]) {
    val reqBreakdownResult: Option[Int] = reqBreakdown.map(calc)
    val rowBreakdownResult: Int         = reqs + other.fold(0)(calc)
  }
}

/**
 * Data representation of a rendered ReqTable.
 *
 * Inspects actual DOM to derive values.
 */
final class ReqTableObs(global: TestGlobal, $: DomZipperJs) {
  import ReqTableObs._

  val activeElement = document.activeElement

  val clipboardText = TestClipboard.readText()

  val svrReqs = global.reqs()

  def findOne[A: UnivEq, B](a: A, bs: Iterable[B])(f: B => A): B =
    bs.iterator.filter(f(_) ==* a).toList match {
      case b :: Nil => b
      case x => sys error s"Expected to find one result for '$a' but found: $x. Available are: ${bs.iterator.map(f).mkString(", ")}."
    }

  object columnSelector {
    val root = $(".ui.popup:has(.ui.checkbox)")

    case class ColumnDom(outer: DomZipperJs) {
      val checkbox: html.Input = outer("input").domAs[html.Input]
      val on      : On         = On when checkbox.checked
      val name    : String     = outer("label").innerText
    }

    val entirety: Vector[ColumnDom] =
      root.collect1n("div.ui.checkbox").map(ColumnDom)

    def column(name: String): ColumnDom =
      findOne(name, entirety)(_.name)

    val allColumns: Vector[String] =
      entirety.map(_.name)

    val onColumns: Vector[String] =
      entirety.filter(_.on is On).map(_.name)
  }
  columnSelector // force

  val filterInput: html.Input =
    $("input[placeholder='Filter...']").domAs[html.Input]

  val filterValue: String =
    filterInput.value

  val filterValueValidity: Validity =
    Invalid when filterInput.parentNode.asInstanceOf[html.Element].classList.contains("error")

  val filterDeadButton: html.Button =
    $(s"${Style.reqtable.page.filterDeadButtonContainer.selector} button").domAs[html.Button]

  val filterDead: FilterDead =
    ShowDead when filterDeadButton.classList.contains("red")

  object sorting {
//    private val all = (SortMethod.ignoreBlanks ++ SortMethod.considerBlanks).whole
//    private val readSortMethod: String => Option[SortMethod] = {
//      case "Unused" => None
//      case s => all.find(_.optionLabel == s).fold(sys error s"Unknown sort method: $s")(Some(_))
//    }
//
//    private val readSortMethodIB: String => SortMethod.IgnoreBlanks =
//      s => SortMethod.ignoreBlanks.whole.find(_.optionLabel == s).getOrElse(sys error s"Unknown sort method: $s")

    val $: DomZipperJs =
      ReqTableObs.this.$("Sort row", Style.reqtable.sortEditor.dragArea.selector)

    case class CriteriaDom(nameDom: html.Element, orderDom: html.Element) {
      val name = nameDom.textContent
    }

    val criteriaDom = $.collect1n("tr").map(tr => CriteriaDom(
      tr("td", 1 of 2).domAsHtml,
      tr("td", 2 of 2)("*[title]").domAsHtml))

    val names: Vector[String] =
      criteriaDom.map(_.name)

//      val inconclusive: Vector[(String, SortMethod)] =
//        $("ol").collect("li", li =>
//          (li("select").selectedOptionText.get |> readSortMethod, li("select + span").innerHTML))

//      val conclusiveOrder: SortMethod.IgnoreBlanks =
//        $("ol+div select", 1 of 2).selectedOptionText.get |> readSortMethodIB
//
//      val conclusiveColumnSelected: String =
//        $("ol+div select", 2 of 2).selectedOptionText.get
//
//      val conclusiveColumns: Vector[String] =
//        $("ol+div select", 2 of 2) collectInnerHTML "option"
//
//      val visibleColumns: Vector[String] =
//        inconclusive.map(_._2) ++ conclusiveColumns
  }

  object table {
    val $ = ReqTableObs.this.$("ReqTable", Style.reqtable.table.table.selector)
    val thead = $("ReqTable", ">thead")
    val tbody = $("ReqTable", ">tbody")

    case class ColumnDom(zipper: DomZipperJs) {
      val headerCell = zipper.dom
      val name = headerCell.textContent
    }

    val columnDoms: Vector[ColumnDom] =
      thead.collect1n("th").map(ColumnDom)

    val columns: Vector[String] =
      columnDoms.map(_.name)

    val fieldColumns: Vector[String] =
      columns.drop(1)

    def column(name: String): ColumnDom =
      findOne(name, columnDoms)(_.name)

    import Table.Shared.CellState

//    private def cell(s: CellState): String =
//      s"td.${Style.reqtable.table.dataCell(s).className.value}"

//    private def cell(s: Status, focus: Boolean): String =  {
//      var r = "td." + Style.reqtable.cell(s).className.value
//      if (focus)
//        r += ":focus"
//      else
//        r += ":not(:focus)"
//      r
//    }

    val allRows : Vector[html.TableRow] =
      tbody.collect0n(">tr").domsAs[html.TableRow]

    // Existence of a Live cell means the row is Live
    // Existence of a Dead cell does NOT mean the row is Dead
    val (liveRows, deadRows) = allRows.partition(rowFilter(Live))

    /*
    // This isn't an accurate way to do this because live rows *can* have dead cells when a column is dead
    val deadRows: Vector[html.TableRow] = allRows.filter(rowFilter(Dead))
    val liveRows: Vector[html.TableRow] = allRows.filter(rowFilter(Live))

    for (r <- liveRows.filter(r => deadRows.exists(_ eq r))) {
      println(">" * 200)
      r.children.iterator.filterNot(hasNA).foreach(c => println(c.outerHTML))
      println("<" * 200)
    }
    */

//    val focusRow = tbody downO byFocus(true, row)
//    val focus    = tbody downO byFocus(true, identity)
//
//    val inputsInFocusRow: Option[Int] =
//      focusRow.map(_.getAll("input,select,textarea").length)
//
//    def ensureHasFocus(): Unit =
//      focus getOrElse fail("No focus.")

    private def findIndex(subj: String, in: Vector[String], err: => String): Int = {
      val i = in.indexOf(subj)
      if (i < 0) fail(s"$err\n$in")
      i
    }

    def columnIndex(title: String): Int =
      findIndex(title, columns, s"Column '$title' not found.")

    val pubidColumnIndex =
      columnIndex("ID")

    val rowPubids: Vector[String] =
      tbody collect0n s">tr >td:nth-child(${pubidColumnIndex + 1})" innerTexts

    def rowIndexByPubid(pubid: String): Int =
      findIndex(pubid, rowPubids, s"Row with pubid [$pubid] not found.")

    def cell(loc: CellLoc): DomZipperJs =
      cell(row = loc.row, col = loc.col)

    def cell(row: Int, col: Int): DomZipperJs = {
      var c = col
      if (c < 0) c += columns.length
      var r = row
      if (r < 0) r += allRows.size
      tbody(s">tr:nth-child(${r + 1}) >td:nth-child(${c + 1})")
    }

    def cell(pubid: String, col: String): DomZipperJs =
      cell(cellLoc(pubid, col))

    def cellLoc(pubid: String, col: String): CellLoc =
      CellLoc(row = rowIndexByPubid(pubid), columnIndex(col))

    def rowSelectionInput(row: Int): html.Input =
      cell(row, 0)("input[type=checkbox]").domAs[html.Input]

    lazy val allRowSelectionInput: html.Input =
      columnDoms(0).zipper("input[type=checkbox]").domAs[html.Input]

    lazy val entireContent =
      tbody.collect0n(">tr").zippers.iterator
        .map(_.collect1n(">td").innerTexts.mkString("│ ", "\t│ ", " │"))
        .mkString("\n")
  }

  // ===================================================================================================================

  object stats {
    val text = $("Stats", Style.reqtable.page.summary.selector).innerText

    val lines: Vector[String] =
      text.split("[\r\n]+").iterator
        .map(_.filterNot(c => c.isWhitespace || c == '_'))
        .filter(_.nonEmpty)
        .toVector

    val contentLine: String =
      lines(0)

    val content: ContentSummary =
      contentLine match {
        case parseContentSummary(rows, reqs, reqBreakdown, other) =>
          ContentSummary(
            rows.toInt,
            reqs.toInt,
            Option(reqBreakdown).filter(_.nonEmpty),
            Option(other).filter(_.nonEmpty))
        case u => fail(s"Unable to parse content summary [$u]")
      }
  }
}