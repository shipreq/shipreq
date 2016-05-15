package shipreq.webapp.client.project.app.reqtable

import org.parboiled2.Parser.DeliveryScheme.Throw
import org.scalajs.dom.{document, html}
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.data._
import shipreq.webapp.client.project.test._
import shipreq.webapp.client.project.widgets.Checkbox
import TestState._

object ReqTableObs {
  case class CellLoc(row: Int, col: Int)

  val reportedRowCount = "^(\\d+) row.*".r
  val reportedReqCount = ".*\\D(\\d+) reqs?.*".r
  val reportedReqFormula = ".*\\d reqs? +\\((.+?)\\).*".r.pattern

  val nonFormula = "[^0-9+-]+".r
}

/**
 * Data representation of a rendered ReqTable.
 *
 * Inspects actual DOM to derive values.
 */
final class ReqTableObs(cp: TestClientProtocol, $: HtmlDomZipper) {
  import ReqTableObs._

  val activeElement = document.activeElement

  val svrReqs = cp.reqs

  def findOne[A: UnivEq, B](a: A, bs: Iterable[B])(f: B => A): B =
    bs.iterator.filter(f(_) ==* a).toList match {
      case b :: Nil => b
      case x => sys error s"Expected to find one result for '$a' but found: $x. Available are: ${bs.iterator.map(f).mkString(", ")}."
    }

  object viewSettings {
    val $ = ReqTableObs.this.$("ViewSettings", ">table", 1 of 2)
    def vsCol(i: Int) = $("column #" + i, "tbody tr")(">td", i of 3)

    object columns {

      case class ColumnDom(outer: HtmlDomZipperAt[html.Label]) {
        val checkbox = outer("input").domAs[html.Input]
        val on       = On <~ checkbox.checked
        val name     = outer.innerText
      }

      val entirety: Vector[ColumnDom] =
        vsCol(1).collect1n("label").as[html.Label].mapZippers(ColumnDom)

      def column(name: String): ColumnDom =
        findOne(name, entirety)(_.name)

      val allColumns: Vector[String] =
        entirety.map(_.name)

      val onColumns: Vector[String] =
        entirety.filter(_.on :: On).map(_.name)
    }

    object filter {
      val $ = vsCol(3)
      val input = $("textarea").domAs[html.TextArea]
      val value = input.value
    }

    object filterDead {
      val checkbox = filter.$("input[type=checkbox]").domAs[html.Input]
      val value: FilterDead = Checkbox.filterDeadChecked <~ checkbox.checked
    }
  }

  object sorting {
//    private val all = (SortMethod.ignoreBlanks ++ SortMethod.considerBlanks).whole
//    private val readSortMethod: String => Option[SortMethod] = {
//      case "Unused" => None
//      case s => all.find(_.optionLabel == s).fold(sys error s"Unknown sort method: $s")(Some(_))
//    }
//
//    private val readSortMethodIB: String => SortMethod.IgnoreBlanks =
//      s => SortMethod.ignoreBlanks.whole.find(_.optionLabel == s).getOrElse(sys error s"Unknown sort method: $s")

    val $: HtmlDomZipper = ReqTableObs.this.$("Sort row", ">div:contains('Sort')")

    //    val criteriaDom = $.collect1("tr", tr => (
    //      tr("td", 2 of 2).innerText,
    //      tr("td", 1 of 2)("*[title]").domAs[html.Element].title))

    case class CriteriaDom(nameDom: html.Element, orderDom: html.Element) {
      val name = nameDom.textContent
    }

    val criteriaDom = $.collect1n("tr").mapZippers(tr => CriteriaDom(
      tr("td", 2 of 2).dom,
      tr("td", 1 of 2)("*[title]").dom))

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
    val $ = ReqTableObs.this.$("ReqTable", ">table", 2 of 2)
    val thead = $("ReqTable", ">thead")
    val tbody = $("ReqTable", ">tbody")

    case class ColumnDom(zipper: HtmlDomZipperAt[html.TableCell]) {
      val headerCell = zipper.dom
      val name = headerCell.textContent
    }

    val columnDoms: Vector[ColumnDom] =
      thead.collect1n("th").as[html.TableCell].mapZippers(ColumnDom)

    val columns: Vector[String] =
      columnDoms map (_.name)

    val fieldColumns: Vector[String] =
      columns.drop(1)

    def column(name: String): ColumnDom =
      findOne(name, columnDoms)(_.name)

    import Table.CellStatus

    private def cell(s: CellStatus): String =
      "td." + Style.reqtable.cell(s).className.value

//    private def cell(s: Status, focus: Boolean): String =  {
//      var r = "td." + Style.reqtable.cell(s).className.value
//      if (focus)
//        r += ":focus"
//      else
//        r += ":not(:focus)"
//      r
//    }

    private def row(inner: String): String =
      s">tr:has($inner)"

//    private def byFocus(focus: Boolean, wrap: String => String): String =
//      ColumnRenderer.statusDomain.toStream.map(s => wrap(cell(s, focus))).mkString(",")

    private def byStatus(s: CellStatus, wrap: String => String): String =
      wrap(cell(s))

    val allRows  = tbody collect0n ">tr" doms
    val deadRows = tbody collect0n byStatus(CellStatus.DeadRow, row) doms
    val liveRows = tbody collect0n byStatus(CellStatus.Normal, row) doms
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

    def cell(loc: CellLoc): HtmlDomZipperAt[html.TableCell] =
      cell(row = loc.row, col = loc.col)

    def cell(row: Int, col: Int): HtmlDomZipperAt[html.TableCell] = {
      var c = col
      if (c < 0) c += columns.length
      var r = row
      if (r < 0) r += allRows.size
      tbody(s">tr:nth-child(${r + 1}) >td:nth-child(${c + 1})").as[html.TableCell]
    }

    def cell(pubid: String, col: String): HtmlDomZipperAt[html.TableCell] =
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
    val text = $("Stats", ">div", 2 of 4).innerText

    val reportedRows: Int =
      text match {
        case reportedRowCount(n) => n.toInt
        case u => fail(s"Unable to extract row count from [$u].")
      }

    val reportedReqs: Int =
      text match {
        case reportedReqCount(n) => n.toInt
        case u => fail(s"Unable to extract req count from [$u].")
      }

    val reportedReqFormulaText: Option[String] = {
      val m = reportedReqFormula.matcher(text)
      if (m.matches) {
        val f = m group 1
        if (f == "0 deleted") None else Some(f)
      } else
        None
    }

    val reportedReqFormulaValue: Option[Int] =
      reportedReqFormulaText.map{ t =>
        val f = nonFormula.replaceAllIn(t, "")
        val i = new Calculator(f).InputLine.run()
        //println(s"$t  ==>  $f  ==  $i")
        i
      }
  }

  def selectableCols = viewSettings.columns.allColumns
  def filterDead = viewSettings.filterDead.value
}
