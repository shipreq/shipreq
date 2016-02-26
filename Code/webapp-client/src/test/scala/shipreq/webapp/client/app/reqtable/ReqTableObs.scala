package shipreq.webapp.client.app.reqtable

import org.parboiled2.Parser.DeliveryScheme.Throw
import org.scalajs.dom.html
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{UnivEq, univEqOps}
import shipreq.base.util.UnivEq.{apply => _, force => _, _}
import shipreq.webapp.client.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.test._
import shipreq.webapp.client.widgets.Checkbox
import DomZipper.Implicits._

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
final class ReqTableObs($ : DomZipper) {
  import ReqTableObs._

  def findOne[A: UnivEq, B](a: A, bs: Iterable[B])(f: B => A): B =
    bs.iterator.filter(f(_) ==* a).toList match {
      case b :: Nil => b
      case x => sys error s"Expected to find one result for '$a' but found: $x. Available are: ${bs.iterator.map(f).mkString(", ")}."
    }

  object viewSettings {
    val $ = ReqTableObs.this.$.down("ViewSettings", ">table", 1 of 2)
    def vsCol(i: Int) = $.down("column #" + i, "tbody tr").down(">td", i of 3)

    object columns {

      case class ColumnDom(outer: DomZipperAt[html.Label]) {
        val checkbox = outer.down("input").as[html.Input]
        val on       = On <~ checkbox.inputChecked
        val name     = outer.down(">span").innerHTML
      }

      val entirety: Vector[ColumnDom] =
        vsCol(1).collect1("label").as[html.Label].map(ColumnDom)

      def column(name: String): ColumnDom =
        findOne(name, entirety)(_.name)

      val allColumns: Vector[String] =
        entirety.map(_.name)

      val onColumns: Vector[String] =
        entirety.filter(_.on :: On).map(_.name)
    }

    object filter {
      val $ = vsCol(3)
      val input = $.down("textarea")
      val value = input.value
    }

    object filterDead {
      val checkbox = filter.$.down("input[type=checkbox]")
      val value: FilterDead = Checkbox.filterDeadChecked <~ checkbox.inputChecked
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

    val $: DomZipper = ReqTableObs.this.$.down("Sort row", ">div:contains('Sort')")

    //    val criteriaDom = $.collect1("tr", tr => (
    //      tr.down("td", 2 of 2).innerText,
    //      tr.down("td", 1 of 2).down("*[title]").domAs[html.Element].title))

    case class CriteriaDom(nameDom: html.Element, orderDom: html.Element) {
      val name = nameDom.textContent
    }

    val criteriaDom = $.collect1("tr").map(tr => CriteriaDom(
      tr.down("td", 2 of 2).asHtml.dom,
      tr.down("td", 1 of 2).down("*[title]").asHtml.dom))

    val names: Vector[String] =
      criteriaDom.map(_.name)

//      val inconclusive: Vector[(String, SortMethod)] =
//        $.down("ol").collect("li", li =>
//          (li.down("select").selectedOptionText.get |> readSortMethod, li.down("select + span").innerHTML))

//      val conclusiveOrder: SortMethod.IgnoreBlanks =
//        $.down("ol+div select", 1 of 2).selectedOptionText.get |> readSortMethodIB
//
//      val conclusiveColumnSelected: String =
//        $.down("ol+div select", 2 of 2).selectedOptionText.get
//
//      val conclusiveColumns: Vector[String] =
//        $.down("ol+div select", 2 of 2) collectInnerHTML "option"
//
//      val visibleColumns: Vector[String] =
//        inconclusive.map(_._2) ++ conclusiveColumns
  }

  object table {
    val $ = ReqTableObs.this.$.down("ReqTable", ">table", 2 of 2)
    val tbody = $.down("ReqTable", ">tbody")

    case class ColumnDom(headerCell: html.TableCell) {
      val name = headerCell.textContent
    }

    val columnDoms: Vector[ColumnDom] =
      $.down(">thead").collect1("th").as[html.TableCell].mapDom(ColumnDom)

    val columns: Vector[String] =
      columnDoms map (_.name)

    val fieldColumns: Vector[String] =
      columns.drop(1)

    def column(name: String): ColumnDom =
      findOne(name, columnDoms)(_.name)

    import ColumnRenderer.{Status, Normal, DeadRow}

    private def cell(s: Status): String =
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

    private def byStatus(s: Status, wrap: String => String): String =
      wrap(cell(s))

    val allRows  = tbody collect0 ">tr" get()
    val deadRows = tbody collect0 byStatus(DeadRow, row) get()
    val liveRows = tbody collect0 byStatus(Normal, row) get()
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
      tbody collect0 s">tr >td:nth-child(${pubidColumnIndex + 1})" innerText()

    def rowIndexByPubid(pubid: String): Int =
      findIndex(pubid, rowPubids, s"Row with pubid [$pubid] not found.")

//    def cell(loc: CellLoc): DomZipper =
//      cell(row = loc.row, col = loc.col)
//
//    def cell(row: Int, col: Int): DomZipper =
//      tbody down s">tr:nth-child(${row + 1}) >td:nth-child(${col + 1})"
//
//    def cell(pubid: String, col: String): DomZipper =
//      cell(cellLoc(pubid, col))
//
//    def cellLoc(pubid: String, col: String): CellLoc =
//      CellLoc(row = rowIndexByPubid(pubid), columnIndex(col))
//
//    def entireContent =
//      tbody.collect(">tr", _.collectInnerText(">td").mkString("│ ", " │ ", " │")).mkString("\n")
  }

  // ===================================================================================================================

  object stats {
    val text = $.down("Stats", ">div", 2 of 4).innerText

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
