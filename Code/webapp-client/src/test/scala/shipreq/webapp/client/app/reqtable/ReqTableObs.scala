package shipreq.webapp.client.app.reqtable

import org.parboiled2.Parser.DeliveryScheme.Throw
import org.scalajs.dom.html
import shipreq.webapp.client.data._
import shipreq.base.util.UnivEq.{apply => _, force => _, _}
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.test._
import shipreq.webapp.client.widgets.Checkbox
import DomZipper.IntExt // TODO tmp
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
final class ReqTableObs(val $ : DomZipper) {
  import ReqTableObs._

  object viewSettings {
    val $ = ReqTableObs.this.$.down("ViewSettings", ">table", 1 of 2)
    def vsCol(i: Int) = $.down("column #" + i, "tbody tr").down(">td", i of 3)

    object columns {
      val entirety: Vector[(On, String)] =
        vsCol(1).collect1("label", l =>
          (On <~ l.down("input").to_![html.Input].checked, l.down(">span").innerHTML))

      val allColumns: Vector[String] =
        entirety.map(_._2)

      val onColumns: Vector[String] =
        entirety.filter(_._1 :: On).map(_._2)
    }

//    object sorting {
//      val $ = vsCol(2)
//
//      private val all = (SortMethod.ignoreBlanks ++ SortMethod.considerBlanks).whole
//      private val readSortMethod: String => Option[SortMethod] = {
//        case "Unused" => None
//        case s => all.find(_.optionLabel == s).fold(sys error s"Unknown sort method: $s")(Some(_))
//      }
//
//      private val readSortMethodIB: String => SortMethod.IgnoreBlanks =
//        s => SortMethod.ignoreBlanks.whole.find(_.optionLabel == s).getOrElse(sys error s"Unknown sort method: $s")
//
//      val inconclusive: Vector[(Option[SortMethod], String)] =
//        $.down("ol").collect("li", li =>
//          (li.down("select").selectedOptionText.get |> readSortMethod, li.down("select + span").innerHTML))
//
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
//    }

    object filter {
      val $ = vsCol(3)
      val input = $.down("textarea")
      val value = input.value
    }

    object filterDead {
      val checkbox = filter.$.down("input[type=checkbox]")
      val value: FilterDead = Checkbox.filterDeadChecked <~ checkbox.to_![html.Input].checked
    }
  }

  object table {
    val $ = ReqTableObs.this.$.down("ReqTable", ">table", 2 of 2)
    val tbody = $.down("ReqTable", ">tbody")

    val columns: Vector[String] =
      $.down(">thead") collectInnerText1 "th"

    val fieldColumns: Vector[String] =
      columns.drop(1)

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

    val allRows  = tbody getAll ">tr"
    val deadRows = tbody getAll byStatus(DeadRow, row)
    val liveRows = tbody getAll byStatus(Normal, row)
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
      tbody collectInnerText s">tr >td:nth-child(${pubidColumnIndex + 1})"

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
