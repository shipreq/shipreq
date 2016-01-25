package shipreq.webapp.client.app.reqtable

import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.test._
import org.parboiled2.Parser.DeliveryScheme.Throw
import org.scalajs.dom, dom.html
import org.scalajs.dom.ext.{KeyCode, KeyValue}
import shipreq.webapp.client.data._
import scalajs.js
import scalaz.Equal
import scalaz.std.option._
import scalaz.syntax.equal._
import utest._
import ReactTestUtils.Simulate
import shipreq.base.util._
import shipreq.base.util.Debug._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UnivEq.{apply => _, force => _, _}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.{CreateContentFn, UpdateContentFn, UpdateContentCmd, RemoteFn}
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.app.state.ClientData
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.lib._
import shipreq.webapp.client.test._
import shipreq.webapp.client.test.TestUtil.fakeKeyboardEvent
import shipreq.webapp.client.widgets.Checkbox
import UpdateContentCmd._
import DomZipper.IntExt // TODO tmp
import DomZipper.Implicits._
import teststate._

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
    }

    object filterDead {
      val $ = filter.$.down("label input")

      val value: FilterDead =
        Checkbox.filterDeadChecked <~ $.to_![html.Input].checked
    }
  }

  object table {
    val $ = ReqTableObs.this.$.down("ReqTable", ">table", 2 of 2)
    val tbody = $.down("ReqTable", ">tbody")

//    val columns: Vector[String] =
//      $.down(">thead") collectInnerText "th"

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
//
//    private def findIndex(subj: String, in: Vector[String], err: => String): Int = {
//      val i = in.indexOf(subj)
//      if (i < 0) fail(s"$err\n$in")
//      i
//    }
//
//    def columnIndex(title: String): Int =
//      findIndex(title, columns, s"Column '$title' not found.")
//
//    val pubidColumnIndex =
//      columnIndex("ID")
//
//    val rowPubids: Vector[String] =
//      tbody collectInnerText s">tr >td:nth-child(${pubidColumnIndex + 1})"
//
//    def rowIndexByPubid(pubid: String): Int =
//      findIndex(pubid, rowPubids, s"Row with pubid [$pubid] not found.")
//
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

  def availCols = viewSettings.columns.allColumns
}

// =====================================================================================================================

object Stuff {
  val * = Dsl[Unit, ReqTableObs, Project, String]
  type S = ReqTableObs

  //import shipreq.webapp.client.app.reqtable.{ReqTableObs => Obs}

//  // TODO Move following into Nyaya
//
//  import scala.util.Try
//  def propTrySuccess(name: => String): Prop[Try[Any]] =
//    Prop.test(name, _.isSuccess)
//
//  def propTry[A](name: => String, f: A => Any): Prop[A] =
//    propTrySuccess(name).contramap(a => Try(f(a)))

  val builtInColumns = Column.builtInValues.map(Column.NameResolver.builtIn).toNES.whole

//  def propO[O](name: String, f: String => Prop[S]) = {
//    val p = f(name)
//    *.point(_ => name, i => {val r = p(i.obs); if (r.success) None else Some(r.failureTree)})
//  }

  val invariants = {

    def availableColumns = {
      val ** = *.focus("Available columns").collection(_.obs.availCols)

      val uniqueColumns =
        **.assertDistinct

      val builtInColumnsAlwaysAvailable =
        **.assertContainsAll(_ + " contains all built-in.", _ => builtInColumns)

      def customFieldNames(project: Project, a: Live): Set[String] = {
        val cfname = CustomField.nameP(project)
        project.config.fields.customFields.valuesIterator
          .filter(_.live(project.config) ==* a).map(cfname)
          .toSet
      }

      val liveCustomFieldColumnsAlwaysAvailable =
        **.assertContainsAll(_ + " contains all live custom field columns.", i => customFieldNames(i.state, Live))

      val deadColumns =
        **.assertExistence("dead custom field columns",
          _.obs.viewSettings.filterDead.value :: ShowDead,
          i => customFieldNames(i.state, Dead))

      liveCustomFieldColumnsAlwaysAvailable & builtInColumnsAlwaysAvailable & deadColumns & uniqueColumns
    }

//    def sortableColumns = equal("Sortable columns = selected VS columns")(
//      _.viewSettings.sorting.visibleColumns.sorted, _.viewSettings.columns.onColumns.sorted)
//
//    def tableColumns = equal("Table columns = selected VS columns")(
//      _.table.columns, _.viewSettings.columns.onColumns)

    def tableContents = {
      val rowEitherDeadOrLive =
        *.assertEqual(_ => "Rows are either dead or live.",
          _.obs.table.allRows.length,
          t => t.obs.table.liveRows.length + t.obs.table.deadRows.length)

//      val oneFocusMax = propTry[S]("Maximum one focus", _.table.focus)
//
//      rowEitherDeadOrLive & oneFocusMax
      rowEitherDeadOrLive
    }

    def stats = {
      val rowCount =
        *.assertEqual(_ => "Reported row count matches rows in table.",
          _.obs.table.allRows.length, _.obs.stats.reportedRows)

      val reqFormula =
        *.point(_ => "Req formula.", os => {
          os.obs.stats.reportedReqFormulaValue.flatMap(fv =>
            if (fv == os.obs.stats.reportedReqs)
              None
            else
              Some(s"${os.obs.stats.reportedReqs} ≠ $fv (${os.obs.stats.reportedReqFormulaText})")
          )
        })

//      "Stats" rename_: (rowCount & reqFormula)
      rowCount & reqFormula
    }

//    "Invariants" rename_: (
//      availableColumns & sortableColumns & tableColumns & tableContents & stats)
    availableColumns & tableContents & stats
  }
}

// ===================================================================================================================

object ReqTableTest2 extends TestSuite {

  PrepareEnv()

  val cd = new ClientData(SampleProject3.project)
  val cp = new TestClientProtocol
  val createRemote = RemoteFn.Instance("x", CreateContentFn)
  val updateRemote = RemoteFn.Instance("x", UpdateContentFn)

  val stateVar = ReactTestVar(ReqTable.State.init(cd, HideDead, None))

  val reqTable = new ReqTable(cd, cp, createRemote, updateRemote, stateVar.compStateAccess())

  def reset(): Unit = {
    cp.reset()
    stateVar.reset()
    cd.setProject(stateVar.initialValue.project)
  }

  lazy val c = ReactTestUtils renderIntoDocument reqTable.Component(stateVar.initialValue)

//  lazy val cTable = ReactTestUtils.findRenderedComponentWithType(c, Table.Component).asInstanceOf[Table.Component.Mounted]

  import Stuff._

  type S = Project
  type O = ReqTableObs
  type E = String

//  import scala.util._
//
//  val TSinvariants = Invariant[O, S, E](_ => "Invariants", (o, s) =>
//    /*
//    Try(invariants.assert(PS(s, o))) match {
//      case Success(_) => None
//      case Failure(f) => Some(f.getMessage)
//    }
//    */
//  {
//    val e: Eval = invariants(PS(s, o))
//    if (e.success) None else Some(e.failureTree)
//
//  }
//  )

  val t = new Test0[Unit, ReqTableObs, Project, String](Action.empty, invariants)
    .observe(_ => new ReqTableObs(DomZipper(c)))

  override def tests = TestSuite {
    'initialState {

//      // TODO should be able to wrap up all first param list of run()
//
//      val r = DomZipper2(c.getDOMNode())
//      r("> table")

      val h = t.run(stateVar.value().project, ())
      println()
      println(formatHistory(h, Options.colored.alwaysShowChildren))
      println()
    }
  }
}