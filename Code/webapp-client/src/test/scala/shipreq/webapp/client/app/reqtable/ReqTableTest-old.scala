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
import scalajs.js
import scalaz.Equal
import scalaz.std.option._
import scalaz.syntax.equal._
import utest.TestSuite
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
import shipreq.webapp.client.test.{TestClientProtocol, DomZipper, PrepareEnv}
import shipreq.webapp.client.test.TestUtil.fakeKeyboardEvent
import shipreq.webapp.client.widgets.Checkbox
import UpdateContentCmd._
/*
object ReqTableScreen {
  case class CellLoc(row: Int, col: Int)

  val reportedRowCount = "^(\\d+) row.*".r
  val reportedReqCount = ".*\\D(\\d+) reqs?.*".r
  val reportedReqFormula = ".*\\d reqs? +\\((.+?)\\).*".r.pattern

  val nonFormula = "[^0-9+-]+".r
}
import ReqTableScreen.CellLoc

/**
 * Data representation of a rendered ReqTable.
 *
 * Inspects actual DOM to derive values.
 */
final class ReqTableScreen(root: => DomZipper) {
  import ReqTableScreen._

  lazy val $ = root

  object viewSettings {
    lazy val $ = ReqTableScreen.this.$(2, ">table", 0)
    def vsCol(i: Int) = $("tbody tr")(3, ">td", i)

    object columns {
      lazy val entirety: Vector[(On, String)] =
        vsCol(0)(">ol").collectD("li", li =>
          (On <~ li("input").as[html.Input].checked, li("label span").innerHTML))

      lazy val allColumns: Vector[String] =
        entirety.map(_._2)

      lazy val onColumns: Vector[String] =
        entirety.filter(_._1 :: On).map(_._2)
    }

    object sorting {
      lazy val $ = vsCol(1)

      private val all = (SortMethod.ignoreBlanks ++ SortMethod.considerBlanks).whole
      private val readSortMethod: String => Option[SortMethod] = {
        case "Unused" => None
        case s => all.find(_.optionLabel == s).fold(sys error s"Unknown sort method: $s")(Some(_))
      }

      private val readSortMethodIB: String => SortMethod.IgnoreBlanks =
        s => SortMethod.ignoreBlanks.whole.find(_.optionLabel == s).getOrElse(sys error s"Unknown sort method: $s")

      lazy val inconclusive: Vector[(Option[SortMethod], String)] =
        $("ol").collectD("li", li =>
          (li("select").selectedOptionText |> readSortMethod, li("select + span").innerHTML))

      lazy val conclusiveOrder: SortMethod.IgnoreBlanks =
        $(2, "ol+div select", 0).selectedOptionText |> readSortMethodIB

      lazy val conclusiveColumnSelected: String =
        $(2, "ol+div select", 1).selectedOptionText

      lazy val conclusiveColumns: Vector[String] =
        $(2, "ol+div select", 1) collectInnerHTML "option"

      lazy val visibleColumns: Vector[String] =
        inconclusive.map(_._2) ++ conclusiveColumns
    }

    object filter {
      lazy val $ = vsCol(2)

      lazy val input = $("textarea")
    }

    object filterDead {
      lazy val $ = filter.$("label input")

      lazy val value: FilterDead =
        Checkbox.filterDeadChecked <~ $.as[html.Input].checked
    }
  }

  object table {
    lazy val $ = ReqTableScreen.this.$(2, ">table", 1)
    lazy val tbody = $(">tbody")

    lazy val columns: Vector[String] =
      $(">thead") collectInnerText "th"

    import ColumnRenderer.{Status, Normal, DeadRow}

    private def cell(s: Status, focus: Boolean): String =  {
      var r = "td." + Style.reqtable.cell(s).className.value
      if (focus)
        r += ":focus"
      else
        r += ":not(:focus)"
      r
    }

    private def row(inner: String): String =
      s">tr:has($inner)"

    private def byFocus(focus: Boolean, wrap: String => String): String =
      ColumnRenderer.statusDomain.toStream.map(s => wrap(cell(s, focus))).mkString(",")

    private def byStatus(s: Status, wrap: String => String): String =
      Vector(true, false).map(f => wrap(cell(s, f))).mkString(",")

    lazy val allRows  = tbody getAll ">tr"
    lazy val deadRows = tbody getAll byStatus(DeadRow, row)
    lazy val liveRows = tbody getAll byStatus(Normal, row)
    lazy val focusRow = tbody option byFocus(true, row)
    lazy val focus    = tbody option byFocus(true, identity)

    lazy val inputsInFocusRow: Option[Int] =
      focusRow.map(_.getAll("input,select,textarea").length)

    def ensureHasFocus(): Unit =
      focus getOrElse fail("No focus.")

    private def findIndex(subj: String, in: Vector[String], err: => String): Int = {
      val i = in.indexOf(subj)
      if (i < 0) fail(s"$err\n$in")
      i
    }

    def columnIndex(title: String): Int =
      findIndex(title, columns, s"Column '$title' not found.")

    lazy val pubidColumnIndex =
      columnIndex("ID")

    lazy val rowPubids: Vector[String] =
      tbody collectInnerText s">tr >td:nth-child(${pubidColumnIndex + 1})"

    def rowIndexByPubid(pubid: String): Int =
      findIndex(pubid, rowPubids, s"Row with pubid [$pubid] not found.")

    def cell(loc: CellLoc): DomZipper =
      cell(row = loc.row, col = loc.col)

    def cell(row: Int, col: Int): DomZipper =
      tbody(s">tr:nth-child(${row + 1}) >td:nth-child(${col + 1})")

    def cell(pubid: String, col: String): DomZipper =
      cell(cellLoc(pubid, col))

    def cellLoc(pubid: String, col: String): CellLoc =
      CellLoc(row = rowIndexByPubid(pubid), columnIndex(col))

    def entireContent =
      tbody.collectD(">tr",
        _.collectInnerText(">td").mkString("│ ", " │ ", " │")
      ).mkString("\n")
  }

  object stats {
    lazy val text = $(2, ">div", 1).innerText

    lazy val reportedRows: Int =
      text match {
        case reportedRowCount(n) => n.toInt
        case u => fail(s"Unable to extract row count from [$u].")
      }

    lazy val reportedReqs: Int =
      text match {
        case reportedReqCount(n) => n.toInt
        case u => fail(s"Unable to extract req count from [$u].")
      }

    lazy val reportedReqFormulaText: Option[String] = {
      val m = reportedReqFormula.matcher(text)
      if (m.matches) {
        val f = m group 1
        if (f == "0 deleted") None else Some(f)
      } else
        None
    }

    lazy val reportedReqFormulaValue: Option[Int] =
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
import shipreq.webapp.client.app.ui.reqtable.{ReqTableScreen => S}

// Mitigate IntelliJ being so slow with μTest
sealed trait ReqTableTest0 {
  PrepareEnv()

  import ReqTable.State
  lazy val vs_order_init = ViewSettings.order ^|-> SortCriteria.init
  lazy val s_order_init = State.viewSettings ^|-> vs_order_init
  lazy val s_filterDead = State.viewSettings ^|-> ViewSettings.filterDead

  lazy val project = SampleProject3.project

  val cp = new TestClientProtocol

  val createRemote = RemoteFn.Instance("x", CreateContentFn)
  val updateRemote = RemoteFn.Instance("x", UpdateContentFn)

  def propsForProject(p: Project) =
    ReqTable.Props(new ClientData(p), cp, createRemote, updateRemote, HideDead)

  lazy val initialProps = propsForProject(project)

  lazy val initialState = ReqTable.initialState(initialProps)

  lazy val c = ReactTestUtils renderIntoDocument initialProps.component

  lazy val cTable = Table.Component castM ReactTestUtils.findRenderedComponentWithType(c, Table.Component.jsCtor)

  def reset(): Unit = {
    cp.reset()
    c.setState(initialState).runNow()
  }

  def * = new S(new DomZipper(c.getDOMNode()))

  // ===================================================================================================================
  // Properties

  // TODO Move following into Nyaya

  @inline def existance[A](name: String) = new ExistanceB[A](name)
  final class ExistanceB[A](val name: String) { //extends AnyVal {
    def apply[B](expect: A => Boolean, expected: A => Set[B], testData: A => Traversable[B]): Prop[A] = {
      lazy val yes = Prop.allPresent[A](name + " available")(expected, testData)
      lazy val no = Prop.blacklist[A](name + " not available")(expected, testData)
      Prop.test[A](name, expect).ifelse(yes, no)
    }
  }

  import scala.util.Try
  def propTrySuccess(name: => String): Prop[Try[Any]] =
    Prop.test(name, _.isSuccess)

  def propTry[A](name: => String, f: A => Any): Prop[A] =
    propTrySuccess(name).contramap(a => Try(f(a)))

  case class PS(project: Project, screen: S) {
    lazy val cfname = CustomField.nameP(project)

    def customFieldNames(a: Live): Set[String] = {
      val cfs   = project.config.fields.customFields.values.toStream
      val names = cfs.filter(_.live ==* a).map(cfname(_).unmust)
      names.toSet
    }
  }

  val builtInColumns = Column.builtInValues.map(Column.NameResolver.builtIn).toNES.whole

  val invariants: Prop[PS] = {
    implicit def autoContraS(p: Prop[S]): Prop[PS] = p.contramap[PS](_.screen)
    def equal(name: => String) = Prop.equal[S](name)

    def availableColumns = {
      val uniqueColumns =
        Prop.distinct("Unique columns", (_: S).availCols)

      val builtInColumnsAlwaysAvailable =
        Prop.allPresent[S]("Built-in columns always available")(_ => builtInColumns, _.availCols)

      val liveCustomFieldColumnsAlwaysAvailable =
        Prop.allPresent[PS]("Live custom field columns available")(_ customFieldNames Live, _.screen.availCols)

      val deadColumns =
        existance[PS]("Dead custom field columns")(_.screen.viewSettings.filterDead.value :: ShowDead,
          _ customFieldNames Dead, _.screen.availCols)

      liveCustomFieldColumnsAlwaysAvailable & builtInColumnsAlwaysAvailable & deadColumns & uniqueColumns
    }

    def sortableColumns = equal("Sortable columns = selected VS columns")(
      _.viewSettings.sorting.visibleColumns.sorted, _.viewSettings.columns.onColumns.sorted)

    def tableColumns = equal("Table columns = selected VS columns")(
      _.table.columns, _.viewSettings.columns.onColumns)

    def tableContents: Prop[PS] = {
      val rowEitherDeadOrLive = equal("Rows are either dead or live")(
        _.table.allRows.length,
        t => t.table.liveRows.length + t.table.deadRows.length)

      val oneFocusMax = propTry[S]("Maximum one focus", _.table.focus)

      rowEitherDeadOrLive & oneFocusMax
    }

    def stats = {
      val rowCount = equal("Reported row count")(_.table.allRows.length, _.stats.reportedRows)

      val reqFormula = Prop.atom[S]("Req formula", s => {
        s.stats.reportedReqFormulaValue.flatMap(fv =>
          if (fv == s.stats.reportedReqs)
            None
          else
            Some(s"${s.stats.reportedReqs} !=* $fv (${s.stats.reportedReqFormulaText})")
        )
      })

      "Stats" rename_: (rowCount & reqFormula)
    }

    "Invariants" rename_: (
      availableColumns & sortableColumns & tableColumns & tableContents & stats)
  }

  def assertInvariants(s: S = *): Unit =
    invariants assert PS(project, s)

  // ===================================================================================================================
  // Actions

  object ScreenAction extends ActionTester {
    override protected type S          = ReqTableScreen
    override protected def newState    = *
    override protected def defaultLast = assertInvariants
  }
  import ScreenAction.{S => _, _}

  def actionProp[A](f: A => Action[_]): Prop[A] = {
    Prop.atom("action", a =>
      try {
        run(f(a))
        None
      } catch {
        case e: Throwable => Some(e.getMessage)
      }
    )
  }

  def enterFilter(f: String) = {
    val e = ChangeEventData(f)
    Action(s"enterFilter($f)", e simulate _.viewSettings.filter.input.get)
  }

  val filterDeadToggle =
    Action("filterDeadToggle", Simulate change _.viewSettings.filterDead.$.get)
      .focus(_.viewSettings.filterDead.value)
      .assertChange

  def setFilterDead(fd: FilterDead): Action[Unit] =
    filterDeadToggle.unless(_.viewSettings.filterDead.value == fd)

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).focus(_.viewSettings.columns.onColumns).assertNoChange

  def setProject(p: Project): Action[Unit] =
    Action.exec(s"setProject($p)", c.setState(ReqTable.initialState(propsForProject(p))).runNow())

  val sortByPubid = applyViewSettings("sortByPubid",
    c.state.viewSettings.copy(order = SortCriteria.byPubidOnly))

  def selectVisibleColumns(isOn: Column => Boolean, p: Project = c.state.project): ColumnsEditor.State = {
    val f = isOn || Column.mandatory
    val cols = Column.allInProject(p).whole
    ColumnsEditor.State.init(cols)(On <~ f(_))
  }

  val showAllColumns = applyViewSettings("showAllColumns", {
    val s  = c.state
    val vs = s.viewSettings
    val cs = selectVisibleColumns(_ => true, s.project)
    val o  = vs.order.copy(init = Vector.empty) // remove ReqCodeGroups
    vs.copy(columnState = cs, order = o, filterDead = ShowDead)
  })

  def focusCell(loc: S => CellLoc): Action[Unit] =
    Action.apply2({ s =>
      val l = loc(s)
      //(s"focusCell($l)", l)
      ("focusCell", l)
    })((s, l) => {
      val cell = s.table.cell(l).get
      println("Clicking: " + cell)
      Simulate.click(cell)
      println("After click: " + dom.document.activeElement)
      cell.asInstanceOf[dom.html.Element].focus()
      println("After manual focus: " + dom.document.activeElement)
      println(cell.outerHTML)
      Simulate.doubleClick(cell)
      println(cell.outerHTML)
    })

//  val F2 = fakeKeyboardEvent(keyCode = KeyCode.F2, target = dom.document.body)

  val editFocused = Action("editFocused", { s =>
    println("F2 on: " + dom.document.activeElement)
    s.table.ensureHasFocus()
    println("F2 on: " + dom.document.activeElement)
    KeyboardEventData(keyCode = KeyCode.F2) simulateKeyDownPressUp dom.document.activeElement
//    cTable.backend._onKeyDown(F2).runNow()
//    cTable.backend._onKeyUp(F2).runNow()
  })

  val printTableContent =
    Action.readonly(s => println("\n" + s.table.entireContent + "\n"))

  val ctrlEnter = KeyboardEventData(key = KeyValue.Enter, keyCode = KeyCode.Enter, ctrlKey = true)

  val escape = KeyboardEventData(key = KeyValue.Escape, keyCode = KeyCode.Escape)

  def ioAssertReqsSent(expect: Int) = Action.assert(cp assertReqsSent expect)

  val ioAssertLastTwoUpdateRequestsEqual = Action.assert(cp.assertLastTwoRequestsEqual(updateRemote))

  val ioFailLast = Action.exec("failLast", cp.failLast())

  // ===================================================================================================================
  // Tests

  implicit val settings = DefaultSettings.propSettings.setSampleSize(8) //.setDebug

  def testDeadToggleInvariants(): Unit =
    RandomReqTableData.viewSettings(project, allowFilter = true) mustSatisfy
      actionProp(applyViewSettings("testDeadToggleInvariants", _) >> filterDeadShowHide)

  def testDeadRowsNotEditable(): Unit = {
    val colCount = *.availCols.length

    def focus(rowType: Live, colIndex: Int) =
      Action(s"focus($rowType, $colIndex)", { s =>
        val row = rowType match {
          case Live => DomZipper.first("Live row", s.table.liveRows)
          case Dead => DomZipper.first("Dead row", s.table.deadRows)
        }
        val cell = row.getAll(">td")(colIndex)
        Simulate.click(cell)
      })

    def editAllColumns(rowType: Live): Action[Int] = {
      val editEachCell =
        (0 until colCount).map { c =>
          focus(rowType, c).focus(_.table.focus).assertChange >> editFocused
        }.reduce(_ >> _)

      (showAllColumns >> editEachCell).focus(_.table.inputsInFocusRow getOrElse 0)
    }

    editAllColumns(Dead).assertAfter(0).run()

    // Ensure test logic works
    reset()
    editAllColumns(Live).testAfter(_ > 0, "[Live Row] Cells should be in edit-mode").run()
  }

  def testEditIO(): Unit = {
    val ce = CellEditor(_.table.cellLoc(pubid = "MF-6", col = "Title"))
    import ce._

    val newValue = "issues!"

    val editCommitWithoutChange =
      startEdit.assertAfter("Incompletions") >> commit >> ioAssertReqsSent(0)

    val editChangeCommit = (
      startEdit.assertAfter("Incompletions")
        >> enterValue(newValue)
        >> commit.assertNowLocked
        >> assertEditDoesNothing
        >> ioAssertReqsSent(1)
        >> Action.assert(assert(cp.last.input.toString contains newValue)))

    val fail = (
      ioFailLast.focus(failed_?).assertAfter(true, "Should be in failed state after I/O failure")
        >> assertEditDoesNothing)

    val retry = (
      clickRetry.assertNowLocked
        >> ioAssertReqsSent(2)
        >> ioAssertLastTwoUpdateRequestsEqual)

    val cancelSaveCommitAgain = (
      clickFailOk.assertNowEditing
        >> Action.nop.focus(editorValue).assertAfter(newValue)
        >> commit.assertNowLocked
        >> ioAssertReqsSent(3)
        >> ioAssertLastTwoUpdateRequestsEqual)

    val saveSucceeds = (
      Action.exec("saveSucceeds", cp.respondToLast(updateRemote)(Vector.empty))
        >> Action.nop.assertNoCellState)

    run(editCommitWithoutChange >> editChangeCommit >> fail >> retry >> fail >> cancelSaveCommitAgain >> saveSucceeds)
  }
}

object ReqTableTest extends TestSuite with ReqTableTest0 {
  override def tests = TestSuite {
    'dead {
      'toggle      - testDeadToggleInvariants()
//      'notEditable - testDeadRowsNotEditable()
    }
//    'editor {
//      'io           - testEditIO()
//    }
  }
}
*/ // TODO ReqTableTests disabled