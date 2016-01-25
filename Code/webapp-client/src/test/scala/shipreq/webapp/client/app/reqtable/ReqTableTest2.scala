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
import DomZipper.Implicits._
import teststate._

// =====================================================================================================================

object Stuff {
  val * = Dsl[Unit, ReqTableObs, Project, String]

//  // TODO Move following into Nyaya
//
//  import scala.util.Try
//  def propTrySuccess(name: => String): Prop[Try[Any]] =
//    Prop.test(name, _.isSuccess)
//
//  def propTry[A](name: => String, f: A => Any): Prop[A] =
//    propTrySuccess(name).contramap(a => Try(f(a)))

  val mandatoryColumns = FilterDead.memo(fd =>
    Column.mandatory.iterator
      .filter(fd.filterFnA(_.live))
      .map(Column.NameResolver.builtIn)
      .toSet)

//  def propO[O](name: String, f: String => Prop[S]) = {
//    val p = f(name)
//    *.point(_ => name, i => {val r = p(i.obs); if (r.success) None else Some(r.failureTree)})
//  }

  val invariants = {

    def selectableColumns = {
      val ** = *.focus("Selectable columns").collection(_.obs.selectableCols)

      val uniqueColumns =
        **.assertDistinct

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
          _.obs.filterDead :: ShowDead,
          i => customFieldNames(i.state, Dead))

      uniqueColumns & liveCustomFieldColumnsAlwaysAvailable & deadColumns
    }

//    def sortableColumns = equal("Sortable columns = selected VS columns")(
//      _.viewSettings.sorting.visibleColumns.sorted, _.viewSettings.columns.onColumns.sorted)

    def tableColumns =
      *.focus("Table columns").collection(_.obs.table.fieldColumns)
        .assertEqualIgnoringOrder(_ + " contents",
          i => mandatoryColumns(i.obs.filterDead) ++ i.obs.viewSettings.columns.onColumns)

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
//      selectableColumns & sortableColumns & tableColumns & tableContents & stats)
    selectableColumns & tableColumns & tableContents & stats
  }

  // ===================================================================================================================

  implicit def equalFromUnivEq[A: UnivEq] = teststate.Equal.byUnivEq[A]
  implicit def autoGetDomFromZipper(d: DomZipper): ReactOrDomNode = d.dom.domAsHtml
  implicit val showFilterDead = Show.byToString[FilterDead]

  def enterFilter(f: String) = {
    val e = ChangeEventData(f)
    *.action(s"Enter filter: '$f'").act(e simulate _.obs.viewSettings.filter.input).noStateUpdate
  }

  val filterDeadToggle =
    *.action("filterDeadToggle").act(Simulate change _.obs.viewSettings.filterDead.$).noStateUpdate
      .addCheck(*.focus("FilterDead").value(_.obs.filterDead).assertChanges)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd)

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.viewSettings.columns.onColumns).assertDoesntChange)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)

  def testFilter = (
    enterFilter("-MF")
      >> filterDeadToggle
        .addCheck(tablePubids.before.assertEqualIgnoringOrder(_ + " (before)", _ => List("FR-1", "FR-2")))
        .addCheck(tablePubids.after .assertEqualIgnoringOrder(_ + " (after)", _ => List("FR-1", "FR-2", "CO-1", "CO-2")))
      >> enterFilter("FR")
      >> filterDeadToggle
        .addCheck(tablePubids.assertEqualIgnoringOrder(s => s, _ => List("FR-1", "FR-2")).beforeAndAfter)
  )
}

// ===================================================================================================================

object ReqTableTest2 extends TestSuite {

  PrepareEnv()

  import Stuff._

  val createRemote = RemoteFn.Instance("x", CreateContentFn)
  val updateRemote = RemoteFn.Instance("x", UpdateContentFn)

  def runTest(action: *.Action) = {
    val cd = new ClientData(SampleProject3.project)
    val cp = new TestClientProtocol
    val stateVar = ReactTestVar(ReqTable.State.init(cd, HideDead, None))
    val reqTable = new ReqTable(cd, cp, createRemote, updateRemote, stateVar.compStateAccess())
    val c = ReactTestUtils renderIntoDocument reqTable.Component(stateVar.initialValue)

    try {
      val tt = new Test0(action, invariants).observe(_ => new ReqTableObs(DomZipper(c)))
      val h = tt.run(stateVar.value().project, ())
      if (h.failed) {
        println()
        println(formatHistory(h, Options.colored))
        println()
      }
    } finally {
      ReactDOM.unmountComponentAtNode(c.getDOMNode().parentNode)
    }
  }

  override def tests = TestSuite {
    'initialState - runTest(Action.empty)
    'filter - runTest(testFilter)
  }
}