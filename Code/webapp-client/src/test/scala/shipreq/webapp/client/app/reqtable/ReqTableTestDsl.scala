package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import japgolly.scalajs.react.test._
import org.scalajs.dom.ext.{KeyCode, KeyValue}
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.data._
import shipreq.webapp.client.test._
import testate.domzipper.DomZipper.EditableSel
import TestState._

object ReqTableTestDsl {

  case class Ref($: CompState.AccessD[ReqTable.State], svr: MockServer)

  val * = Dsl[Ref, ReqTableObs, Project]

  def apply(action: *.Action = *.emptyAction): *.Plan =
    Plan(action, invariants)

//  // TODO Move following into Nyaya
//
//  import scala.util.Try
//  def propTrySuccess(name: => String): Prop[Try[Any]] =
//    Prop.test(name, _.isSuccess)
//
//  def propTry[A](name: => String, f: A => Any): Prop[A] =
//    propTrySuccess(name).contramap(a => Try(f(a)))

  def selectVisibleColumns(isOn: Column => Boolean, p: ProjectConfig, fd: FilterDead): NonEmptyVector[Column] = {
    // I want Pubid as the first column so that obs.table.entireContent is readable
    val set: Set[Column] = Column.mandatory ++ Column.all(p, fd).whole.filter(isOn) - Column.Pubid
    NonEmptyVector(Column.Pubid, set.toVector)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Focuses

  val mandatoryColumns = FilterDead.memo(fd =>
    Column.mandatory.iterator
      .filter(fd.filterFnA(_.live))
      .map(Column.NameResolver.builtIn)
      .toSet)

  def visibleColumns(obs: ReqTableObs): Set[String] =
    mandatoryColumns(obs.filterDead) ++ obs.viewSettings.columns.onColumns

  val selectableColumns = *.focus("Selectable columns").collection(_.obs.selectableCols)

  val filterDead = *.focus("FilterDead").value(_.obs.filterDead)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)

  val svrReqs = *.focus("Server requests").value(_.obs.svrReqs.length)

  val svrLastTwoReqs =
    *.focus("Retry requests").compare(_.obs.svrReqs.last, _.obs.svrReqs.init.last)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val editorInvalidSel: String =
    "." + Style.reqtable.cellEditor(Invalid).className.value +
    ",." + Style.reqtable.cellEditorErrMsg.className.value

  val ctrlEnter = KeyboardEventData(key = KeyValue.Enter, keyCode = KeyCode.Enter, ctrlKey = true)

  val escape = KeyboardEventData(key = KeyValue.Escape, keyCode = KeyCode.Escape)

  sealed abstract class CellState
  case object Normal  extends CellState
  case object Editing extends CellState
  case object Locked  extends CellState
  case object Failed  extends CellState

  def cellEditor(pubid: String, col: String): CellEditor =
    CellEditor(_.table.cellLoc(pubid = pubid, col = col))

  final case class CellEditor(loc: ReqTableObs => ReqTableObs.CellLoc) {

    private implicit def ROStoOS(r: *.ROS) = r.os

    private def editorCss      = EditableSel
    private def retryButtonCss = "button:contains(Retry)"
    private def abortButtonCss = "button:contains(Abort)"

    val cell        = *.focus("Subject cell").value(s => s.obs.table.cell(loc(s.obs)))
    val cellText    = cell.map(_.innerText)                          rename "Cell innerText"
    val retryButton = cell.map(_(retryButtonCss).domAs[html.Button]) rename "Retry button"
    val abortButton = cell.map(_(abortButtonCss).domAs[html.Button]) rename "Abort button"
    val editor      = cell.map(_(editorCss).forceDomAs[html.Input])  rename "Editor"
    val editorValue = editor.map(_.value)                            rename "Editor value"

    private val _editing = cell.map(_ exists editorCss)      rename "Editing"
    private val _locked  = cell.map(_ exists "img")          rename "Locked"
    private val _failed  = cell.map(_ exists retryButtonCss) rename "Async failure"

    val editorValidity = *.focus("Editor validity").value(Invalid <~ cell.run(_).exists(editorInvalidSel))

    def assertState(s: CellState) = {
      var e,l,f = false
      s match {
        case Normal  => ()
        case Editing => e = true
        case Locked  => l = true
        case Failed  => f = true
      }
      _editing.assert(e) & _locked.assert(l) & _failed.assert(f)
    }

    val assertNotEditing =
      _editing.assert(false)

    val tryStartEdit =
      *.action("Start editor.")(Simulate doubleClick cell.run(_).dom)

    val startEdit = (
      tryStartEdit
        +> svrReqs.assert.noChange
        +> assertState(Editing))

    val assertCantStartEdit = (
      tryStartEdit.rename("Attempt to start editor.")
        +> svrReqs.assert.noChange
        +> assertNotEditing)

    def enterValue(text: String, desc: String = "Enter value") =
      *.action(s"$desc: ${text.display}")(ChangeEventData(text) simulate editor.run(_)) +>
        editorValue.assert(text)

    def modifyValue(mod: String => String, desc: String = "Modify value") =
      *.chooseAction(desc + ".")(i => {
        val value1 = editorValue.run(i)
        val value2 = mod(value1)
        enterValue(value2, desc)
      })

    def testValid  (text: String) = enterValue(text, "Enter valid value")   +> editorValidity.assert(Valid)
    def testInvalid(text: String) = enterValue(text, "Enter invalid value") +> editorValidity.assert(Invalid)

    val commit =
      *.action("Press Ctrl-Enter.")(ctrlEnter simulateKeyDown editor.run(_)) +> assertNotEditing

    val abortEdit =
      *.action("Press Escape.")(escape simulateKeyDown editor.run(_)) +> assertState(Normal)

    val clickRetry =
      *.action("Click Retry.")(Simulate click retryButton.run(_))

    val clickAbort =
      *.action("Click Abort.")(Simulate click abortButton.run(_))
  }


  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Invariants

  val invariants = {

    def selectableColumns = {
      def ** = ReqTableTestDsl.selectableColumns

      val uniqueColumns =
        **.assert.distinct

      def customFieldNames(project: Project, a: Live): Set[String] = {
        val cfname = CustomField.nameP(project)
        project.config.fields.customFields.valuesIterator
          .filter(_.live(project.config) ==* a).map(cfname)
          .toSet
      }

      val liveCustomFieldColumnsAlwaysAvailable =
        **.assert.containsAll("live custom field columns")(i => customFieldNames(i.state, Live))

      val deadColumns =
        **.assert.existenceOfAllBy("dead custom field columns")(i => customFieldNames(i.state, Dead))(
          _.obs.filterDead :: ShowDead)

      uniqueColumns & liveCustomFieldColumnsAlwaysAvailable & deadColumns
    }

    def sortColumns = {
      val names = *.focus("Sort criteria").collection(_.obs.sorting.names)
      names.assert.distinct &
      names.assert.containsOnly("visible columns")(i => visibleColumns(i.obs))
    }

    def tableColumns =
      *.focus("Table columns").collection(_.obs.table.fieldColumns)
        .assert.equalIgnoringOrderBy(i => visibleColumns(i.obs))

    def tableContents = {
      val rowEitherDeadOrLive = *.focus("")
        .compare(
          _.obs.table.allRows.length,
          i => i.obs.table.liveRows.length + i.obs.table.deadRows.length)
        .assert.equal
        .rename("Rows are either dead or live.")

//      val oneFocusMax = propTry[S]("Maximum one focus", _.table.focus)
//
//      rowEitherDeadOrLive & oneFocusMax
      rowEitherDeadOrLive
    }

    def stats = {
      val rowCount =
        *.focus("")
        .compare(_.obs.table.allRows.length, _.obs.stats.reportedRows)
        .assert.equal
        .rename("Reported row count matches rows in table.")

      val reqFormula =
        *.point("Req formula.")(os => {
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

    selectableColumns & sortColumns & tableColumns & tableContents & stats
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Actions

  implicit def autoGetDomFromZipper(d: DomZipper): ReactOrDomNode = d.dom.domAsHtml

  def applyViewSettings(vs: ViewSettings): *.Action =
    applyViewSettings("ApplyViewSettings: " + vs, _ => vs)

  def applyViewSettings(name: => String, f: ReqTable.State => ViewSettings): *.Action =
    *.action(name)(_.ref.$.modState(s => s.copy(viewSettings = f(s))))

  def setProject(p: Project): *.Action =
    *.action("Set project.")(_.ref.$.modState(_.updateProject(p))).updateState(_ => p)

  def showAllColumns: *.Action =
    showAllColumns(ShowDead)

  def showAllColumns(fd: FilterDead): *.Action =
    applyViewSettings("Show all columns.", s => {
      val vs = s.viewSettings
      val cs = selectVisibleColumns(_ => true, s.project.config, fd)
      val o  = vs.order.copy(init = Vector.empty) // remove ReqCodeGroups
      vs.copy(columns = cs, order = o, filterDead = fd)
    })

  val showBuiltInColumnsSortedByPubid = applyViewSettings("Show built-in columns sorted by pubid.", s => {
    val fd = ShowDead
    val cs = selectVisibleColumns(Column.builtInValues.whole.contains, s.project.config, fd)
    ViewSettings(cs, SortCriteria.byPubidOnly, None, fd)
  })

  def showHideColumn(columnName: String): *.Action =
    *.action("Show/hide " + columnName)(
      Simulation.change run _.obs.viewSettings.columns.column(columnName).checkbox)

  def sortBy(columnName: String): *.Action =
    *.action("Sort by " + columnName)(
      Simulation.click run _.obs.table.column(columnName).headerCell)

  val sortByPubid =
    sortBy(UiText.ColumnNames.pubid)

  def enterFilter(f: String) = {
    val e = ChangeEventData(f)
    *.action(s"enterFilter('$f')")(e simulate _.obs.viewSettings.filter.input)
      .addCheck(*.focus("Filter").value(_.obs.viewSettings.filter.input.value).assert(f).after)
  }

  val filterDeadToggle =
    *.action("filterDeadToggle")(Simulate change _.obs.viewSettings.filterDead.checkbox)
      .addCheck(filterDead.assert.change)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd).rename(s"setFilterDead($fd)")

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.viewSettings.columns.onColumns).assert.noChange)

  val logTable = *.print(_.obs.table.entireContent)

  val svrDisableAutoRespond = *.action("Disable auto-respond.")(_.ref.svr.autoRespond = false)

  val svrAutoRespondToLast = *.action("Server responds.")(_.ref.svr.autoRespondToLast())

  val svrFailLast = *.action("Fail last server request.")(_.ref.svr.failLast())

  val svrAssertLastTwoReqsEqual = svrLastTwoReqs.map(_.input).assert.equal(Equal.by_==, implicitly)
}
