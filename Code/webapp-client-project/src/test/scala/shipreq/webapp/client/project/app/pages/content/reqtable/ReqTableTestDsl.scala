package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil.quoteStringForDisplay
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.reqtable._
import shipreq.webapp.base.feature.clipboard.TestClipboard
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.test._
import shipreq.webapp.base.util.Browser
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.test._
import teststate.domzipper.DomZipper.EditableSel
import TestState._

object ReqTableTestDsl {

  case class Ref($: StateAccessImpure[ReqTablePage.State], global: TestGlobal)

  val * = Dsl[Ref, ReqTableObs, Project]

  def apply(action: *.Actions = *.emptyAction): *.Plan =
    Plan(action, invariants)

//  import scala.util.Try
//  def propTrySuccess(name: => String): Prop[Try[Any]] =
//    Prop.test(name, _.isSuccess)
//
//  def propTry[A](name: => String, f: A => Any): Prop[A] =
//    propTrySuccess(name).contramap(a => Try(f(a)))

  def selectVisibleColumns(isOn: Column => Boolean, p: Project, fd: FilterDead): NonEmptyVector[Column] = {
    // I want Pubid as the first column so that obs.table.entireContent is readable
    val set: Set[Column] =
      Column.mandatory.whole ++ ColumnPlus.All(p, fd).columns.whole.map(_.column).filter(isOn) - Column.Pubid
    NonEmptyVector(Column.Pubid, set.toVector)
  }

  def cmdOrCtrl(kb: SimEvent.Keyboard): SimEvent.Keyboard =
    if (Browser.isMac)
      kb.copy(metaKey = true)
    else
      kb.copy(ctrlKey = true)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Focuses

  val mandatoryColumns: FilterDead => Set[String] =
    FilterDead.memo(fd =>
      ColumnPlus.All(Project.empty)
        .columns
        .iterator
        .filter(cp => Column.isMandatory(cp.column) && ColumnPlus.filterDead(fd)(cp))
        .map(_.name)
        .toSet)

  def visibleColumns(obs: ReqTableObs): Set[String] =
    mandatoryColumns(obs.filterDead) ++ obs.columnSelector.onColumns

  val selectableColumns = *.focus("Selectable columns").collection(_.obs.columnSelector.allColumns)

  val filterDead = *.focus("FilterDead").value(_.obs.filterDead)

  val filterText = *.focus("Filter text").value(_.obs.filterValue)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)

  val svrReqs = *.focus("Server requests").value(_.obs.svrReqs.length)

  val svrLastTwoReqs =
    *.focus("Retry requests").compare(_.obs.svrReqs.last, _.obs.svrReqs.init.last)

  val activeElement = *.focus("activeElement").value(_.obs.activeElement)

  val clipboardText = *.focus("clipboardText").value(_.obs.clipboardText)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val editorInvalidSel: String =
    ".pointing.red.label"

  private val naSel = Style.reqtable.table.`N/A`.selector

  sealed abstract class CellState
  case object Normal  extends CellState
  case object Editing extends CellState
  case object Locked  extends CellState
  case object Failed  extends CellState

  def cellEditor(pubid: String, col: String): CellEditor =
    CellEditor(_.table.cellLoc(pubid = pubid, col = col), s"$pubid: $col")

  final case class CellEditor(loc: ReqTableObs => ReqTableObs.CellLoc, locDesc: String) {

    private def editorCss = EditableSel

    private val cell = *.focus("Subject cell").value(s => s.obs.table.cell(loc(s.obs)))

    val isNA        = cell.map(_.exists(naSel))                      rename "Cell is N/A"
    val cellText    = cell.map(_.innerText)                          rename "Cell innerText"
    val editor      = cell.map(_(editorCss).forceDomAs[html.Input])  rename "Editor"
    val editorValue = editor.map(_.value)                            rename "Editor value"

    private val _editing = cell.map(_ exists editorCss)      rename "Editing"
    private val _locked  = cell.map(_ exists ".loading")     rename "Locked"

    val editorValidity = *.focus("Editor validity").value(Invalid when cell.run(_).exists(editorInvalidSel))

    def assertState(s: CellState) = {
      var e,l = false
      var v: Validity = Valid
      s match {
        case Normal  => ()
        case Editing => e = true
        case Locked  => l = true
        case Failed  => v = Invalid; e = true
      }
      _editing.assert(e) & _locked.assert(l) & editorValidity.assert(v)
    }

    val assertNotEditing =
      _editing.assert(false)

    val focus =
      setFocus(o => o.table.cell(loc(o)).domAsHtml).rename("Focus on " + locDesc)

    val tryStartEdit =
      *.action("Start editor.")(Simulate doubleClick cell.run(_).dom)

    val startEdit = (
      isNA.assert(false)
        +> tryStartEdit
        +> svrReqs.assert.noChange
        +> assertState(Editing))

    val assertCantStartEdit = (
      tryStartEdit.rename("Attempt to start editor.")
        +> svrReqs.assert.noChange
        +> assertNotEditing)

    def enterValue(text: String, desc: String = "Enter value") =
      *.action(s"$desc: ${text.display}")(SimEvent.Change(text) simulate editor.run(_)) +>
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
      *.action("Press Ctrl-Enter.")(KB.Enter.ctrl simulateKeyDown editor.run(_)) +> assertNotEditing

    val abortEdit =
      *.action("Press Escape.")(KB.Escape simulateKeyDown editor.run(_)) +> assertState(Normal)

    // These used to be buttons
    def clickRetry = commit
    def clickAbort = abortEdit

    def change(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
      (cellText.assert(textFromTo._1)
        +> startEdit
        +> editorValue.assert(editorFromTo._1)
        >> enterValue(editorFromTo._2)
        >> commit
        +> cellText.assert(textFromTo._2)
        ).group(s"Change $locDesc from '${textFromTo._1}' to '${textFromTo._2}'")

    def changeAndBack(editorFromTo: (String, String), textFromTo: (String, String)): *.Actions =
      change(editorFromTo, textFromTo) >> change(editorFromTo.swap, textFromTo.swap)

    def changeAndBack(fromTo: (String, String)): *.Actions =
      changeAndBack(fromTo, fromTo)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Invariants

  val invariants = {

    def selectableColumns = {
      def ** = ReqTableTestDsl.selectableColumns

      val uniqueColumns =
        **.assert.distinct

      def customFieldNames(project: Project, a: Live): Set[String] =
        project.config.fields.customFields.valuesIterator
          .filter(_.live(project.config) ==* a).map(f => project.config.fieldName(f.fieldId))
          .toSet

      val liveCustomFieldColumnsAlwaysAvailable =
        **.assert.containsAll("live custom field columns")(i => customFieldNames(i.state, Live))

      val deadColumns =
        **.assert.existenceOfAllBy("dead custom field columns")(i => customFieldNames(i.state, Dead))(
          _.obs.filterDead is ShowDead)

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
        .rename("Rows are either dead or live, but not both.")

//      val oneFocusMax = propTry[S]("Maximum one focus", _.table.focus)
//
//      rowEitherDeadOrLive & oneFocusMax
      rowEitherDeadOrLive
    }

    def stats = {
      val rowCount =
        *.focus("")
        .compare(_.obs.table.allRows.length, _.obs.stats.content.rows)
        .assert.equal
        .rename("Reported row count matches rows in table.")

      val reqBreakdown =
        *.point("Req breakdown.")(os => {
          os.obs.stats.content.reqBreakdownResult.flatMap { result =>
            val expect = os.obs.stats.content.reqs
            Option.when(result !=* expect)(s"$result ≠ $expect [${os.obs.stats.contentLine}]")
          }
        })

      val rowBreakdown =
        *.point("Row breakdown.")(os => {
          val result = os.obs.stats.content.rowBreakdownResult
          val expect = os.obs.stats.content.rows
          Option.when(result !=* expect)(s"$result ≠ $expect [${os.obs.stats.contentLine}]")
        })

      (rowCount & rowBreakdown & reqBreakdown).renameContextFree("Stats")
    }

    selectableColumns & sortColumns & tableColumns & tableContents & stats
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Actions

  implicit def autoGetDomFromZipper(d: DomZipperJs): ReactOrDomNode = d.domAsHtml

  def modState(name: => String, mod: (Project, ReqTablePage.State) => ReqTablePage.State): *.Actions =
    *.action(name)(i => i.ref.$.modState(s => mod(i.state, s)))

  def setViewSettings(name: => String, fd: FilterDead, mod: (Project, View) => View): *.Actions =
    (setFilterDead(fd) >> *.action("setView")(i =>
      i.ref.$.modState(ReqTablePage.State.modifyView(i.state, fd, true)(mod(i.state, _)))))
      .renameContextFree(name)

//  def applyTableSettings(ts: TableSettings): *.Actions =
//    applyTableSettings("ApplyTableSettings: " + ts, _ => ts)

//  def applyTableSettings(name: => String, f: (Project, ReqTablePage.State) => TableSettings): *.Actions =
//    modState(name)
//      i.ref.$.modState(s => s.copy(tableSettings =
//        f(i.ref.project(), s))))

//  def setProject(p: Project): *.Actions =
//    *.action("Set project.")(_.ref.$.modState(_.updateProject(p))).updateState(_ => p)

  def showAllColumns: *.Actions =
    showAllColumns(ShowDead)

  def showAllColumns(fd: FilterDead): *.Actions =
    setViewSettings(s"Show all columns @ $fd", fd, (p, ts) => {
      val cs = selectVisibleColumns(_ => true, p, fd)
      val sc = ts.order.copy(init = Vector.empty) // remove CodeGroups
      ts.copy(columns = cs, order = sc)
    })

  val showBuiltInColumnsSortedByPubid: *.Actions =
    setViewSettings("Show built-in columns sorted by pubid.", ShowDead, (p, s) => {
      val cs = selectVisibleColumns(Column.builtInValues.whole.contains, p, ShowDead)
      View(cs, SortCriteria.byPubidOnly, s.filterDead, None)
    })

  def showHideColumn(columnName: String): *.Actions =
    *.action("Show/hide " + columnName)(
      Simulation.change run _.obs.columnSelector.column(columnName).checkbox)

  def sortBy(columnName: String): *.Actions =
    *.action("Sort by " + columnName)(
      Simulation.click run _.obs.table.column(columnName).headerCell)

  val sortByPubid =
    sortBy(UiText.ColumnNames.pubid)

  def enterFilter(f: String) = {
    val e = SimEvent.Change(f)
    *.action(s"enterFilter('$f')")(e simulate _.obs.filterInput)
      .addCheck(filterText.assert(f).after)
  }

  lazy val filterDeadToggle =
    *.action("filterDeadToggle")(Simulate click _.obs.filterDeadButton)
      .addCheck(filterDead.assert.change)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd).rename(s"setFilterDead($fd)")

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.columnSelector.onColumns).assert.noChange)

  val logTable = *.print(_.obs.table.entireContent)

  val svrDisableAutoRespond = *.action("Disable auto-respond.")(_.ref.global.disableAutoResponse())

  val svrAutoRespondToLast = *.action("Server responds.")(_.ref.global.autoRespondToLast())

  val svrFailLast = *.action("Fail last server request.")(_.ref.global.failLast())

  val svrAssertLastTwoReqsEqual = svrLastTwoReqs.map(_.req).assert.equal(Equal.by_==, implicitly)

  def setFocus(f: ReqTableObs => html.Element): *.Actions =
    *.action("Set focus")(i => f(i.obs).focus()) +>
      activeElement.assert.equalBy(i => f(i.obs))

  def press(k: SimEvent.Keyboard): *.Actions =
    *.action(s"Press ${k.desc}.")(k simulateKeyDownPressUp _.obs.activeElement)

  def copyToClipboard(text: String): *.Actions =
    *.action(s"Copy to clipboard: ${quoteStringForDisplay(text)}")(_ => TestClipboard.writeText(text))
}