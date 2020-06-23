package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.test.BaseTestUtil.quoteString
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.savedview._
import shipreq.webapp.base.event.Event
import shipreq.webapp.base.feature.clipboard.TestClipboard
import shipreq.webapp.base.test._
import shipreq.webapp.base.util.Browser
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.feature.SavedViewFeature
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.feature.savedview.SavedViewTestDsl
import shipreq.webapp.client.project.test._
import teststate.domzipper.DomZipper.EditableSel

object ReqTableTestDsl {
  import TestState._

  final case class Ref(savedViewState: StateAccessImpure[SavedViewFeature.State],
                       global: TestGlobal,
                       promptJs: TestPromptJs)

  val * = Dsl[Ref, ReqTableObs, Project]

  val svr = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  def apply(action: *.Actions = *.emptyAction): *.Plan =
    Plan(action, invariants)

  val savedViews = SavedViewTestDsl(*)(_.savedViews, _.filterDead, _.filter, _.promptJs)

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
    mandatoryColumns(obs.filterDead.value) ++ obs.columnSelector.onColumns

  val tableColumns = *.focus("Table columns").collection(_.obs.table.fieldColumns)

  val selectableColumns = *.focus("Selectable columns").collection(_.obs.columnSelector.allColumns)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)

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

    val editorError = *.focus("Editor error").option(cell.run(_).collect01(editorInvalidSel).innerTexts)

    val noEditorError = editorError.assert(None)

    def assertState(s: CellState) = {
      var e,l = false
      var v: Validity = Valid
      s match {
        case Normal  => ()
        case Editing => e = true
        case Locked  => l = true
        case Failed  => v = Invalid; e = true
      }
      _editing.assert(e) & _locked.assert(l) & editorValidity.assert(v) & noEditorError.when(_ => v is Valid)
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
        +> svr.requestCount.assert.noChange
        +> assertState(Editing))

    val assertCantStartEdit = (
      tryStartEdit.rename("Attempt to start editor.")
        +> svr.requestCount.assert.noChange
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
          _.obs.filterDead.value is ShowDead)

      uniqueColumns & liveCustomFieldColumnsAlwaysAvailable & deadColumns
    }

    def sortColumns = {
      val names = *.focus("Sort criteria").collection(_.obs.sorting.names)
      names.assert.distinct &
      names.assert.containsOnly("visible columns")(i => visibleColumns(i.obs))
    }

    def tableColumns =
      ReqTableTestDsl.tableColumns.assert.equalIgnoringOrderBy(i => visibleColumns(i.obs))

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

  def setViewSettings(name: => String, fd: FilterDead, mod: (Project, View) => View): *.Actions =
    (savedViews.setFilterDead(fd) >> *.action("setView")(i =>
      i.ref.savedViewState.modState(_.modifyView(i.state, fd, updateFilterText = true)(mod(i.state, _)))))
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
      View(cs, SortCriteria.byPubidOnly, s.filterDead, None, None)
    })

  val showMandatoryColumnsSortedByPubid: *.Actions =
    setViewSettings("Show mandatory columns sorted by pubid.", HideDead, (p, s) => {
      val cs = selectVisibleColumns(Column.isMandatory, p, HideDead)
      View(cs, SortCriteria.byPubidOnly, s.filterDead, None, None)
    })

  def showHideColumn(columnName: String): *.Actions =
    *.action("Show/hide " + columnName)(
      Simulation.change run _.obs.columnSelector.column(columnName).checkbox)

  def sortBy(columnName: String): *.Actions =
    *.action("Sort by " + columnName)(
      Simulation.click run _.obs.table.column(columnName).headerCell)

  val sortByPubid =
    sortBy(SpecialBuiltInField.Pubid.name)

  val filterDeadShowHide =
    savedViews.setFilterDead(HideDead) >>
    savedViews.filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.columnSelector.onColumns).assert.noChange)

  val logTable = *.print(_.obs.table.entireContent)

  def receiveExternalEvent(e: Event): *.Actions =
    svr.receiveExternalEvent(e)
      .updateState(WebappTestUtil.applyEventSuccessfully(_, e))

  def setFocus(f: ReqTableObs => html.Element): *.Actions =
    *.action("Set focus")(i => f(i.obs).focus()) +>
      activeElement.assert.equalBy(i => f(i.obs))

  def press(k: SimEvent.Keyboard): *.Actions =
    *.action(s"Press ${k.desc}.")(k simulateKeyDownPressUp _.obs.activeElement)

  def copyToClipboard(text: String): *.Actions =
    *.action(s"Copy to clipboard: ${quoteString(text)}")(_ => TestClipboard.writeText(text))
}