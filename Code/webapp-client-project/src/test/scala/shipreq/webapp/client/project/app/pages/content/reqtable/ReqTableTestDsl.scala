package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.webapp.base.test._
import shipreq.webapp.base.util.Browser
import shipreq.webapp.client.project.feature.SavedViewFeature
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.feature.savedview.SavedViewTestDsl
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.test._

object ReqTableTestDsl {
  import TestState._

  final case class Ref(savedViewState: StateAccessImpure[SavedViewFeature.State],
                       global        : TestGlobal,
                       promptJs      : TestPromptJs,
                       confirmJs     : TestConfirmJs)

  val * = Dsl[Ref, ReqTableObs, Project]

  val global = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  val confirmJs = new TestConfirmJs.TestDsl(*)(_.confirmJs, _.confirmJs)

  val newFormButton = new CommonObs.DropdownButton.TestDsl(*, "New-Form button")(_.newForm.button)

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

  val newFormIsVisible = *.focus("New-form is visible").value(_.obs.newForm.form.isDefined)

  val newFormFields = *.focus("New-form fields").collection(_.obs.newForm.form.fold(Vector.empty[String])(_.fields.map(_.name)))

  def newFormEditor(name: String) = new CommonObs.Editor.TestDsl(*, s"New-form $name")(_.newForm.form.get.field(name).editor)

  val tableColumns = *.focus("Table columns").collection(_.obs.table.fieldColumns)

  val selectableColumns = *.focus("Selectable columns").collection(_.obs.columnSelector.allColumns)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)

  val clipboardText = *.focus("clipboardText").value(_.obs.clipboardText)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class CellState
  case object Normal  extends CellState
  case object Editing extends CellState
  case object Locked  extends CellState
  case object Failed  extends CellState

  def cellEditor(pubid: String, col: String): CellEditor =
    CellEditor(_.table.cellLoc(pubid = pubid, col = col), s"$pubid: $col")

  final case class CellEditor(loc: ReqTableObs => ReqTableObs.CellLoc, locDesc: String)
      extends CommonObs.Editor.TestDsl(*, locDesc)(o => o.table.cell(loc(o))) {

    private val cell = *.focus("Subject cell").value(s => s.obs.table.cell(loc(s.obs)))

    val isNA = cell.map(_.isNA) rename "Cell is N/A"

    private val _editing = cell.map(_.editing) rename "Editing"
    private val _locked  = cell.map(_.isSpinning)  rename "Locked"

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
      setFocus(o => o.table.cell(loc(o)).dom).rename("Focus on " + locDesc)

    val tryStartEdit =
      *.action("Start editor.")(Simulate doubleClick cell.run(_).dom)

    val startEdit = (
      isNA.assert(false)
        +> tryStartEdit
        +> global.requestCount.assert.noChange
        +> assertState(Editing))

    val assertCantStartEdit = (
      tryStartEdit.rename("Attempt to start editor.")
        +> global.requestCount.assert.noChange
        +> assertNotEditing)
  } // CellEditor

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
    global.receiveExternalEvent(e)
      .updateState(WebappTestUtil.applyEventSuccessfully(_, e))

  def setFocus(f: ReqTableObs => html.Element): *.Actions =
    *.action("Set focus")(i => f(i.obs).focus()) +>
      global.activeElement.assert.equalBy(i => f(i.obs))

  def copyToClipboard(text: String): *.Actions =
    *.action(s"Copy to clipboard: ${text.quote}")(_ => TestClipboard.writeText(text))
}