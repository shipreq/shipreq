package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.util.UnivEq.{apply => _, force => _}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.Style
import shipreq.webapp.client.data._
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import TestState._

object ReqTableTestDsl {
  val * = Dsl.sync[CompState.AccessD[ReqTable.State], ReqTableObs, Project, String]

  def apply(action: *.Action = emptyAction): *.TestContent =
    Test(action, invariants)

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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val editorInvalidSel: String =
    "." + Style.reqtable.cellEditor(Invalid).className.value +
    ",." + Style.reqtable.cellEditorErrMsg.className.value

  final case class CellEditor(loc: ReqTableObs => ReqTableObs.CellLoc) {

    private implicit def ROStoOS(r: *.ROS) = r.os

//    def cellText = *.focus("Cell innerText").value(cell(_).innerText)

    private def editorCss       = DomZipper.EditableSel
    private def retryButtonCss  = "button:contains(Retry)"
    private def failOkButtonCss = "button:contains(OK)"

    val cell        = *.focus("Subject cell").value(s => s.obs.table.cell(loc(s.obs)))
    val cellText    = cell.map(_.innerText) rename "Cell innerText"
    val editing     = cell.map(_ exists editorCss) rename "Editing"
//    val locked       = cell(s).collectInnerHTML("img").nonEmpty
//    val failed       = cell(s).collectInnerHTML(retryButtonCss).nonEmpty
    val editor      = cell.map(_.down(editorCss).domAs[html.Input])
//    val editorClass = editor.map(_.className) rename "Editor class"
    val editorValue = editor.map(_.value) rename "Editor value"
//    val retryButton  = cell(s)(retryButtonCss).as[html.Button]
//    val failOkButton = cell(s)(failOkButtonCss).as[html.Button]

    val editorValidity = *.focus("Editor validity").value(Invalid <~ cell.run(_).exists(editorInvalidSel))
//
//    implicit class CEActionExt[A](a: Action[A]) {
//      def assertNowEditing      = a.focus(editing_?).assertBefore(false).assertAfter(true)
//      def assertNowLocked       = a.focus(locked_?) .assertBefore(false).assertAfter(true)
//      def assertNoLongerEditing = a.focus(editing_?).assertBefore(true).assertAfter(false)
//      def assertNoLongerLocked  = a.focus(locked_?) .assertBefore(true).assertAfter(false)
//
//      def assertNoCellState = a
//        .focus(editing_?).assertAfter(false)
//        .focus(locked_?).assertAfter(false)
//        .focus(failed_?).assertAfter(false)
//    }
//
//    def printCell(): Unit =
//      println(cell(*).outerHTML)

//    def setup(p: Project) =
//      (setProject(p) >> showAllColumns).group("Setup")

    val tryStartEdit =
      *.action("Start editor").act(Simulate doubleClick cell.run(_).dom)

    val startEdit =
      tryStartEdit +> editing.assert(true)

//    val assertEditDoesNothing =
//      tryStartEdit.focus(editing_?).assertAfter (false)
//
  def enterValue(text: String, desc: String = "Enter value") =
    *.action(s"$desc: ${text.show}").act(ChangeEventData(text) simulate editor.run(_)) +>
      editorValue.assert(text)

    def testValid  (text: String) = enterValue(text, "Enter valid value")   +> editorValidity.assert(Valid)
    def testInvalid(text: String) = enterValue(text, "Enter invalid value") +> editorValidity.assert(Invalid)

//    val commit =
//      Action.exec2("commit", editor)(ctrlEnter simulateKeyDown _).assertNoLongerEditing
//
//    val clickRetry =
//      Action.exec2("clickRetry", retryButton)(Simulate click _)
//
//    val clickFailOk =
//      Action.exec2("clickFailOk", failOkButton)(Simulate click _)
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
        **.assert.containsAll("live custom field columns", i => customFieldNames(i.state, Live))

      val deadColumns =
        **.assert.existenceOfAllBy("dead custom field columns", i => customFieldNames(i.state, Dead))(
          _.obs.filterDead :: ShowDead)

      uniqueColumns & liveCustomFieldColumnsAlwaysAvailable & deadColumns
    }

    def sortColumns = {
      val names = *.focus("Sort criteria").collection(_.obs.sorting.names)
      names.assert.distinct &
      names.assert.containsOnly("visible columns", i => visibleColumns(i.obs))
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
        *.point("Req formula.", os => {
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
    *.action(name).act(_.ref.modState(s => s.copy(viewSettings = f(s))))

  def setProject(p: Project): *.Action =
    *.action("Set project.").act(_.ref.modState(_.updateProject(p))).updateState(_ => p)

  val showAllColumns = applyViewSettings("Show all columns.", s => {
    val fd = ShowDead
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
    *.action("Show/hide " + columnName)
      .act(Simulation.change run _.obs.viewSettings.columns.column(columnName).checkbox)

  def sortBy(columnName: String): *.Action =
    *.action("Sort by " + columnName)
      .act(Simulation.click run _.obs.table.column(columnName).headerCell)

  val sortByPubid =
    sortBy(UiText.ColumnNames.pubid)

  def enterFilter(f: String) = {
    val e = ChangeEventData(f)
    *.action(s"enterFilter('$f')").act(e simulate _.obs.viewSettings.filter.input)
      .addCheck(*.focus("Filter").value(_.obs.viewSettings.filter.input.value).assert(f).after)
  }

  val filterDeadToggle =
    *.action("filterDeadToggle").act(Simulate change _.obs.viewSettings.filterDead.checkbox)
      .addCheck(filterDead.assert.change)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd).rename(s"setFilterDead($fd)")

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.viewSettings.columns.onColumns).assert.noChange)

  val logTable = *.print(_.obs.table.entireContent)
}
