package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import japgolly.scalajs.react.test._
import shipreq.base.util.UnivEq.{apply => _, force => _}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.data._
import shipreq.webapp.client.test._
import teststate.Exports._

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

  val mandatoryColumns = FilterDead.memo(fd =>
    Column.mandatory.iterator
      .filter(fd.filterFnA(_.live))
      .map(Column.NameResolver.builtIn)
      .toSet)

  def visibleColumns(obs: ReqTableObs): Set[String] =
    mandatoryColumns(obs.filterDead) ++ obs.viewSettings.columns.onColumns

  val invariants = {

    def selectableColumns = {
      val ** = *.focus("Selectable columns").collection(_.obs.selectableCols)

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

  // ===================================================================================================================

  implicit def equalFromUnivEq[A: UnivEq] = Equal.by_==[A]
  implicit def autoGetDomFromZipper(d: DomZipper): ReactOrDomNode = d.dom.domAsHtml
  implicit val showFilterDead = Show.byToString[FilterDead]

  def applyViewSettings(name: => String, f: ViewSettings => ViewSettings): *.Action =
    *.action(name).act(_.ref.modState(ReqTable.State.viewSettings modify f))

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
      .addCheck(*.focus("Filter").value(_.obs.viewSettings.filter.input.value).assert.equal(f).after)
  }

  val filterDead =
    *.focus("FilterDead").value(_.obs.filterDead)

  val filterDeadToggle =
    *.action("filterDeadToggle").act(Simulate change _.obs.viewSettings.filterDead.checkbox)
      .addCheck(filterDead.assert.changeOccurs)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd).rename(s"setFilterDead($fd)")

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.viewSettings.columns.onColumns).assert.not.changeOccurs)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)
}
