package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test.ReactTestUtils.Simulate
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import shipreq.base.util.UnivEq.{apply => _, force => _}
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.data._
import shipreq.webapp.client.feature.ContentEditorFeature.EditFieldKey
import shipreq.webapp.client.feature._
import shipreq.webapp.client.test.DomZipper.Implicits._
import shipreq.webapp.client.test._
import shipreq.webapp.client.widgets.high.ProjectWidgets
import teststate._
import utest._

// =====================================================================================================================

object ReqTableTestDsl {
  val * = Dsl.sync[CompState.AccessD[ReqTable.State], ReqTableObs, Project, String]

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
        **.assert.existenceOfAll("dead custom field columns",
          _.obs.filterDead :: ShowDead,
          i => customFieldNames(i.state, Dead))

      uniqueColumns & liveCustomFieldColumnsAlwaysAvailable & deadColumns
    }

    def sortColumns = {
      val names = *.focus("Sort criteria").collection(_.obs.sorting.names)
      names.assert.distinct &
      names.assert.containsOnly("visible columns", i => visibleColumns(i.obs))
    }

    def tableColumns =
      *.focus("Table columns").collection(_.obs.table.fieldColumns)
        .assert.equalIgnoringOrder(i => visibleColumns(i.obs))

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

  implicit def equalFromUnivEq[A: UnivEq] = teststate.Equal.byUnivEq[A]
  implicit def autoGetDomFromZipper(d: DomZipper.Temp): ReactOrDomNode = d.dom.domAsHtml
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

  val filterDeadToggle =
    *.action("filterDeadToggle").act(Simulate change _.obs.viewSettings.filterDead.checkbox)
      .addCheck(*.focus("FilterDead").value(_.obs.filterDead).assert.changeOccurs)

  def setFilterDead(fd: FilterDead) =
    filterDeadToggle.unless(_.obs.filterDead == fd).rename(s"setFilterDead($fd)")

  val filterDeadShowHide =
    setFilterDead(HideDead) >>
    filterDeadToggle.times(2).addCheck(
      *.focus("On-columns").value(_.obs.viewSettings.columns.onColumns).assert.not.changeOccurs)

  val tablePubids = *.focus("Visible pubids").collection(_.obs.table.rowPubids)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ReqTableTest extends TestSuite {
  import ReqTableTestDsl._

  PrepareEnv()

  val remotes = MockRemotes.projectSPA

  @Lenses
  case class State(editStates  : ContentEditorFeature.D2.State.Simple[Row.SourceId, EditFieldKey],
                   asyncStates : AsyncActionFeature.D2.State.Simple[Row.SourceId, EditFieldKey, String],
                   previewState: PreviewFeature.State[FocusId],
                   reqTable    : ReqTable.State)

  def runTest(action: *.Action) = {
    val reqDetailRC = MockRouterCtl[ExternalPubid]()
    val cp = new TestClientProtocol
    val cd = TestClientData(SampleProject3.project)
    import cd.pxProject

    val pxPlainText      = pxProject map PlainText.apply
    val pxTextSearch     = Px.apply2(pxProject, pxPlainText)(TextSearch.apply)
    val pxProjectWidgets = Px.apply2(pxProject, pxPlainText)(ProjectWidgets(_, _, reqDetailRC))

    val outer = WithExternalCompStateAccess.init { ($: CompState.Access[State], s: State) =>

      val asyncFeature: AsyncActionFeature.D2.Feature[Row.SourceId, EditFieldKey, String] =
        AsyncActionFeature.D2.Feature($ zoomL State.asyncStates)

      val previewFeature = new PreviewFeature($, State.previewState)

      def initReqTableEditor: ReqTable.InitEditor = {
        import ContentEditorFeature._
        new D2.InitChild[Row, Column, FocusId] {
          override type Parent    = State
          override val parent     = $: CompState.Access[Parent]
          override val preview    = previewFeature
          override val editorLens =
            (r: Row, c: Column) =>
              Column.EditFieldKeyIntersection.getOption(c).map(efk =>
                State.editStates ^|-> D2.State.at(r.sourceId) ^|-> D1.State.at(efk))
        }
      }

      ReqTable(ReqTable.StaticProps(
        cd, cp, remotes.createContent, remotes.updateContent,
        pxPlainText, pxTextSearch, pxProjectWidgets,
        initReqTableEditor,
        asyncFeature.mapK1(Column.EditFieldKeyIntersection.reverse),
        reqDetailRC,
        $ zoomL State.reqTable))

    }((reqTable, $, s) =>
      reqTable(ReqTable.DynamicProps(
        s.editStates.mapK1(Column.EditFieldKeyIntersection.reverse),
        s.asyncStates.mapK1(Column.EditFieldKeyIntersection.reverse),
        s.previewState,
        s.reqTable))
    )

    def initialState = State(
      ContentEditorFeature.D2.State.init,
      AsyncActionFeature.D2.State.init,
      PreviewFeature.initState,
      ReqTable.State.init(cd, HideDead, None))

    ReactTestUtils.withRenderedIntoDocument(outer(initialState)) { c =>
      def newObs = new ReqTableObs(DomZipper(c))
      val tt = Test(action, invariants).observe(_ => newObs)
      val h =  tt.run(initialState.reqTable.project, c.zoomL(State.reqTable))
//      println(h.format(History.Options.colored.alwaysShowChildren))
//      println(h.format(History.Options.colored))
      h.assert(History.Options.colored)
    }
  }

  override def tests = TestSuite {
    'initialState - runTest(Action.empty)
    'filter       - runTest(testFilter)
  }

  def testFilter = (
    sortByPubid
      >> enterFilter("-MF")
      >> filterDeadToggle
        .addCheck(tablePubids.assert.equalIgnoringOrder(_ => List("FR-1", "FR-2")).before)
        .addCheck(tablePubids.assert.equalIgnoringOrder(_ => List("FR-1", "FR-2", "CO-1", "CO-2")).after)
      >> enterFilter("FR")
      >> filterDeadToggle
        .addCheck(tablePubids.assert.equalIgnoringOrder(_ => List("FR-1", "FR-2")).beforeAndAfter)
  )
}
