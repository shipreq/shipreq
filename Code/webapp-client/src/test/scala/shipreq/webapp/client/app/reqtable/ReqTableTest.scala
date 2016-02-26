package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import shipreq.base.util.UnivEq.{apply => _, force => _}
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

object ReqTableTest extends TestSuite {
  import ReqTableTestDsl._

  PrepareEnv()

  val remotes = MockRemotes.projectSPA

  @Lenses
  case class State(editStates  : ContentEditorFeature.D2.State.Simple[Row.SourceId, EditFieldKey],
                   asyncStates : AsyncActionFeature.D2.State.Simple[Row.SourceId, EditFieldKey, String],
                   previewState: PreviewFeature.State[FocusId],
                   reqTable    : ReqTable.State)

  def runTest(action: *.Action): Unit = {
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
