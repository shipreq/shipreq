package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import nyaya.test.PropTest._
import shipreq.base.test.JsEnv
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
import utest._
import TestState._
import SampleProject.Values._

object ReqTableTest extends TestSuite {
  import ReqTableTestDsl._

  PrepareEnv()

  val remotes = MockRemotes.projectSPA

  @Lenses
  case class State(editStates  : ContentEditorFeature.D2.State.Simple[Row.SourceId, EditFieldKey],
                   asyncStates : AsyncActionFeature.D2.State.Simple[Row.SourceId, EditFieldKey, String],
                   previewState: PreviewFeature.State[FocusId],
                   reqTable    : ReqTable.State)

  def defaultProject = SampleProject3.project

  def runTest(action: *.Action, project: Project = defaultProject): Unit = {
    val reqDetailRC = MockRouterCtl[ExternalPubid]()
    val cd = TestClientData(project)
    val cp = MockServer(cd)
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
      val ref = Ref(c zoomL State.reqTable, cp)
      def newObs = new ReqTableObs(cp, DomZipper(c))
      val t = Test(action, invariants).observe(_ => newObs)
      val r = t.run(initialState.reqTable.project, ref)
      r.assert()
    }
  }

//  def runTestInBrowser(action: *.Action, project: Project = defaultProject): Unit =
//    JsEnv.realBrowserMTest(runTest(action, project))

  // TODO Move
  import nyaya.gen._
  import nyaya.test.Settings
  import teststate.data.ROS
  def zxc[F[_], R, O, S, E](g: Gen[Actions[F, R, O, S, E]])(name: NameFn[ROS[R, O, S]])(implicit s: Settings): Actions[F, R, O, S, E] =
      g.samples(GenCtx(s.genSize))
      .take(s.sampleSize.value)
      .map(_.group("TODO .groupIfNotGrouped"))
      .zipWithIndex
      .map(x => x._1.nameMod(n => s"[${x._2 + 1}/${s.sampleSize.value}] ${n.value}"))
      .foldLeft[Actions[F, R, O, S, E]](emptyAction)(_ >> _)
      .group(name)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  import ProjectDsl._, UnsafeTypes._

  def testFilter = (
    sortByPubid
      >> enterFilter("-MF")
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> filterDeadToggle
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "CO-1", "CO-2")
      >> enterFilter("FR")
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> filterDeadToggle
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
  )

  def testDeadColumns = (
    filterDeadToggle
      +> filterDead.assert(ShowDead)
      +> selectableColumns.size.assert.increaseBy(3) // DeletionReasons + Reporter + Released
      >> filterDeadToggle
    ) +> selectableColumns.size.assert.noChange

  def testDeadToggleInvariants =
    *.chooseAction("testDeadToggleInvariants", i =>
      zxc(
        RandomReqTableData.viewSettings(i.state, allowFilter = true)
          .map(applyViewSettings(_) >> filterDeadShowHide)
      )("testDeadToggleInvariants"))

//  def testDeadRowsNotEditable(): Unit = {
//    val colCount = *.availCols.length
//
//    def focus(rowType: Live, colIndex: Int) =
//      Action(s"focus($rowType, $colIndex)", { s =>
//        val row = rowType match {
//          case Live => DomZipper.first("Live row", s.table.liveRows)
//          case Dead => DomZipper.first("Dead row", s.table.deadRows)
//        }
//        val cell = row.getAll(">td")(colIndex)
//        Simulate.click(cell)
//      })
//
//    def editAllColumns(rowType: Live): Action[Int] = {
//      val editEachCell =
//        (0 until colCount).map { c =>
//          focus(rowType, c).focus(_.table.focus).assertChange >> editFocused
//        }.reduce(_ >> _)
//
//      (showAllColumns >> editEachCell).focus(_.table.inputsInFocusRow getOrElse 0)
//    }
//
//    editAllColumns(Dead).assertAfter(0).run()
//
//    // Ensure test logic works
//    reset()
//    editAllColumns(Live).testAfter(_ > 0, "[Live Row] Cells should be in edit-mode").run()
//  }

  def testImplicationSrcColumnEditor() = {
    val ce = CellEditor(_.table.cellLoc(pubid = "FR-1", col = "Implied By"))
    import ce._
    // TODO What about an implication cycle with a dead link. Ok? Not ok? What about when when link is undeleted?
    runTest(
      showBuiltInColumnsSortedByPubid +> cellText.assert("MF-12, MF-19")
        >> startEdit +> editorValue.assert("MF-12") // Should remove dead
        >> testInvalid("MF-28").suffix(" (Dead target)")
        >> testInvalid("MF-19").suffix(" (Dead target)")
        >> testInvalid("MF-27").suffix(" (Causes cycle)") // because FR-1 → FR-2 → MF-27
        >> testValid("FR-1") // reflexivity is tolerated but should be ignored on save
        >> testValid("MF-12")
        >> testValid("MF-14")
        >> testValid("MF-12 MF-14"))
  }

  def testImplicationTgtColumnEditor() = {
    val ce = CellEditor(_.table.cellLoc(pubid = "MF-3", col = "Implies"))
    import ce._
    runTest(
      showAllColumns +> cellText.assert("FR-4, MF-4")
        >> startEdit +> editorValue.assert("FR-4 MF-4")
        >> testInvalid("BR-1").suffix(" (Causes cycle)") // because BR-1 → BR-2 → FR-3 → BR-1
        >> testInvalid("BR-2").suffix(" (Causes cycle)") // because BR-1 → BR-2 → FR-3 → BR-2
        >> testValid("MF-3") // reflexivity is tolerated but should be ignored on save
        >> testValid("FR-4 MF-4")
        >> testValid("MF-4 FR-6")
        >> testValid("MF-2"),
      SampleImplicationGraph.project)
  }

  def testCustomImplicationColumnEditor() = {
    val p = (
      // MF- 1ᵒ → MF-5ᵒ → MF-13
      // MF- 2ˣ → MF-6ᵒ → MF-13 <-- difficult case - it should be displayed as its part of (a chain with ShowDead)
      // MF- 3ᵒ → MF-7ˣ → MF-13 <-- important case - shouldn't hold for FR-1 even in ShowDead
      // MF- 4ˣ → MF-8ˣ → MF-13
      // MF- 9ᵒ → CO-1ᵒ → MF-13
      // MF-10ˣ → CO-2ᵒ → MF-13 <-- difficult case - it should be displayed as its part of (a chain with ShowDead)
      // MF-11ᵒ → CO-3ˣ → MF-13 <-- important case - shouldn't hold for FR-1 even in ShowDead
      // MF-12ˣ → CO-4ˣ → MF-13
      //          CO-5ᵒ → MF-1↖
      GReq(reqType = mf, id = 1) +
      GReq(reqType = mf, id = 2, live = Dead) +
      GReq(reqType = mf, id = 3) +
      GReq(reqType = mf, id = 4, live = Dead) +
      GReq(reqType = mf, id = 5).impSrc(1) +
      GReq(reqType = mf, id = 6).impSrc(2) +
      GReq(reqType = mf, id = 7, live = Dead).impSrc(3) +
      GReq(reqType = mf, id = 8, live = Dead).impSrc(4) +
      GReq(reqType = mf, id = 9) +
      GReq(reqType = mf, id = 10, live = Dead) +
      GReq(reqType = mf, id = 11) +
      GReq(reqType = mf, id = 12, live = Dead) +
      GReq(reqType = co, id = 51).impSrc(9) +
      GReq(reqType = co, id = 52).impSrc(10) +
      GReq(reqType = co, id = 53, live = Dead).impSrc(11) +
      GReq(reqType = co, id = 54, live = Dead).impSrc(12) +
      GReq(reqType = co, id = 55).impTgt(1) +
      GReq(reqType = mf, id = 13).impSrc(5, 6, 7, 8, 51, 52, 53, 54)
      ) ! SampleProject.project

    val ce = CellEditor(_.table.cellLoc(pubid = "MF-13", col = "Major Feature"))
    import ce._

    def mfs(sep: String, mfs: Int*): String =
      mfs.sorted.map("MF-" + _) mkString sep

    runTest(
      showAllColumns +> cellText.assert(mfs(", ", 1, 5, 2, 6, 7, 8, 9, 10, 13))
        >> startEdit +> editorValue.assert(mfs(" ", 5, 6)) // Should only show direct & live
        >> testInvalid("MF-4").suffix(" (Dead target)")
        >> testInvalid("MF-8").suffix(" (Dead target)")
        >> testInvalid("MF-5 CO-5").suffix(" (Causes cycle)") // because MF-1 → MF-5 → MF-13 → CO-5 → MF-1
        >> testValid("MF-13") // reflexivity is tolerated but should be ignored on save
        >> testValid("MF-5 MF-6")
        >> testValid("MF-5 MF-6 MF-1")
        >> testValid("MF-3")
        >> testValid("MF-1"),
      p)
  }

  def testTagsColumnEditor() = {
    val p = GReq(reqType = co, title = reqTitleTagRefs(v11, v13, v4x)).tag(wip, uat, v11, v1x, v3x) !
      SampleProject.project

    val ce = CellEditor(_.table.cellLoc(pubid = "CO-1", col = "Tags"))
    import ce._

    runTest(
      showAllColumns +> cellText.assert("v1.3 v1.x v3.x v4.x") // wip & uat in Status col
        >> startEdit +> editorValue.assert("v1.1 v1.x") // Should only show direct & live
        >> testInvalid("v0.9").suffix(" (Dead target)")
        >> testInvalid("v3.x").suffix(" (Dead target)")
        >> testInvalid("v4.x").suffix(" (Dead target)")
        >> testInvalid("wip").suffix(" (Status has its own column)")
        >> testValid("v1.3") // declared in text too = ok
        >> testValid("v1.x")
        >> testValid("v1.x v1.0")
        >> testValid("v1.1"),
      p)
  }

  def testCustomTagColumnEditor() = {
    val p = GReq(reqType = co, title = reqTitleTagRefs(prod, uat3)).tag(wip, uat, v1x, v3x) !
      SampleProject.project

    val ce = CellEditor(_.table.cellLoc(pubid = "CO-1", col = "Status"))
    import ce._

    runTest(
      showAllColumns +> cellText.assert("wip uat uat3 prod")
        >> startEdit +> editorValue.assert("wip") // Should only show direct & live
        >> testInvalid("uat").suffix(" (Dead target)")
        >> testInvalid("uat2").suffix(" (Dead target)")
        >> testInvalid("uat3").suffix(" (Dead target)")
        >> testInvalid("v1.0").suffix(" (Not a status)")
        >> testInvalid("v3.x").suffix(" (Not a status)")
        >> testValid("prod") // declared in text too = ok
        >> testValid("wip")
        >> testValid("wip defer")
        >> testValid("defer"),
      p)
  }

  def testEditIO(): Unit = {

    val ce = CellEditor(_.table.cellLoc(pubid = "MF-6", col = "Title"))
    import ce._

    val editCommitWithoutChange = (
      startEdit
        +> cellText.assert("Incompletions")
        >> commit
        +> svrReqs.assert.noChange
      ) group "editCommitWithoutChange"

    val newValue = "issues!"

    val editChangeCommit = (
      startEdit
        +> cellText.assert("Incompletions")
        >> enterValue(newValue)
        >> commit
        +> svrReqs.assert.increment
        +> assertState(Locked)
        >> assertCantStartEdit
      ) group "editChangeCommit"

    val fail = (
      svrFailLast
        +> assertState(Failed) // Should be in failed state after I/O failure
        >> assertCantStartEdit)

    val retry = (
      clickRetry
        +> assertState(Locked)
        +> svrReqs.assert.increment
        +> svrAssertLastTwoReqsEqual
      )

    val cancelSaveCommitAgain = (
      clickAbort
        +> assertState(Editing)
        +> editorValue.assert(newValue)
        >> commit
        +> assertState(Locked)
        +> svrReqs.assert.increment
        +> svrAssertLastTwoReqsEqual
      ) group "cancelSaveCommitAgain"

    val saveSucceeds =
      svrAutoRespondToLast +> assertState(Normal)

    runTest(svrDisableAutoRespond >>
//      editCommitWithoutChange >> // TODO Test failing due to real bug. Fix!
      editChangeCommit >> fail >> retry >> fail >> cancelSaveCommitAgain >> saveSucceeds)
  }


  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = TestSuite {
    'initialState - runTest(emptyAction)

    'filter       - runTest(testFilter)

    'dead {
      'cols        - runTest(testDeadColumns)
      // 'toggle      - runTest(testDeadToggleInvariants) TODO Should dead col stay on but hidden when ShowDead→HideDead?
      // 'notEditable - testDeadRowsNotEditable()
    }

    'editor {
      'impSrc       - testImplicationSrcColumnEditor()
      'impTgt       - testImplicationTgtColumnEditor()
      'customImpCol - testCustomImplicationColumnEditor()
      'tags         - testTagsColumnEditor()
      'customTagCol - testCustomTagColumnEditor()
      'io           - testEditIO()
    }

//    'real - realBrowserMTest(runTest(emptyAction))

  }
}
