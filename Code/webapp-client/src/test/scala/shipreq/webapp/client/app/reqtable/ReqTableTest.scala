package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.test._
import monocle.macros.Lenses
import nyaya.test.PropTest._
import shipreq.base.test.JsEnv
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.{PlainText, ProjectText, TextSearch}
import shipreq.webapp.base.UiText.ColumnNames
import shipreq.webapp.client.data._
import shipreq.webapp.client.feature.ContentEditorFeature.EditFieldKey
import shipreq.webapp.client.feature._
import shipreq.webapp.client.test._
import shipreq.webapp.client.widgets.high.ProjectWidgets
import utest._
import TestState.{scalazEqualFromTestState => _, _}
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

  def runTest(plan: *.Plan): Unit =
    runTest(plan, false)

  def runTest(plan: *.PlanWithInitialState): Unit =
    runTest(plan, false)

  def runTest(plan: *.Plan, testsFocus: Boolean): Unit =
    runTest(plan withInitialState SampleProject3.project, testsFocus)

  def runTest(plan: *.PlanWithInitialState, testsFocus: Boolean) = {
    val reqDetailRC = MockRouterCtl[ExternalPubid]()
    val cd = TestClientData(plan.initialState)
    val cp = MockServer(cd)
    import cd.pxProject

    val pxPlainText      = pxProject.map(PlainText(_, ProjectText.Context.None))
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

    ReactTestUtils.withRendered(outer(initialState), testsFocus) { c =>
      def observe() = new ReqTableObs(cp, c.htmlDomZipper)
      val ref       = Ref(c zoomL State.reqTable, cp)
      val test      = plan.addInvariants(invariants).test(Observer watch observe())
      val result    = test.run(ref)
      result.assert()
    }
  }

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
    *.genActionsBy("testDeadToggleInvariants")(i =>
      RandomReqTableData.viewSettings(i.state, allowFilter = true)
        .map(applyViewSettings(_) >> filterDeadShowHide))

  def testDeadNotEditable =
    Plan.action(
      showAllColumns(ShowDead) >> *.chooseAction("Edit all dead columns.") { i =>
        val cn = Column.NameResolver.byProject(i.state)
        Column.all(i.state.config, ShowDead)
          .iterator
          .filter(_.live :: Dead)
          .map { c =>
            val n = cn(c)
            val ce = cellEditor("MF-1", n)
            import ce._
            (tryStartEdit +> assertState(Normal)).group(s"Try to edit $n.")
          }
          .combine
      }
    )

  def testImplicationSrcColumnEditor = {
    val ce = cellEditor(pubid = "FR-1", col = "Implied By")
    import ce._
    // TODO What about an implication cycle with a dead link. Ok? Not ok? What about when when link is undeleted?
    Plan.action(
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

  def testImplicationTgtColumnEditor = {
    val ce = cellEditor(pubid = "MF-3", col = "Implies")
    import ce._
    Plan.action(
      showAllColumns +> cellText.assert("FR-4, MF-4")
        >> startEdit +> editorValue.assert("FR-4 MF-4")
        >> testInvalid("BR-1").suffix(" (Causes cycle)") // because BR-1 → BR-2 → FR-3 → BR-1
        >> testInvalid("BR-2").suffix(" (Causes cycle)") // because BR-1 → BR-2 → FR-3 → BR-2
        >> testValid("MF-3") // reflexivity is tolerated but should be ignored on save
        >> testValid("FR-4 MF-4")
        >> testValid("MF-4 FR-6")
        >> testValid("MF-2")
    ) withInitialState SampleImplicationGraph.project
  }

  def testCustomImplicationColumnEditor = {
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

    val ce = cellEditor(pubid = "MF-13", col = "Major Feature")
    import ce._

    def mfs(sep: String, mfs: Int*): String =
      mfs.sorted.map("MF-" + _) mkString sep

    Plan.action(
      showAllColumns +> cellText.assert(mfs(", ", 1, 5, 2, 6, 7, 8, 9, 10, 13))
        >> startEdit +> editorValue.assert(mfs(" ", 5, 6)) // Should only show direct & live
        >> testInvalid("MF-4").suffix(" (Dead target)")
        >> testInvalid("MF-8").suffix(" (Dead target)")
        >> testInvalid("MF-5 CO-5").suffix(" (Causes cycle)") // because MF-1 → MF-5 → MF-13 → CO-5 → MF-1
        >> testValid("MF-13") // reflexivity is tolerated but should be ignored on save
        >> testValid("MF-5 MF-6")
        >> testValid("MF-5 MF-6 MF-1")
        >> testValid("MF-3")
        >> testValid("MF-1")
    ) withInitialState p
  }

  def testTagsColumnEditor = {
    val p = GReq(reqType = co, title = reqTitleTagRefs(v11, v13, v4x)).tag(wip, uat, v11, v1x, v3x) !
      SampleProject.project

    val ce = cellEditor(pubid = "CO-1", col = "Tags")
    import ce._

    Plan.action(
      showAllColumns +> cellText.assert("v1.3 v1.x v3.x v4.x") // wip & uat in Status col
        >> startEdit +> editorValue.assert("v1.1 v1.x") // Should only show direct & live
        >> testInvalid("v0.9").suffix(" (Dead target)")
        >> testInvalid("v3.x").suffix(" (Dead target)")
        >> testInvalid("v4.x").suffix(" (Dead target)")
        >> testInvalid("wip").suffix(" (Status has its own column)")
        >> testValid("v1.3") // declared in text too = ok
        >> testValid("v1.x")
        >> testValid("v1.x v1.0")
        >> testValid("v1.1")
    ) withInitialState p
  }

  def testCustomTagColumnEditor = {
    val p = GReq(reqType = co, title = reqTitleTagRefs(prod, uat3)).tag(wip, uat, v1x, v3x) !
      SampleProject.project

    val ce = cellEditor(pubid = "CO-1", col = "Status")
    import ce._

    Plan.action(
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
        >> testValid("defer")
    ) withInitialState p
  }

  def testEditorTitleIO = {
    val ce = cellEditor(pubid = "MF-6", col = "Title")
    import ce._

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

    Plan.action(
      svrDisableAutoRespond >>
      editChangeCommit >> fail >> retry >> fail >> cancelSaveCommitAgain >> saveSucceeds)
  }

  val nopMod = ("No change.", (s: String) => s)

  def testNopEdits(pubid: String, col: String): *.Plan =
    testNopEditsBy(pubid, col)("Trailing whitespace." -> (_ + " "))

  def testNopEditsBy(pubid: String, col: String)(mods: (String, String => String)*): *.Plan = {
    val ce = cellEditor(pubid, col)
    import ce._

    val post = svrReqs.assert.noChange & assertState(Normal).after

    val commitNop = commit +> post

    val nopEdit = (x: (String, String => String)) =>
      (startEdit >> modifyValue(x._2) >> commitNop) group x._1

    Plan.action(
      showAllColumns
        >> (startEdit >> commitNop).group("Commit without edit.")
        >> (nopMod +: mods).map(nopEdit).combine
        >> (startEdit >> abortEdit +> post).group("Abort.")
    ) named s"NOP edits: $pubid/$col"
  }

  def testKeyboardNavigation = Plan.action(
    setFocus(_.table.cell(1, 1).dom)
      >> press(DownKey)   +> activeElement.assert.equalBy(_.obs.table.cell(2, 1).dom)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.cell(2, 2).dom)
      >> press(UpKey)     +> activeElement.assert.equalBy(_.obs.table.cell(1, 2).dom)
      >> press(LeftKey)   +> activeElement.assert.equalBy(_.obs.table.cell(1, 1).dom)
      >> press(End)       +> activeElement.assert.equalBy(_.obs.table.cell(1, -1).dom)
      >> press(Home)      +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(1))
      >> press(LeftKey)   +> activeElement.assert.equalBy(_.obs.table.cell(1, -1).dom)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(1))
      >> press(DownKey)   +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(2))
      >> press(End.ctrl)  +> activeElement.assert.equalBy(_.obs.table.cell(-1, -1).dom)
      >> press(Home.ctrl) +> activeElement.assert.equalBy(_.obs.table.allRowSelectionInput)
      >> press(LeftKey)   +> activeElement.assert.equalBy(_.obs.table.columnDoms.last.headerCell)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.allRowSelectionInput)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.columnDoms(1).headerCell)
      >> press(DownKey)   +> activeElement.assert.equalBy(_.obs.table.cell(0, 1).dom)
      >> press(UpKey)     +> activeElement.assert.equalBy(_.obs.table.columnDoms(1).headerCell)
      >> press(UpKey)     +> activeElement.assert.equalBy(_.obs.table.cell(-1, 1).dom)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).dom)
      >> press(RightKey)  +> activeElement.assert.equalBy(_.obs.table.cell(-1, 3).dom) // The Title column
      >> press(F2)        +> activeElement.assert.equalBy(_.obs.table.cell(-1, 3)("textarea").dom)
      // TODO Test jumping from textarea ↔ table
  ) named "Keyboard navigation"

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = TestSuite {
    'initialState - runTest(*.emptyPlan)

    'filter       - runTest(Plan action testFilter named "testFilter")

    'dead {
      'cols        - runTest(Plan action testDeadColumns named "testDeadColumns")
      // 'toggle      - runTest(testDeadToggleInvariants) TODO Should dead col stay on but hidden when ShowDead→HideDead?
      'notEditable - runTest(testDeadNotEditable named "testDeadNotEditable")
    }

    'editor {
      'impSrc  - runTest(testImplicationSrcColumnEditor    named "testImplicationSrcColumnEditor"   )
      'impTgt  - runTest(testImplicationTgtColumnEditor    named "testImplicationTgtColumnEditor"   )
      'impCol  - runTest(testCustomImplicationColumnEditor named "testCustomImplicationColumnEditor")
      'tags    - runTest(testTagsColumnEditor              named "testTagsColumnEditor"             )
      'tagCol  - runTest(testCustomTagColumnEditor         named "testCustomTagColumnEditor"        )
      'titleIO - runTest(testEditorTitleIO                 named "testEditorTitleIO"                )

      'nop {
        // RCG title
        // RCG code
        'title    - runTest(testNopEdits("MF-6", ColumnNames.title))
        'textCol  - runTest(testNopEdits("MF-1", "Description"))
        'impSrc   - runTest(testNopEdits("MF-1", ColumnNames.implicationSrc))
        'impTgt   - runTest(testNopEdits("MF-1", ColumnNames.implicationTgt))
        'impCol   - runTest(testNopEdits("MF-1", "Major Feature"))
        'tags     - runTest(testNopEdits("MF-1", ColumnNames.tags))
        'tagCol   - runTest(testNopEdits("MF-1", "Status"))
        'reqCodes - runTest(testNopEditsBy("MF-1", ColumnNames.code)("Trailing \\n." -> (_ + "\n")))
      }
    }

    'kbNav - runTest(testKeyboardNavigation, true)
  }
}
