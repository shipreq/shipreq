package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.test.SimEvent.{Keyboard => KB}
import nyaya.test.PropTest._
import shipreq.base.util.{Invalid, Valid}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview._
import shipreq.webapp.member.project.event.Event
import shipreq.webapp.member.project.filter.Filter
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.SampleProject.Values._
import shipreq.webapp.member.test.project.{RandomData, SampleImplicationGraph, SampleProject, SampleProject3, SampleProject4, SampleProject7}
import utest._
import utest.framework.TestPath

object ReqTableTest extends TestSuite {
  import ReqTableTestDsl.{savedViews => _, _}
  import ReqTableTestDsl.savedViews.{* => _, _}
  import global.{activeElement, press}

  PrepareEnv()

  def runTest(plan: *.Plan, project: Project = SampleProject4.project)(implicit path: TestPath): Unit =
    runTest(plan withInitialState project)

  def runTest(p: *.PlanWithInitialState)(implicit path: TestPath): Unit = {
    import ProjectSpaTestDsl._
    ProjectSpaTestDsl.runTest(
      liftReqTableTests(p.plan).asAction(path.value.mkString("ReqTableTest.", ".", "")),
      page = Page.ReqTable,
      project = p.initialState)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  import shipreq.webapp.member.test.project.ProjectDsl._
  import shipreq.webapp.member.test.project.UnsafeTypes._

  def testFilterTotal = Plan.action(
    sortByPubid
      >> enterFilter("-MF")
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> filterDeadToggle
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "CO-1", "CO-2")
      >> enterFilter("FR")
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> filterDeadToggle
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
  ).named("testFilterTotal").withInitialState(SampleProject3.project)

  def testFilterPartial = Plan.action(
    enterFilter("FR")             +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> enterFilter("FR | ")     +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2")
      >> enterFilter("FR | MF-3") +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "MF-3")
      >> saveCurrentView("x")
      >> enterFilter("FR | MF-|") +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "MF-3")
      >> enterFilter("FR | MF-4") +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "MF-4")
      >> selectView("x")          +> tablePubids.assert.equalIgnoringOrder("FR-1", "FR-2", "MF-3")
  ).named("testFilterPartial").withInitialState(SampleProject3.project)

  def testDeadColumns = (
    filterDeadToggle
      +> filterDead.assert(ShowDead)
      +> selectableColumns.size.assert.increaseBy(3) // DeletionReasons + Reporter + Released
      >> filterDeadToggle
    ) +> selectableColumns.size.assert.noChange

  def testDeadToggleInvariants =
    *.genActionsBy("testDeadToggleInvariants")(i =>
        for {
          fd <- RandomData.filterDead
          ts <- RandomSavedView.view(i.state, fd, allowFilter = true)
        } yield setViewSettings("Apply random view settings", fd, (_, _) => ts) >> filterDeadShowHide)

  def testDeadNotEditable =
    Plan.action(
      showAllColumns(ShowDead) >> *.chooseAction("Edit all dead columns.") { i =>
        ColumnPlus.All(i.state, ShowDead)
          .columns
          .iterator
          .filter(_.live is Dead)
          .map { c =>
            val n = c.name
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
      showBuiltInColumnsSortedByPubid +> text.assert("MF-12, MF-19")
        >> startEdit +> editorValue.assert("MF-12") // Should remove dead
        >> testInvalid("MF-28").suffix(" (Dead target)")
        >> testInvalid("MF-19").suffix(" (Dead target)")
        >> testInvalid("MF-27").suffix(" (Causes cycle)") // because FR-1 → FR-2 → MF-27
        >> testValid("FR-1") // reflexivity is tolerated but should be ignored on save
        >> testValid("MF-12")
        >> testValid("MF-14")
        >> testValid("MF-12 MF-14")
        >> testValid("12 14")
        >> testInvalid("2") // ambiguous
    )
  }

  def testImplicationTgtColumnEditor = {
    val ce = cellEditor(pubid = "MF-3", col = "Implies")
    import ce._
    Plan.action(
      showAllColumns +> text.assert("FR-4, MF-4")
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
      showAllColumns +> text.assert(mfs(", ", 1, 5, 2, 6, 7, 8, 9, 10, 13))
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

  def testOtherTagsColumnEditor = {
    val p = GReq(reqType = co, title = reqTitleTagRefs(v11, v13, v4x).whole).tag(wip, uat, v11, v1x, v3x) !
      SampleProject.projectWithOtherTags

    val ce = cellEditor(pubid = "CO-1", col = StaticField.OtherTags.name)
    import ce._

    Plan.action(
      showAllColumns +> text.assert("v1.3#v1.xv3.xv4.x#") // wip & uat in Status col
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
    val p = GReq(reqType = co, title = reqTitleTagRefs(prod, uat3).whole).tag(wip, uat, v1x, v3x) !
      SampleProject.project

    val ce = cellEditor(pubid = "CO-1", col = "Status")
    import ce._

    Plan.action(
      showAllColumns +> text.assert("wipuatuat3#prod#")
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
        >> commit
        +> text.assert("deferuatuat3#prod#") // dead #uat preserved
    ) withInitialState p
  }

  def testEditorTitleIO = {
    val ce = cellEditor(pubid = "MF-6", col = "Title")
    import ce._

    val newValue = "issues!"

    val editChangeCommit = (
      startEdit
        +> text.map(TestUtil.removeEditInstructionText).assert("Incompletions")
        >> setEditorValue(newValue)
        >> commit
        +> global.requestCount.assert.increment
        +> assertState(Locked)
        >> assertCantStartEdit
      ) group "editChangeCommit"

    val fail =
      global.failLastRequest +> assertState(Failed) // Should be in failed state after I/O failure

    val retry = (
      commit
        +> assertState(Locked)
        +> global.requestCount.assert.increment
        +> global.assertLastTwoRequestsAreEqual
      )

    val saveSucceeds =
      global.autoRespondToLast +> assertState(Normal)

    Plan.action(
      global.disableAutoResponse >>
      editChangeCommit >> fail >> retry >> fail >> retry >> saveSucceeds)
  }

  def testFailureClearedOnEsc = {
    val ce = cellEditor(pubid = "MF-6", col = "Title")
    import ce._
    val origValue = "Incompletions"
    Plan.action(
      global.disableAutoResponse
        >> startEdit
        +> editorValue.assert(origValue)
        >> setEditorValue("boop")
        >> commit +> global.requestCount.assert.increment
        >> global.failLastRequest +> assertState(Failed)
        >> abort
        >> startEdit // This is the key point of the test - it asserts the previous server failure is cleared
        +> editorValue.assert(origValue)
    )
  }

  val nopMod = ("No change.", (s: String) => s)

  def testNopEdits(pubid: String, col: String) =
    testNopEditsBy(pubid, col)("Trailing whitespace." -> (_ + " "))

  def testNopEditsBy(pubid: String, col: String)(mods: (String, String => String)*) = {
    val ce = cellEditor(pubid, col)
    import ce._

    val post = global.requestCount.assert.noChange & assertState(Normal).after

    val commitNop = commit +> post

    val nopEdit = (x: (String, String => String)) =>
      (startEdit >> modifyEditorValue(x._2) >> commitNop) group x._1

    Plan.action(
      showAllColumns
        >> (startEdit >> commitNop).group("Commit without edit.")
        >> (nopMod +: mods).map(nopEdit).combine
        >> (startEdit >> abort +> post).group("Abort.")
    ).named(s"NOP edits: $pubid/$col").withInitialState(SampleProject4.projectWithAllAndOtherTags)
  }

  def testKeyboardNavigation()(implicit path: TestPath) =
    runTest(
      Plan.action(
        showHideColumn(StaticField.OtherTags.name)
          +> tableColumns.assert("ID", "Title", StaticField.OtherTags.name)
          >> setFocus(_.table.cell(1, 1).dom)
          >> press(KB.Down)      +> activeElement.assert.equalBy(_.obs.table.cell(2, 1).dom)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.cell(2, 2).dom)
          >> press(KB.Up)        +> activeElement.assert.equalBy(_.obs.table.cell(1, 2).dom)
          >> press(KB.Left)      +> activeElement.assert.equalBy(_.obs.table.cell(1, 1).dom)
          >> press(KB.End)       +> activeElement.assert.equalBy(_.obs.table.cell(1, -1).dom)
          >> press(KB.Home)      +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(1))
          >> press(KB.Left)      +> activeElement.assert.equalBy(_.obs.table.cell(1, -1).dom)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(1))
          >> press(KB.Down)      +> activeElement.assert.equalBy(_.obs.table.rowSelectionInput(2))
          >> press(KB.End.ctrl)  +> activeElement.assert.equalBy(_.obs.table.cell(-1, -1).dom)
          >> press(KB.Home.ctrl) +> activeElement.assert.equalBy(_.obs.table.allRowSelectionInput)
          >> press(KB.Left)      +> activeElement.assert.equalBy(_.obs.table.columnDoms.last.headerCell)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.allRowSelectionInput)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.columnDoms(1).headerCell)
          >> press(KB.Down)      +> activeElement.assert.equalBy(_.obs.table.cell(0, 1).dom)
          >> press(KB.Up)        +> activeElement.assert.equalBy(_.obs.table.columnDoms(1).headerCell)
          >> press(KB.Up)        +> activeElement.assert.equalBy(_.obs.table.cell(-1, 1).dom)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).dom)
          >> press(KB.Right)     +> activeElement.assert.equalBy(_.obs.table.cell(-1, 3).dom)
          >> press(KB.Left)      +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).dom)
          >> press(KB.F2)        +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).editor.get)
          >> press(KB.Tab)       +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).dom) // tab out to cell
          >> press(KB.F2)        +> activeElement.assert.equalBy(_.obs.table.cell(-1, 2).editor.get)
      ) named "Keyboard navigation",
      SampleProject4.projectWithOtherTags
    )

//  def newUseCaseWithMinimalColumns: *.Actions = Plan.action(
//    // select minimal columns
//    // select UC in new dropdown
//    // open form
//    // hit new
//    // confirm new UC exists
//  )

  def testInitialFilter()(implicit path: TestPath) = {
    val project = applyEventsSuccessfully(SampleProject4.project,
      Event.SavedViewCreateV1(
        id         = SavedView.Id(1),
        name       = SavedView.Name("ewrsd"),
        columns    = Column.mandatory.toNEV,
        order      = SortCriteria.byPubidOnly,
        filterDead = ShowDead,
        filter     = Some(Filter.Valid.tag(priHigh))
      ),
    )

    val plan = Plan.action(
      *.emptyAction
        +> filterDead.assert(ShowDead)
        +> filterText.assert("#pri=high")
    )

    runTest(plan withInitialState project)
  }

  def testTagLegality(pubid: String, col: String)(implicit path: TestPath) = {
    val ce = cellEditor(pubid = pubid, col = col)
    import ce._

    val noProdInText   = text.test("doesn't contain prod")(!_.toLowerCase.contains("prod"))
    val noProdInEditor = editorValue.value.test("doesn't contain prod")(!_.get.toLowerCase.contains("prod"))

    val plan = Plan.action(
      enterFilter("BR")
        >> showAllColumns(HideDead)
        +> noProdInText
        >> startEdit
        +> noProdInEditor
        >> testInvalid("prod")
        >> abort

        >> setFilterDead(ShowDead)
        >> startEdit
        +> noProdInEditor
        >> testInvalid("prod")
        >> abort

        >> setFilterDead(HideDead)
        >> enterFilter("#prod")
        +> tablePubids.assert.not.contains(pubid)
    )

    runTest(plan withInitialState SampleProject7.project)
  }

  private def testImpEditorFocusRetention()(implicit path: TestPath): Unit = {
    implicit val ce   = cellEditor("FR-1", "Major Feature")
    val assertFocus   = activeElement.assert.equalBy(ce.editorDom.run(_).get)
    val assertSameDom = ce.editorDom.valueBy(_.get).assert.not.change

    val test = (
      showHideColumn("Major Feature")
        >> cellEditor("FR-1", "Title").startEdit

        >> ce.startEdit
        +> assertFocus

        >> ce.setEditorValue("")
        +> assertFocus
        +> assertSameDom

        >> ce.setEditorValue("mf")
        +> assertFocus
        +> assertSameDom
        +> ce.editorValidity.assert.equal(Invalid)

        >> ce.setEditorValue("mf3")
        +> assertFocus
        +> assertSameDom
        +> ce.editorValidity.assert.equal(Valid)
      )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  private def testTextEditorFocusRetention()(implicit path: TestPath): Unit = {
    implicit val ce   = cellEditor("MF-1", "Description")
    val assertFocus   = activeElement.assert.equalBy(ce.editorDom.run(_).get)
    val assertSameDom = ce.editorDom.valueBy(_.get).assert.not.change

    val test = (
      showHideColumn("Description")
        >> cellEditor("MF-1", "Title").startEdit

        >> ce.startEdit
        +> assertFocus

        >> ce.setEditorValue("")
        +> assertFocus
        +> assertSameDom

        >> ce.setEditorValue("*")
        +> assertFocus
        +> assertSameDom
        +> ce.hasPreview.assert(false)

        >> ce.setEditorValue("* ")
        +> assertFocus
        +> assertSameDom
        +> ce.hasPreview.assert(true)
      )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  private def testAbortionConfirmation()(implicit path: TestPath): Unit = {
    implicit val ce = cellEditor("MF-1", "Description")

    val test = (
      global.disableAutoResponse
        >> confirmJs.setNextResponse(false)
        >> showHideColumn("Description")

        >> ce.doubleClick
        +> ce.editorValue.assert(Some(""))
        >> ce.setEditorValue("      ")
        >> ce.commit
        +> confirmJs.calls.assert(0)
        +> ce.editing.assert(false)

        >> ce.doubleClick
        +> ce.editorValue.assert(Some(""))
        >> ce.setEditorValue("      ")
        >> ce.abort
        +> confirmJs.calls.assert(0)
        +> ce.editing.assert(false)

        >> ce.doubleClick
        +> ce.editorValue.assert(Some(""))
        >> ce.setEditorValue("zxc")
        >> ce.abort.rename("Abort and say no")
        +> confirmJs.calls.assert.increment
        +> ce.editing.assert(true)

        >> confirmJs.setNextResponse(true)
        >> ce.abort.rename("Abort and say yes")
        +> confirmJs.calls.assert.increment
        +> ce.editing.assert(false)
      )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  private def testNewFormStateSharedBetweenReqTypes()(implicit path: TestPath): Unit = {
    val T = "Title"
    val B = "Business Justification"
    val C = "Component"
    // Type B C
    // BR   y n
    // CO   n y
    // FR   y y
    // UC   y n

    val test = (
      showHideColumn(B)
      >> showHideColumn(C)
      +> newFormFields.assert()

      >> newFormButton.click
      +> newFormButton.dropdown.text.assert("BR")
      +> newFormFields.assert(T, B)
      >> newFormEditor(T).setEditorValue("T")
      >> newFormEditor(B).setEditorValue("B")

      >> newFormButton.dropdown.select("CO: Constraint")
      +> newFormButton.dropdown.text.assert("CO")
      +> newFormFields.assert(T, C)
      +> newFormEditor(T).editorValue.assert("T")
      >> newFormEditor(T).setEditorValue("T2")
      >> newFormEditor(C).setEditorValue("C")

      >> newFormButton.dropdown.select("UC: Use Case")
      +> newFormButton.dropdown.text.assert("UC")
      +> newFormFields.assert(T, B)
      +> newFormEditor(T).editorValue.assert("T2")
      +> newFormEditor(B).editorValue.assert("B")
      >> newFormEditor(B).setEditorValue("B2")

      >> newFormButton.dropdown.select("FR: Functional Requirement")
      +> newFormButton.dropdown.text.assert("FR")
      +> newFormFields.assert(T, B, C)
      +> newFormEditor(T).editorValue.assert("T2")
      +> newFormEditor(B).editorValue.assert("B2")
      +> newFormEditor(C).editorValue.assert("C")
      >> newFormEditor(T).setEditorValue("T3")

      >> showHideColumn("Code")
      >> sortBy("Code")
      >> newFormButton.dropdown.select("Code Group")

      >> newFormButton.dropdown.select("CO: Constraint")
      +> newFormButton.dropdown.text.assert("CO")
      +> newFormFields.assert("Code", T, C)
      +> newFormEditor(T).editorValue.assert("T3")
      +> newFormEditor(C).editorValue.assert("C")
    )
    runTest(Plan.action(test) withInitialState SampleProject7.project)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = Tests {
    "initialState" - {
      "default" - runTest(*.emptyPlan)
      "filter" - testInitialFilter()
    }

    "filter" - {
      "total" - runTest(testFilterTotal)
      "partial" - runTest(testFilterPartial)
    }

    "dead" - {
      "cols" - runTest(Plan action testDeadColumns named "testDeadColumns")
      // 'toggle - runTest(testDeadToggleInvariants) TODO Should dead col stay on but hidden when ShowDead→HideDead?
      "notEditable" - runTest(testDeadNotEditable named "testDeadNotEditable")
    }

    "new" - {
      "state" - testNewFormStateSharedBetweenReqTypes()
    }

    "editor" - {
      "impSrc"         - runTest(testImplicationSrcColumnEditor    named "testImplicationSrcColumnEditor"   )
      "impTgt"         - runTest(testImplicationTgtColumnEditor    named "testImplicationTgtColumnEditor"   )
      "impCol"         - runTest(testCustomImplicationColumnEditor named "testCustomImplicationColumnEditor")
      "tagsOther"      - runTest(testOtherTagsColumnEditor         named "testOtherTagsColumnEditor"        )
      "tagsCustom"     - runTest(testCustomTagColumnEditor         named "testCustomTagColumnEditor"        )
      "titleIO"        - runTest(testEditorTitleIO                 named "testEditorTitleIO"                )
      "failClear"      - runTest(testFailureClearedOnEsc           named "testFailureClearedOnEsc"          )

      "focusRetention" - {
        "imp"  - testImpEditorFocusRetention()
        "text" - testTextEditorFocusRetention()
      }

      "tagLegality" - {
        "status"   - testTagLegality("BR-1", "Status")
        "col"      - testTagLegality("BR-1", StaticField.OtherTags.name)
        "exStatus" - testTagLegality("BR-2", "Status")
        "exCol"    - testTagLegality("BR-2", StaticField.OtherTags.name)
      }

      "nop" - {
        // RCG title
        // RCG code
        "title"     - runTest(testNopEdits("MF-6", SpecialBuiltInField.Title.name))
        "textCol"   - runTest(testNopEdits("MF-1", "Description"))
        "impSrc"    - runTest(testNopEdits("MF-1", SpecialBuiltInField.ImplyBackward.name))
        "impTgt"    - runTest(testNopEdits("MF-1", SpecialBuiltInField.ImplyForward.name))
        "impCol"    - runTest(testNopEdits("MF-1", "Major Feature"))
        "otherTags" - runTest(testNopEdits("MF-1", StaticField.OtherTags.name))
        "allTags"   - runTest(testNopEdits("MF-1", StaticField.AllTags.name))
        "tagCol"    - runTest(testNopEdits("MF-1", "Status"))
        "reqCodes"  - runTest(testNopEditsBy("MF-1", SpecialBuiltInField.Code.name)("Trailing \\n." -> (_ + "\n")))
      }

      "abortionConfirmation" - testAbortionConfirmation()
    }

    "kbNav" - testKeyboardNavigation()

//    'new {
//      'useCaseWithMinimalColumns - ???
//      'codeGroupWithMinimalColumns - ???
//    }
  }
}
