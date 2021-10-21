package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.test.SimEvent.{Keyboard => KB}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.ProjectSpaTestDsl
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.{Event, GenericReqGD}
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.{SampleProject3, SampleProject4, SampleProject7}
import utest._
import utest.framework.TestPath

// Split away from ReqTableTest to avoid PhantomJS crashing once all tests finish
object ReqTableTest2 extends TestSuite {
  import ReqTableTestDsl.{savedViews => _, _}
  import ReqTableTestDsl.savedViews.{* => _, _}
  import global.press

  PrepareEnv()

  def runTest(plan: *.Plan, project: Project = SampleProject4.project)(implicit path: TestPath): Unit =
    runTest(plan withInitialState project)

  def runTest(p: *.PlanWithInitialState)(implicit path: TestPath): Unit = {
    import ProjectSpaTestDsl._
    ProjectSpaTestDsl.runTest(
      liftReqTableTests(p.plan).asAction(path.value.mkString("ReqTableTest2.", ".", "")),
      page = Page.ReqTable,
      project = p.initialState)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  import shipreq.webapp.member.test.project.UnsafeTypes._

  def testCopy(pubid: String, col: String)(expect: String) =
    Plan.action(
      showAllColumns
        >> cellEditor(pubid = pubid, col = col).focus
        >> press(cmdOrCtrl(KB.c))
        +> clipboardText.assert(expect)
    )

  def testPasteClosedDesc = {
    val cell = cellEditor(pubid = "MF-1", col = "Description")
    val text = "* omfg\n* ahh"
    Plan.action(
      copyToClipboard(text)
        >> showAllColumns
        >> cell.focus
        +> cell.assertNotEditing
        +> cell.text.assert("")
        >> press(cmdOrCtrl(KB.v))
        +> cell.assertState(Editing)
        +> cell.editorValue.assert(text)
        >> cell.commit
        >> cell.startEdit
        +> cell.editorValue.assert(text)
    )
  }

  def testPasteClosedTitle = {
    val cell = cellEditor(pubid = "MF-1", col = "Title")
    val text1 = "* omfg\n* ahh"
    val text2 = "* omfg * ahh"
    Plan.action(
      copyToClipboard(text1)
        >> cell.focus
        +> cell.assertNotEditing
        +> cell.text.assert("Use Case Editor")
        >> press(cmdOrCtrl(KB.v))
        +> cell.assertState(Editing)
        +> cell.editorValue.assert(text2)
        >> cell.commit
        >> cell.startEdit
        +> cell.editorValue.assert(text2)
    )
  }

  def testPasteOpenDesc = {
    val cell = cellEditor(pubid = "MF-1", col = "Description")
    val text = "* omfg\n* ahh"
    Plan.action(
      copyToClipboard(text)
        >> showAllColumns
        >> cell.startEdit
        +> cell.editorValue.assert("")
        >> cell.setEditorValue("yo")
        +> cell.editorValue.assert("yo")
        >> cell.focus
        >> press(cmdOrCtrl(KB.v))
        +> cell.assertState(Editing)
        +> cell.editorValue.assert(text)
        >> cell.commit
        >> cell.startEdit
        +> cell.editorValue.assert(text)
    )
  }

  def testFieldRules()(implicit path: TestPath) = {
    val fr1_biz = cellEditor("FR-1", "Business Justification") // perReq > otherwise, opt
    val fr1_alt = cellEditor("FR-1", "Alternatives") // na
    val fr1_cmp = cellEditor("FR-1", "Component") // perReq > otherwise, opt
    val fr1_pri = cellEditor("FR-1", "Priority") // perReq > otherwise, man
    val fr1_sts = cellEditor("FR-1", "Status") // def:tag:dead
    val fr1_ver = cellEditor("FR-1", "Version") // def:tag:bad

    val br1_biz = cellEditor("BR-1", "Business Justification") // man
    val br1_alt = cellEditor("BR-1", "Alternatives") // na
    val br1_cmp = cellEditor("BR-1", "Component") // na
    val br1_pri = cellEditor("BR-1", "Priority") // def:tag:ok

    val si1_biz = cellEditor("SI-1", "Business Justification")
    val si1_alt = cellEditor("SI-1", "Alternatives")
    val si1_cmp = cellEditor("SI-1", "Component")
    val si1_pri = cellEditor("SI-1", "Priority")
    val si1_sts = cellEditor("SI-1", "Status")
    val si1_ver = cellEditor("SI-1", "Version")

    val plan = Plan.action(
      showAllColumns(HideDead)

      +> fr1_alt.isNA.assert(true)
      >> fr1_biz.changeToAndBack("" -> "X")
      >> fr1_cmp.changeToAndBack("" -> "X")
      >> fr1_pri.changeToAndBack("" -> "pri=low", "blank" -> "pri=low")
      >> fr1_sts.changeToAndBack("" -> "wip")
      >> fr1_ver.changeToAndBack("" -> "v1.0")

      +> br1_alt.isNA.assert(true)
      +> br1_cmp.isNA.assert(true)
      >> br1_biz.changeToAndBack("" -> "uiui", "blank" -> "uiui")
      >> br1_pri.changeToAndBack("" -> "pri=low", "pri=med" -> "pri=low")

      >> showAllColumns(ShowDead)

      +> si1_biz.isNA.assert(true)
      +> si1_alt.text.assert("")
      +> si1_cmp.text.assert("")
      +> si1_pri.text.assert("")
      +> si1_sts.text.assert("uat3")
      +> si1_ver.text.assert("")

      +> fr1_alt.isNA.assert(true)
      +> fr1_biz.text.assert("")
      +> fr1_cmp.text.assert("")
      +> fr1_pri.text.assert("blank")
      >> fr1_sts.changeToAndBack("" -> "wip", "uat2" -> "wip")
      >> fr1_ver.changeToAndBack("" -> "v1.0")

      +> br1_alt.isNA.assert(true)
      +> br1_cmp.isNA.assert(true)
      +> br1_biz.text.assert("blank")
      >> br1_pri.changeToAndBack("" -> "pri=low", "pri=med" -> "pri=low")
    )

    runTest(plan withInitialState SampleProject7.project)
  }

  def testFieldRulesAndSorting()(implicit path: TestPath) = {
    import shipreq.webapp.member.test.project.SampleProject7.Values._
    import shipreq.webapp.member.test.project.UnsafeTypes._

    val project = applyEventsSuccessfully(
      SampleProject7.project,
      Event.ReqTagsPatch(frs(1), nesd()(priLow)),
      Event.ReqTagsPatch(frs(2), nesd()(priHigh)),
      Event.GenericReqCreate(frs(3), fr, GenericReqGD.ValueForTitle("poop")),
    )

    val plan = Plan.action(
      enterFilter("FR | BR")
      >> showHideColumn("Priority")
      >> sortBy("Priority")
      +> tablePubids.assert.equal("FR-2", "BR-1", "BR-2", "BR-3", "FR-1", "FR-3") // BRs have default of pri=med
      //                           high    med     med     med     low     blank
      >> filterDeadToggle
      +> tablePubids.assert.equal("FR-2", "BR-1", "BR-2", "BR-3", "FR-1", "FR-3") // BRs have default of pri=med
    )

    runTest(plan withInitialState project)
  }

  def testFieldRulesAndFilter()(implicit path: TestPath) = {
    import shipreq.webapp.member.test.project.SampleProject7.Values._
    import shipreq.webapp.member.test.project.UnsafeTypes._

    val project = applyEventsSuccessfully(
      SampleProject7.project,
      Event.ReqTagsPatch(frs(1), nesd()(priMed)),
      Event.ReqTagsPatch(frs(2), nesd()(priHigh)),
      Event.GenericReqCreate(frs(3), fr, GenericReqGD.ValueForTitle("poop")),
      Event.GenericReqCreate(brs(4), br, GenericReqGD.ValueForTags(NonEmptySet(priHigh))),
    )

    val plan = Plan.action(
      enterFilter("(FR | BR) #pri=med")
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "BR-1", "BR-2", "BR-3") // BRs have default of pri=med
      >> filterDeadToggle
      +> tablePubids.assert.equalIgnoringOrder("FR-1", "BR-1", "BR-2", "BR-3") // BRs have default of pri=med
    )

    runTest(plan withInitialState project)
  }

  private def assertPreview(editing: Boolean, preview: String, isFS: Boolean, canFS: Boolean, spin: Boolean)
                           (implicit f: CellEditor) = {
    val p = if (preview == "none") "----" else preview
    (
      f.editing.assert(editing)
        & f.hasPreview.assert(p.startsWith("-h") || (preview == "----"))
        & f.previewButtonsStr.assert(p)
        & f.isFullscreen.assert(isFS)
        & global.isBrowserFullscreen.assert(isFS)
        & f.hasEnabledFullscreenButton.assert(canFS)
        & f.isSpinning.assert(spin)
      )
  }

  private def testTitlePreview()(implicit path: TestPath): Unit = {
    implicit val ce = cellEditor("MF-1", "Title")

    val test = (
       ce.doubleClick                +> assertPreview(editing = y, preview = "none", isFS = n, canFS = n, spin = n)
    >> ce.setEditorValue("**bold**") +> assertPreview(editing = y, preview = "----", isFS = n, canFS = n, spin = n)
    )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  private def testPreviewControls()(implicit path: TestPath): Unit = {
    implicit val ce = cellEditor("MF-1", "Description")

    val test = (
      global.disableAutoResponse
        >> showHideColumn("Description")

        >> ce.doubleClick
        +> ce.editorValue.assert(Some(""))
        +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)

        >> ce.setEditorValue("**bold**")
        +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)

        >> ce.clickPreviewRight     +> assertPreview(editing = y, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewHide      +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewShow      +> assertPreview(editing = y, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewDown      +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewHide      +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewShow      +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> ce.setEditorValue("w")   +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> ce.commit                +> assertPreview(editing = n, preview = "none", isFS = n, canFS = n, spin = y)
        >> global.autoRespondToLast +> assertPreview(editing = n, preview = "none", isFS = n, canFS = n, spin = n)

        // No confirm preview-reset while we're here.
        // See comments in ReqDetailTest as to why this is important.
        >> ce.doubleClick
        +> ce.editorValue.assert(Some("w"))
        +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)

        >> ce.setEditorValue("**bold**")
        +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)
      )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  private def testPreviewFullscreen()(implicit path: TestPath): Unit = {
    implicit val ce = cellEditor("MF-1", "Description")

    val startEditing = ce.doubleClick +> ce.editorValue.assert(Some(""))

    val test = (
      global.disableAutoResponse
        >> showHideColumn("Description")
        >> startEditing               +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)
        >> ce.toggleFullscreen        +> assertPreview(editing = y, preview = "-h-r", isFS = y, canFS = y, spin = n)
        >> ce.unfocusEditor           +> assertPreview(editing = y, preview = "-h-r", isFS = y, canFS = y, spin = n)
        >> ce.toggleFullscreen        +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)
        >> ce.setEditorValue("**x**") +> assertPreview(editing = y, preview = "-h-r", isFS = n, canFS = y, spin = n)
        >> ce.toggleFullscreen        +> assertPreview(editing = y, preview = "-h-r", isFS = y, canFS = y, spin = n)
        >> ce.clickPreviewRight       +> assertPreview(editing = y, preview = "-hd-", isFS = y, canFS = y, spin = n)
        >> ce.toggleFullscreen        +> assertPreview(editing = y, preview = "-hd-", isFS = n, canFS = y, spin = n)
        >> ce.clickPreviewHide        +> assertPreview(editing = y, preview = "s---", isFS = n, canFS = y, spin = n)
        >> ce.toggleFullscreen        +> assertPreview(editing = y, preview = "s---", isFS = y, canFS = y, spin = n)
      )

    runTest(Plan.action(test) withInitialState SampleProject3.project)
  }

  def testSavedViewsBasic()(implicit path: TestPath) = {

    val assertViewBasic =
      tableColumns.size.assert(7) &
      tableColumns.assert("ID", "Req Type", "Implied By", "Title", "Code", "Deletion Reason", "Implies") &
      filterText.assert("")

    val assertViewA =
      tableColumns.size.assert(18) &
      filterText.assert("MF") &
      filterDead.assert(HideDead)

    val assertViewD =
      tableColumns.size.assert(20) &
      filterText.assert("UC") &
      filterDead.assert(ShowDead)

    val test = (
      *.emptyAction +> savedViews.assert("> Unsaved view") +> filterText.assert("")

        >> showBuiltInColumnsSortedByPubid
        +> assertViewBasic
        >> saveCurrentView("basic")
        +> savedViews.assert("> * basic")
        +> assertViewBasic

        >> showAllColumns(HideDead)
        >> enterFilter("MF")
        +> assertViewA
        +> savedViews.assert("* basic", "> Unsaved view")
        >> saveCurrentView("AAA")
        +> savedViews.assert("> AAA", "* basic")
        +> assertViewA

        >> showAllColumns(ShowDead)
        >> enterFilter("UC")
        +> assertViewD
        +> savedViews.assert("AAA", "* basic", "> Unsaved view")
        >> saveCurrentView("ded")
        +> savedViews.assert("AAA", "* basic", "> ded")
        +> assertViewD

        >> selectView("basic")   +> savedViews.assert("AAA", "> * basic", "ded") +> assertViewBasic
        >> selectView("AAA")     +> savedViews.assert("> AAA", "* basic", "ded") +> assertViewA
        >> selectView("ded")     +> savedViews.assert("AAA", "* basic", "> ded") +> assertViewD
        >> setDefaultView("AAA") +> savedViews.assert("* AAA", "basic", "> ded") +> assertViewD
        >> setDefaultView("ded") +> savedViews.assert("AAA", "basic", "> * ded") +> assertViewD
        >> selectView("AAA")     +> savedViews.assert("> AAA", "basic", "* ded") +> assertViewA
      )

    runTest(Plan.action(test) withInitialState SampleProject7.project)
  }

  def testSavedViewsDeadCol()(implicit path: TestPath) = {
    val test = (
      *.emptyAction

        >> showMandatoryColumnsSortedByPubid
        +> filterDead.assert(HideDead)
        +> tableColumns.assert("ID", "Title")
        +> filterText.assert("")
        >> showHideColumn("Description")
        >> showHideColumn("Component")
        +> tableColumns.assert("ID", "Title", "Description", "Component")
        >> saveCurrentView("yo!")
        +> savedViews.assert("> * yo!")

        >> receiveExternalEvent(Event.FieldCustomDelete(SampleProject7.Values.descField))
        +> savedViews.assert("> * yo!")
        +> tableColumns.assert("ID", "Title", "Component")

        >> receiveExternalEvent(Event.FieldCustomRestore(SampleProject7.Values.descField))
        +> savedViews.assert("> * yo!")
        +> tableColumns.assert("ID", "Title", "Description", "Component")

        >> showHideColumn("Priority")
        +> savedViews.assert("* yo!", "> Unsaved view")
        +> tableColumns.assert("ID", "Title", "Description", "Component", "Priority")

        >> receiveExternalEvent(Event.FieldCustomDelete(SampleProject7.Values.descField))
        +> savedViews.assert("* yo!", "> Unsaved view")
        +> tableColumns.assert("ID", "Title", "Component", "Priority")

        >> receiveExternalEvent(Event.FieldCustomRestore(SampleProject7.Values.descField))
        +> savedViews.assert("* yo!", "> Unsaved view")
        +> tableColumns.assert("ID", "Title", "Description", "Component", "Priority")

        >> showHideColumn("Priority")
        +> savedViews.assert("> * yo!")
        +> tableColumns.assert("ID", "Title", "Description", "Component")

        >> receiveExternalEvent(Event.FieldCustomDelete(SampleProject7.Values.descField))
        >> showHideColumn("Priority")
        +> savedViews.assert("* yo!", "> Unsaved view")
        +> tableColumns.assert("ID", "Title", "Component", "Priority")

        >> saveAndReplaceView("yo!")
        +> savedViews.assert("> * yo!")
        +> tableColumns.assert("ID", "Title", "Component", "Priority")

        >> receiveExternalEvent(Event.FieldCustomRestore(SampleProject7.Values.descField))
        +> savedViews.assert("> * yo!")
        +> tableColumns.assert("ID", "Title", "Component", "Priority")
      )

    runTest(Plan.action(test) withInitialState SampleProject7.project)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  override def tests = Tests {
    "copy" - {
      "title"     - runTest(testCopy("MF-1", "Title")("Use Case Editor"))
      "desc"      - runTest(testCopy("UC-1", "Description")("This UC is about eating."))
      "id"        - runTest(testCopy("MF-1", "ID")("[MF-1] Use Case Editor"))
      "grReqType" - runTest(testCopy("MF-1", "Req Type")("MF"))
      "ucReqType" - runTest(testCopy("UC-1", "Req Type")("UC"))
      "imps"      - runTest(testCopy("FR-1", "Implies")("CO-2, FR-2"))
      "tags"      - runTest(testCopy("MF-5", "Priority")("pri=high"))
      "tagsEmpty" - runTest(testCopy("MF-2", "Status")(""))
      "dead"      - runTest(testCopy("CO-1", "Title")("Search entities!"))
    }

    "paste" - {
      "closedDesc"  - runTest(testPasteClosedDesc)
      "closedTitle" - runTest(testPasteClosedTitle)
      "openDesc"    - runTest(testPasteOpenDesc)
    }

    "fieldRules" - {
      "main"    - testFieldRules()
      "sorting" - testFieldRulesAndSorting()
      "filter"  - testFieldRulesAndFilter()
    }

    "preview" - {
      "title" - testTitlePreview()
      "controls" - testPreviewControls()
      "fullscreen" - testPreviewFullscreen()
    }

    "savedViews" - {
      "basic" - testSavedViewsBasic()
      "deadCol" - testSavedViewsDeadCol()
    }
  }
}