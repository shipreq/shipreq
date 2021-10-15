package shipreq.webapp.client.project.app

import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.pages.config.reqtypes.{ReqTypeConfigTestDsl => CRT}
import shipreq.webapp.client.project.app.pages.content.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.pages.content.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.app.pages.root.{ProjectHomeTestDsl => PH}
import shipreq.webapp.client.project.test._
import shipreq.webapp.member.feature.PreviewFeature.Position
import shipreq.webapp.member.project.data.ExternalPubid
import shipreq.webapp.member.project.event.Event.FieldCustomDelete
import shipreq.webapp.member.test.project.SampleProject.Values.priField
import shipreq.webapp.member.test.project.SampleProject5
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

/** These tests all involve changing routes */
object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  private def runReqDetailTest(action: *.Actions, pubid: String) = {
    val ep = ExternalPubid.parse(pubid).get
    runTest(action, Page.ReqDetail(ep), rd = RD.State(ep, RD.Mode.Details))
  }

  /** ReqTable columns after local config change */
  private def reqTableColumnsSync: *.Actions = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", FieldCustomDelete(priField))
      >> setPage(Page.ReqTable)
  )

  /** ReqTable filterDead after change on detail page */
  private def reqTableFilterDeadSync: *.Actions =
    ( setPageToReqDetail("FR-1", RD.Mode.Details)
      >> testReqDetail(RD.filterDeadToggle)
      >> setPage(Page.ReqTable)
    )
    .addCheck(RT.savedViews.filterDead.assert.change.lift)
    .times(3)

  /** The Usage links in config screens should configure the ReqTable state appropriately */
  private def cfgUsageLinkToReqTable: *.Actions = (
    RT.savedViews.filterText.assert.equal("").lift
      +> setPage(Page.CfgReqTypes)
      >> CRT.clickUsageLink("UC").lift //.updateState(_.copy(page = Page.ReqTable))
      >> setPage(Page.ReqTable) // TODO This shouldn't be needed. It works outside of tests
      +> (
      RT.savedViews.filterText.assert.equal("UC")
        & RT.tablePubids.assert.forall("be UC (or code group)", _.matches("^(?:UC-.*|–)$"))
      ).lift)

  private def testUnsavedChanges: *.Actions = {
    val mf1t = RT.cellEditor("MF-1", "Title")
    val mf1d = RT.cellEditor("MF-1", "Description")
    val uc1t = RT.cellEditor("UC-1", "Title")
    (  PH.startEdit.lift                                       +> unsavedChanges.assert(0)
    >> PH.setEditValue("").lift                                +> unsavedChanges.assert(1)
    >> PH.setEditValue(SampleProject5.project.name + " ").lift +> unsavedChanges.assert(0)
    >> PH.setEditValue("xxxxxxxxxxxxx").lift                   +> unsavedChanges.assert(1)
    >> setPage(Page.ReqTable)                                  +> unsavedChanges.assert(1)
    >> RT.showAllColumns.lift                                  +> unsavedChanges.assert(1)
    >> mf1t.startEdit.lift                                     +> unsavedChanges.assert(1)
    >> mf1d.startEdit.lift                                     +> unsavedChanges.assert(1)
    >> mf1t.modifyEditorValue(_ + "  ").lift                   +> unsavedChanges.assert(1)
    >> mf1t.setEditorValue("zzzzzzzzzzzzz").lift               +> unsavedChanges.assert(2)
    >> mf1d.setEditorValue("zzzzzzzzzzzzz").lift               +> unsavedChanges.assert(3)
    >> mf1t.commit.lift                                        +> unsavedChanges.assert(2)
    >> mf1d.commit.lift                                        +> unsavedChanges.assert(1)
    >> uc1t.startEdit.lift                                     +> unsavedChanges.assert(1)
    >> uc1t.setEditorValue("zzzzzzzzzzzzz").lift               +> unsavedChanges.assert(2)
    >> setPageToReqDetail("UC-1", RD.Mode.Details)             +> unsavedChanges.assert(2)
    >> RD.doubleClickStepText("1.0.1").lift                    +> unsavedChanges.assert(2)
    >> RD.setStepTextEditValue("1.0.1", "zzzzzzzzzzzzz").lift  +> unsavedChanges.assert(3)
    >> RD.commitStepTextEdit("1.0.1").lift                     +> unsavedChanges.assert(2)
    >> setPage(Page.ReqTable)                                  +> unsavedChanges.assert(2)
    >> uc1t.commit.lift                                        +> unsavedChanges.assert(1)
    )
  }

  private def testReauthCommit(online: Boolean, relogin: Boolean): Unit = {
    val edit: *.Actions =
      (RD.title.doubleClick >> RD.title.setEditorValue("alright!") >> RD.title.commit).lift

    val start: *.Actions =
      if (online)
        (  global.disableAutoResponse
        >> edit
        >> global.expireSession)
      else
        (  global.disableAutoResponse
        >> global.expireSession
        +> reauth.isVisible.assert(true)
        >> reauth.clickCancel
        +> reauth.isVisible.assert(false)
        >> edit)

    val postModal: *.Actions =
      if (relogin)
        (  reauth.setPassword("abcdEFGH123!@#")
        +> reauth.requestCount.assert(0)
        >> reauth.clickLogin
        +> reauth.requestCount.assert(1)
        +> reauth.isVisible.assert(false)
        +> RD.title.isSpinning.assert(false).lift
        +> RD.title.editorValue.isEmpty.assert(true).lift
        +> RD.title.text.assert("alright!").lift
        +> unsavedChanges.assert(0)
        +> global.requestCount.assert(1)
        )
      else
        (  reauth.clickCancel
        +> reauth.requestCount.assert(0)
        +> reauth.isVisible.assert(false)
        +> RD.title.isSpinning.assert(false).lift
        +> RD.title.editorValue.assert.some("alright!").lift
        +> unsavedChanges.assert(1)
        +> global.requestCount.assert(0)
        )

    val test: *.Actions =
      start +> reauth.isVisible.assert(true) >> global.clearReqs >> global.enableAutoResponse >> postModal

    runReqDetailTest(test, "MF-1")
  }

  private def testEditorStyle(): Unit = {
    val pubid = "MF-1"
    val field = "Description"
    val rd = RD.field(field)
    val ce = RT.cellEditor(pubid, field)

    val test: *.Actions = (
      // Open editor in ReqDetail
      rd.doubleClick.lift
      >> rd.setEditorValue("Okay.").lift
      +> rd.previewPosition.assert(Some(Position.Right)).lift

      // Switch to ReqTable
      >> setPage(Page.ReqTable)
      >> RT.showAllColumns.lift
      +> ce.editorValue.assert("Okay.").lift
      +> ce.previewPosition.assert(None).lift
      +> ce.hasEnabledFullscreenButton.assert(true).lift

      // Reopen editor
      >> ce.abort.lift
      >> ce.startEdit.lift
      >> ce.setEditorValue("**Wow!**").lift
      +> ce.previewPosition.assert(Some(Position.Under)).lift
      +> ce.hasEnabledFullscreenButton.assert(true).lift

      // Switch to ReqDetail
      >> setPageToReqDetail(pubid, RD.Mode.Details)
      >> rd.setEditorValue("**Wow!**").lift
      +> rd.previewPosition.assert(Some(Position.Right)).lift
      +> rd.hasEnabledFullscreenButton.assert(true).lift

      // Manually hide preview
      >> rd.clickPreviewHide.lift
      +> rd.previewPosition.assert(None).lift

      // Switch to ReqTable
      >> setPage(Page.ReqTable)
      +> ce.previewPosition.assert(None).lift

      // Switch to ReqDetail
      >> setPageToReqDetail(pubid, RD.Mode.Details)
      +> rd.previewPosition.assert(None).lift
    )

    runReqDetailTest(test, pubid)
  }

  private def testReqSearch(): Unit = {

    val test: *.Actions = (
      reqSearch.setQuery("collab")
      +> reqSearch.resultPubids.assert("MF-9", "MF-10", "MF-11")
      +> reqSearch.focusedResult.assert(None)

      >> global.press(KB.Down) +> reqSearch.focusedResult.assert(0)
      >> global.press(KB.Down) +> reqSearch.focusedResult.assert(1)
      >> global.press(KB.Down) +> reqSearch.focusedResult.assert(2)
      >> global.press(KB.Down) +> reqSearch.focusedResult.assert(None)
      >> global.press(KB.Up)   +> reqSearch.focusedResult.assert(2)
      >> global.press(KB.Up)   +> reqSearch.focusedResult.assert(1)
      >> global.press(KB.Up)   +> reqSearch.focusedResult.assert(0)
      >> global.press(KB.Up)   +> reqSearch.focusedResult.assert(None)

      // Like above, routerCtl doesn't seem to work in these tests
//      >> global.press(KB.Enter)
//      >> reqSearch.clickResult(1)
//      +> currentPage.assert.beforeAndAfter(Page.Index, Page.ReqDetail("MF-10"))
//      +> reqSearch.query.assert("collab")
//      +> reqSearch.resultsAreVisible.assert(false)
    )

    runTest(test, Page.Index)
  }

  override def tests = Tests {
    "editorStyle"            - testEditorStyle()
    "reqSearch"              - testReqSearch()
    "reqTableColumnsSync"    - runTest(reqTableColumnsSync     , Page.ReqTable)
    "reqTableFilterDeadSync" - runTest(reqTableFilterDeadSync  , Page.ReqTable)
    "cfgUsageLinkToReqTable" - runTest(cfgUsageLinkToReqTable  , Page.ReqTable)
    "unsavedChanges"         - runTest(testUnsavedChanges      , Page.Index)

    "reauth" - {
      "onlineLogin"   - testReauthCommit(true,  true)
      "onlineCancel"  - testReauthCommit(true,  false)
      "offlineLogin"  - testReauthCommit(false, true)
      "offlineCancel" - testReauthCommit(false, false)
    }
  }
}
