package shipreq.webapp.client.project.app

import shipreq.webapp.base.event.Event.FieldCustomDelete
import shipreq.webapp.base.test.SampleProject.Values.priField
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test._
import shipreq.webapp.client.project.app.pages.config.reqtypes.{ReqTypeConfigTestDsl => CRT}
import shipreq.webapp.client.project.app.pages.content.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.pages.content.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.pages.root.Routes.Page
import shipreq.webapp.client.project.app.pages.root.{ProjectHomeTestDsl => PH}
import shipreq.webapp.client.project.test._
import utest._

/** These tests all involve changing routes */
object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

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
    >> mf1t.modifyValue(_ + "  ").lift                         +> unsavedChanges.assert(1)
    >> mf1t.enterValue("zzzzzzzzzzzzz").lift                   +> unsavedChanges.assert(2)
    >> mf1d.enterValue("zzzzzzzzzzzzz").lift                   +> unsavedChanges.assert(3)
    >> mf1t.commit.lift                                        +> unsavedChanges.assert(2)
    >> mf1d.commit.lift                                        +> unsavedChanges.assert(1)
    >> uc1t.startEdit.lift                                     +> unsavedChanges.assert(1)
    >> uc1t.enterValue("zzzzzzzzzzzzz").lift                   +> unsavedChanges.assert(2)
    >> setPageToReqDetail("UC-1", RD.Mode.Details)             +> unsavedChanges.assert(2)
    >> RD.doubleClickStepText("1.0.1").lift                    +> unsavedChanges.assert(2)
    >> RD.setStepTextEditValue("1.0.1", "zzzzzzzzzzzzz").lift  +> unsavedChanges.assert(3)
    >> RD.commitStepTextEdit("1.0.1").lift                     +> unsavedChanges.assert(2)
    >> setPage(Page.ReqTable)                                  +> unsavedChanges.assert(2)
    >> uc1t.commit.lift                                        +> unsavedChanges.assert(1)
    )
  }

  override def tests = Tests {
    "reqTableColumnsSync"    - runTest(reqTableColumnsSync   , Page.ReqTable)
    "reqTableFilterDeadSync" - runTest(reqTableFilterDeadSync, Page.ReqTable)
    "cfgUsageLinkToReqTable" - runTest(cfgUsageLinkToReqTable, Page.ReqTable)
    "unsavedChanges"         - runTest(testUnsavedChanges    , Page.Index)
  }
}
