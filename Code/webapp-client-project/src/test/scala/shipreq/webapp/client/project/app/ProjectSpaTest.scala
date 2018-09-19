package shipreq.webapp.client.project.app

import utest._
import shipreq.webapp.base.event.FieldCustomDelete
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.cfg.reqtypes.{CfgReqTypesDsl => CRT}
import shipreq.webapp.client.project.app.reqtable.{ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.root.Routes.Page
import shipreq.webapp.client.project.test._
import SampleProject.Values.priField
import UnsafeTypes._

/** These tests all involve changing routes */
object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  /** ReqTable columns after local config change */
  def reqTableColumnsSync: *.Actions = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", FieldCustomDelete(priField))
      >> setPage(Page.ReqTable)
  )

  /** ReqTable filterDead after change on detail page */
  def reqTableFilterDeadSync: *.Actions =
    ( setPageToReqDetail("FR-1", RD.Mode.Details)
      >> testReqDetail(RD.filterDeadToggle)
      >> setPage(Page.ReqTable)
    )
    .addCheck(RT.filterDead.assert.change.lift)
    .times(3)

  /** The Usage links in config screens should configure the ReqTable state appropriately */
  def cfgUsageLinkToReqTable: *.Actions = (
    RT.filterText.assert.equal("").lift
      +> setPage(Page.CfgReqTypes)
      >> CRT.clickUsageLink("UC").lift //.updateState(_.copy(page = Page.ReqTable))
      >> setPage(Page.ReqTable) // TODO This shouldn't be needed. It works outside of tests
      +> (
      RT.filterText.assert.equal("UC")
        & RT.tablePubids.assert.forall("be UC (or code group)", _.matches("^(?:UC-.*|–)$"))
      ).lift)

  override def tests = Tests {

    'reqTableColumnsSync    - runTest(reqTableColumnsSync   , Page.ReqTable)
    'reqTableFilterDeadSync - runTest(reqTableFilterDeadSync, Page.ReqTable)
    'cfgUsageLinkToReqTable - runTest(cfgUsageLinkToReqTable, Page.ReqTable)
  }
}
