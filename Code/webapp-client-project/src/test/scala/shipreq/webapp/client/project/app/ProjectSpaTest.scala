package shipreq.webapp.client.project.app

import utest._
import shipreq.webapp.base.event.FieldCustomDelete
import shipreq.webapp.base.test._
import shipreq.webapp.client.base.test.TestState._
import shipreq.webapp.client.project.app.reqtable2.{ReqTableTestDsl => RT}
import shipreq.webapp.client.project.app.reqdetail.{ReqDetailTestDsl => RD}
import shipreq.webapp.client.project.app.root.Routes.Page
import shipreq.webapp.client.project.test._
import SampleProject.Values.priField
import UnsafeTypes._

object ProjectSpaTest extends TestSuite {
  import ProjectSpaTestDsl._

  PrepareEnv()

  def `ReqTable columns after local config change`: *.Actions = (
    testReqTable(RT.showHideColumn("Priority") >> RT.sortBy("Priority"))
      >> setPage(Page.CfgFields)
      >> applyEvents("Delete Priority field", FieldCustomDelete(priField))
      >> setPage(Page.ReqTable)
  )

  def `ReqTable filterDead after change on detail page`: *.Actions =
    ( setPageToReqDetail("FR-1", RD.Mode.Details)
      >> testReqDetail(RD.filterDeadToggle)
      >> setPage(Page.ReqTable)
    )
    .addCheck(RT.filterDead.assert.change.lift)
    .times(3)

  override def tests = TestSuite {

    'reqTableColumnsSync - runTest(`ReqTable columns after local config change`, Page.ReqTable)

    'reqTableFilterDeadSync - runTest(`ReqTable filterDead after change on detail page`, Page.ReqTable)
  }
}
