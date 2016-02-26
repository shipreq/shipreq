package shipreq.webapp.client.app.reqdetail

import japgolly.scalajs.react.test._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.SampleProject3._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ProjectSpaTestDsl
import shipreq.webapp.client.test._
import teststate._
import utest._
import DomZipper.Implicits._

object ReqDetailTest extends TestSuite {
  import ReqDetailTestDsl._

  PrepareEnv()

  def runTest(ep: ExternalPubid, expectedError: Option[String])(action: *.Action = *.emptyAction): Unit = {
    val tc = Test(action, invariants)

    import ProjectSpaTestDsl._

    ProjectSpaTestDsl.runTest(
      setPageToReqDetail(ep, expectedError)
        >> liftReqDetailTests(tc).asAction(s"Req Detail (${PlainText.pubid(ep)})")
    )
  }

  override def tests = TestSuite {

    'badReqType - runTest("QL-1", "Type QL not found.")()
    'badReq     - runTest("FR-9", "FR-9 not found.")()
    'ok         - runTest("FR-1", None)()

//    val u = ReqDetail.Props("EMMEFF", 5, project).component
//    val m = ReactTestUtils.renderIntoDocument(u)
//    ReqDetailObs(DomZipper(m))

  }
}
