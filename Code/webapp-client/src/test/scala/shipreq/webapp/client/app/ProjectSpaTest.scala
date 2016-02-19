package shipreq.webapp.client.app

import shipreq.webapp.base.test._
import shipreq.webapp.client.test._
import utest._

object ProjectSpaTest extends TestSuite {
  PrepareEnv()

  def runTest = {
    val cp = new TestClientProtocol
    val cd = TestClientData(SampleProject3.project)
    val spa = new ProjectSpaMain(MockRemotes.projectSPA, cp, cd)
  }

  override def tests = TestSuite {

  }
}
