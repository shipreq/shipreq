package shipreq.webapp.member.protocol.entrypoint

import japgolly.microlibs.scalaz_ext.ScalazMacros
import scalaz.Equal
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.test.BinaryTestUtil._
import shipreq.webapp.member.test.project.RandomData
import utest._

object ProjectSpaEntryPointTest extends TestSuite {

  private implicit val equalProjectSpaInitPageData: Equal[ProjectSpaEntryPoint.InitData] =
    ScalazMacros.deriveEqual

  override def tests = Tests {
    "roundTrip" - propTestRoundTrip(ProjectSpaEntryPoint.picklerInitData.asV1(0))(RandomData.routines.projectSpaInitPageData)
  }
}
