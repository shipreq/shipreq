package shipreq.webapp.base.protocol

import japgolly.microlibs.scalaz_ext.ScalazMacros
import java.time.Instant
import nyaya.gen.Gen
import scalaz.Equal
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data.{Obfuscated, ProjectMetaData}
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.test.BinaryTestUtil._
import utest._

object ProjectSpaEntryPointTest extends TestSuite {

  private implicit val equalProjectSpaInitPageData: Equal[ProjectSpaEntryPoint.InitData] =
    ScalazMacros.deriveEqual

  override def tests = Tests {
    'roundTrip - propTestRoundTrip(ProjectSpaEntryPoint.picklerInitData.asV10)(RandomData.routines.projectSpaInitPageData)
  }
}
