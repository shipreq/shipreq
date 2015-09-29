package shipreq.webapp.base.hash

import utest._
import shipreq.base.util.UnivEq.Implicits._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.SampleProject3

/**
 * These tests ensure the stability of [[HashScheme]]s.
 *
 * Once a [[HashScheme]] has been used (i.e. an Event exists in the database which refers to it), it must never change.
 * Its results should always stay the same. Thus, once used, a [[HashScheme]]s values should be calculated and
 * hard-coded below.
 *
 * If this is (correctly) breaking due to a data structure change, follow the instructions in [[HashScheme]].
 */
object HashSchemeTest extends TestSuite {

  val P0 = Project.empty
  lazy val P3 = SampleProject3.project

  def hashes(h: HashScheme, p: Project): Map[HashScope, Int] =
    HashScope.all.whole.map(s => s -> HashScope.hash(s, h.value, p)).toMap

  def fmt(m: Map[HashScope, Int]): String =
    HashScope.all.toStream
      .flatMap(s => m.get(s).map(v => "%s → 0x%-8x".format(s, v)).toStream)
      .mkString(", ")

  def assertHashes(h: HashScheme, p: Project, ts: (HashScope, Int)*): Unit = {
    val name = if (p eq P0) "P0" else if (p eq P3) "P3" else p.toString
    assertEq(name, fmt(hashes(h, p)), fmt(ts.toMap))
  }

  import HashScope._

  override def tests = TestSuite {
    'v1 {
      val h = HashScheme.all.head
      assertHashes(h, P0, WholeProject → 0xed4fcf48, CfgIssueTypes → 0x47de4849, ReqCodes → 0xa0139eb8, ImplicationData → 0xa0139eb8, CfgTags → 0x47de4849, TextFieldData → 0xa0139eb8, CfgReqTypes → 0x47de4849, CfgFields → 0xbd8a78f7, Reqs → 0x57815d25, TagData → 0xa0139eb8)
      assertHashes(h, P3, WholeProject → 0x6a210433, CfgIssueTypes → 0x67a3e1b9, ReqCodes → 0xdcbd2f53, ImplicationData → 0xb31f8764, CfgTags → 0x5a1d6a0a, TextFieldData → 0xa0139eb8, CfgReqTypes → 0x4b71a1ac, CfgFields → 0x3e1ac0cb, Reqs → 0x45f6c01d, TagData → 0x314932cc)
    }
    'latest - {
      val h = HashScheme.latest
      assertHashes(h, P0, WholeProject → 0xed4fcf48, CfgIssueTypes → 0x47de4849, ReqCodes → 0xa0139eb8, ImplicationData → 0xa0139eb8, CfgTags → 0x47de4849, TextFieldData → 0xa0139eb8, CfgReqTypes → 0x47de4849, CfgFields → 0xbd8a78f7, Reqs → 0x57815d25, TagData → 0xa0139eb8)
      assertHashes(h, P3, WholeProject → 0xa1493797, CfgIssueTypes → 0x67a3e1b9, ReqCodes → 0x4e7f121c, ImplicationData → 0xb31f8764, CfgTags → 0x5a1d6a0a, TextFieldData → 0xa0139eb8, CfgReqTypes → 0x4b71a1ac, CfgFields → 0x3e1ac0cb, Reqs → 0x45f6c01d, TagData → 0x314932cc)
    }
  }
}
