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
      .flatMap(s => m.get(s).map(v => "0x%-8x ~ %s".format(v, s)).toStream)
      .mkString(", ")

  def assertHashes(h: HashScheme, p: Project, ts: (HashScope, Int)*): Unit = {
    val name = if (p eq P0) "P0" else if (p eq P3) "P3" else p.toString
    assertEq(name, fmt(hashes(h, p)), fmt(ts.toMap))
  }

  import HashScope._
  implicit class NumExt(val i: Int) extends AnyVal {
    def ~[A](a: A) = (a, i)
  }

  override def tests = TestSuite {
    'v1 {
      val h = HashScheme.all.head
      assertHashes(h, P0,
        0xed4fcf48 ~ WholeProject,
        0x47de4849 ~ CfgIssueTypes,
        0xa0139eb8 ~ ReqCodes,
        0xa0139eb8 ~ ImplicationData,
        0x47de4849 ~ CfgTags,
        0xa0139eb8 ~ TextFieldData,
        0x47de4849 ~ CfgReqTypes,
        0xbd8a78f7 ~ CfgFields,
        0x57815d25 ~ Reqs,
        0xa0139eb8 ~ TagData)
      assertHashes(h, P3,
        0x6a210433 ~ WholeProject,
        0x67a3e1b9 ~ CfgIssueTypes,
        0xdcbd2f53 ~ ReqCodes,
        0xb31f8764 ~ ImplicationData,
        0x5a1d6a0a ~ CfgTags,
        0xa0139eb8 ~ TextFieldData,
        0x4b71a1ac ~ CfgReqTypes,
        0x3e1ac0cb ~ CfgFields,
        0x45f6c01d ~ Reqs,
        0x314932cc ~ TagData)
    }
    'latest - {
      val h = HashScheme.latest
      assertHashes(h, P0,
        0xed4fcf48 ~ WholeProject,
        0x47de4849 ~ CfgIssueTypes,
        0xa0139eb8 ~ ReqCodes,
        0xa0139eb8 ~ ImplicationData,
        0x47de4849 ~ CfgTags,
        0xa0139eb8 ~ TextFieldData,
        0x47de4849 ~ CfgReqTypes,
        0xbd8a78f7 ~ CfgFields,
        0x57815d25 ~ Reqs,
        0xa0139eb8 ~ TagData)
      assertHashes(h, P3,
        0xa1493797 ~ WholeProject,
        0x67a3e1b9 ~ CfgIssueTypes,
        0x4e7f121c ~ ReqCodes,
        0xb31f8764 ~ ImplicationData,
        0x5a1d6a0a ~ CfgTags,
        0xa0139eb8 ~ TextFieldData,
        0x4b71a1ac ~ CfgReqTypes,
        0x3e1ac0cb ~ CfgFields,
        0x45f6c01d ~ Reqs,
        0x314932cc ~ TagData)
    }
  }
}
