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

  val Vector(hV1, hL) = HashScheme.all.whole

  val P0 = Project.empty
  lazy val P3 = SampleProject3.project

  def hashes(h: HashScheme, p: Project): Map[HashScope, Int] =
    HashScope.all.whole.map(s => s -> HashScope.hash(s, h.value, p)).toMap

  def makeSet(m: Map[HashScope, Int]) =
    HashScope.all.toStream
      .map(s => (s, m.getOrElse(s, Hash.UnsupportedValue)))
      .toSet

  def fmt(s: Set[(HashScope, Int)]) =
    s.toStream
      .sortBy(_._1.toString)
      .map(t => "0x%08x ~ %s".format(t._2, t._1))
      .mkString(", ")

  def assertHashes(h: HashScheme, p: Project, ts: (HashScope, Int)*): Unit = {
    def nameH = if (h eq hL) "latest" else if (h eq hV1) "v1" else h.toString
    def nameP = if (p eq P0) "P0" else if (p eq P3) "P3" else p.toString
    def name = s"$nameH : $nameP"
    var a = makeSet(hashes(h, p))
    var e = makeSet(ts.toMap)
    val common = a & e
    a &~= common
    e &~= common
    assertEq(name, fmt(a), fmt(e))
  }

  import HashScope._
  implicit class NumExt(val i: Int) extends AnyVal {
    def ~[A](a: A) = (a, i)
  }

  // TODO Scopes have the same hash when empty - fix that!
  // Eg 0x47de4849 used by 3, 0xa0139eb8 used by 4

  override def tests = TestSuite {
    'v1 {
      val h = hV1
      assertHashes(h, P0,
        0xed4fcf48 ~ WholeProject,    // Don't change
        0x3515b7ad ~ Config,          // Don't change
        0x47de4849 ~ CfgIssueTypes,   // Don't change
        0x47de4849 ~ CfgReqTypes,     // Don't change
        0xbd8a78f7 ~ CfgFields,       // Don't change
        0x47de4849 ~ CfgTags,         // Don't change
        0x4f22e39f ~ Content,         // Don't change
        0x57815d25 ~ Reqs,            // Don't change
        0xa0139eb8 ~ ReqCodes,        // Don't change
        0xa0139eb8 ~ TextFieldData,   // Don't change
        0xa0139eb8 ~ TagData,         // Don't change
        0xa0139eb8 ~ ImplicationData) // Don't change
      assertHashes(h, P3,
        0x6a210433 ~ WholeProject,    // Don't change
        0x05c14baf ~ Config,          // Don't change
        0x67a3e1b9 ~ CfgIssueTypes,   // Don't change
        0x4b71a1ac ~ CfgReqTypes,     // Don't change
        0x3e1ac0cb ~ CfgFields,       // Don't change
        0x5a1d6a0a ~ CfgTags,         // Don't change
        0xc7754cfb ~ Content,         // Don't change
        0x45f6c01d ~ Reqs,            // Don't change
        0xdcbd2f53 ~ ReqCodes,        // Don't change
        0xa0139eb8 ~ TextFieldData,   // Don't change
        0x314932cc ~ TagData,         // Don't change
        0xb31f8764 ~ ImplicationData) // Don't change
    }
    'latest - {
      val h = hL
      assertHashes(h, P0,
        0xa36e703b ~ WholeProject,
        0x3515b7ad ~ Config,
        0x47de4849 ~ CfgIssueTypes,
        0x47de4849 ~ CfgReqTypes,
        0xbd8a78f7 ~ CfgFields,
        0x47de4849 ~ CfgTags,
        0x4406988a ~ Content,
        0x57815d25 ~ Reqs,
        0xa0139eb8 ~ ReqCodes,
        0xa0139eb8 ~ TextFieldData,
        0xa0139eb8 ~ TagData,
        0xa0139eb8 ~ ImplicationData,
        0x35ed6368 ~ DeletionReasons)
      assertHashes(h, P3,
        0x9959ef1c ~ WholeProject,
        0x05c14baf ~ Config,
        0x67a3e1b9 ~ CfgIssueTypes,
        0x4b71a1ac ~ CfgReqTypes,
        0x3e1ac0cb ~ CfgFields,
        0x5a1d6a0a ~ CfgTags,
        0x16b5e7c8 ~ Content,
        0x45f6c01d ~ Reqs,
        0x8dd89349 ~ ReqCodes,
        0xa0139eb8 ~ TextFieldData, // TODO same as empty - P3 doesn't use
        0x314932cc ~ TagData,
        0xb31f8764 ~ ImplicationData,
        0x17af358b ~ DeletionReasons)
    }
  }
}
