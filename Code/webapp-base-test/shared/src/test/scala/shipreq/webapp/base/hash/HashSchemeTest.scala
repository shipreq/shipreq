package shipreq.webapp.base.hash

import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.test._
import WebappTestUtil._

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

  lazy val Vector(hL) = HashScheme.all.whole

  val P0 = Project.empty
  lazy val P3 = SampleProject3.project
  lazy val P4 = SampleProject4.project

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
    def nameH = if (h eq hL) "latest" else h.toString
    def nameP = if (p eq P0) "P0" else if (p eq P3) "P3" else if (p eq P4) "P4" else p.toString
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

  override def tests = TestSuite {
    'latest - {
      'P0 - assertHashes(hL, P0,
        0xbd8a78f7 ~ CfgFields,
        0x8929e247 ~ CfgIssueTypes,
        0x53a65700 ~ CfgReqTypes,
        0xfff75860 ~ CfgTags,
        0x16519109 ~ Config,
        0xc72a3dda ~ Content,
        0x35ed6368 ~ DeletionReasons,
        0xf71674f7 ~ GenericReqs,
        0x7934b838 ~ ImplicationData,
        0x724ff13d ~ PubidRegister,
        0x7548501d ~ ReqCodes,
        0xf8efb0b3 ~ Reqs,
        0x46a5c86e ~ TagData,
        0xc32727ce ~ TextFieldData,
        0x43b92d0f ~ UseCases,
        0x12795e1a ~ WholeProject)

      'P3 - assertHashes(hL, P3,
        0x3e1ac0cb ~ CfgFields,
        0x25b0e4e6 ~ CfgIssueTypes,
        0x36b43113 ~ CfgReqTypes,
        0x7e949e3c ~ CfgTags,
        0x7801ef17 ~ Config,
        0xc199bfe3 ~ Content,
        0x17af358b ~ DeletionReasons,
        0x8090fed8 ~ GenericReqs,
        0x34c6056e ~ ImplicationData,
        0x084cb8cc ~ PubidRegister,
        0x6f1d8f9b ~ ReqCodes,
        0x5ac4920c ~ Reqs,
        0x174ee061 ~ TagData,
        0xc32727ce ~ TextFieldData, // Note: same as empty, P3 doesn't use
        0x43b92d0f ~ UseCases,      // Note: same as empty, P3 doesn't use
        0x8ba09927 ~ WholeProject)

      'P4 - assertHashes(hL, P4,
        0x3e1ac0cb ~ CfgFields,
        0x25b0e4e6 ~ CfgIssueTypes,
        0x36b43113 ~ CfgReqTypes,
        0x7e949e3c ~ CfgTags,
        0x7801ef17 ~ Config,
        0xcdffe5df ~ Content,
        0x17af358b ~ DeletionReasons,
        0x8090fed8 ~ GenericReqs,
        0x34c6056e ~ ImplicationData,
        0x719d13b1 ~ PubidRegister,
        0x6f1d8f9b ~ ReqCodes,
        0x46717e98 ~ Reqs,
        0x174ee061 ~ TagData,
        0xcef27507 ~ TextFieldData,
        0x73fb17ff ~ UseCases,
        0xaf320bd1 ~ WholeProject)
    }
  }
}
