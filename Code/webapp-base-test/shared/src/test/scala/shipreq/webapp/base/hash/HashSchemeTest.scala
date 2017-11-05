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

  lazy val Vector(h1) = HashSchemes.schemes.whole

  val PE = Project.empty.copy(name = "Empty")
  lazy val P3 = SampleProject3.project
  lazy val P4 = SampleProject4.project

  def makeSet(m: Map[HashScope, Int]) =
    HashScope.all.iterator
      .map(s => (s, m.getOrElse(s, 0)))
      .toSet

  def fmt(s: Set[(HashScope, Int)]) =
    s.toStream
      .sortBy(_._1.toString)
      .map(t => "0x%08x ~ %s".format(t._2, t._1))
      .mkString(", ")

  def assertHashes(h: HashScheme, p: Project, expectations: (HashScope, Int)*): Unit = {
    // def nameH = if (h eq HashSchemes.latest) "latest" else h.toString
    def nameH = h.toString
    def nameP = if (p eq PE) "PE" else if (p eq P3) "P3" else if (p eq P4) "P4" else p.toString
    def name = s"$nameH : $nameP"
    var a = makeSet(h.hash(p))
    var e = makeSet(expectations.toMap)
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
    'V1 - {
      def h = h1

      'PE - assertHashes(h, PE,
        0x19f80db3 ~ CfgFields,
        0x8929e247 ~ CfgIssueTypes,
        0x53a65700 ~ CfgReqTypes,
        0xfff75860 ~ CfgTags,
        0x35ed6368 ~ DeletionReasons,
        0xf71674f7 ~ GenericReqs,
        0x7934b838 ~ ImplicationData,
        0x969d43a3 ~ ProjectName,
        0x724ff13d ~ PubidRegister,
        0x7548501d ~ ReqCodes,
        0x17e3e911 ~ SavedViews,
        0x46a5c86e ~ TagData,
        0xc32727ce ~ TextFieldData,
        0x43b92d0f ~ UseCases,
      )

      'P3 - assertHashes(h, P3,
        0x3e1ac0cb ~ CfgFields,
        0x25b0e4e6 ~ CfgIssueTypes,
        0x36b43113 ~ CfgReqTypes,
        0x7e949e3c ~ CfgTags,
        0x17af358b ~ DeletionReasons,
        0x8090fed8 ~ GenericReqs,
        0x34c6056e ~ ImplicationData,
        0x3ae9403b ~ ProjectName,
        0x084cb8cc ~ PubidRegister,
        0x6f1d8f9b ~ ReqCodes,
        0x17e3e911 ~ SavedViews,
        0x174ee061 ~ TagData,
        0xc32727ce ~ TextFieldData, // Note: same as empty, P3 doesn't use
        0x43b92d0f ~ UseCases,      // Note: same as empty, P3 doesn't use
      )

      'P4 - assertHashes(h, P4,
        0x3e1ac0cb ~ CfgFields,
        0x25b0e4e6 ~ CfgIssueTypes,
        0x36b43113 ~ CfgReqTypes,
        0x7e949e3c ~ CfgTags,
        0x17af358b ~ DeletionReasons,
        0x8090fed8 ~ GenericReqs,
        0x34c6056e ~ ImplicationData,
        0x537726aa ~ ProjectName,
        0x719d13b1 ~ PubidRegister,
        0x6f1d8f9b ~ ReqCodes,
        0x17e3e911 ~ SavedViews,
        0x174ee061 ~ TagData,
        0xcef27507 ~ TextFieldData,
        0x73fb17ff ~ UseCases,
      )

      // TODO Add SampleProject and hash tests that cover SavedViews
    }
  }
}
