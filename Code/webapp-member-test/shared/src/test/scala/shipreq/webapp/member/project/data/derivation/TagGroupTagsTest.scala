package shipreq.webapp.member.project.data.derivation

import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.member.project.data._
import sourcecode.Line
import utest._

object TagGroupTagsTest extends TestSuite {

  override def tests = Tests {

    "abbreviation" - {
      def test(data: (String, String)*)(implicit l: Line): Unit = {
        var tags = Vector.empty[ApplicableTag]
        var expect = Vector.empty[String]
        for ((a, e) <- data) {
          val id = ApplicableTagId(tags.length + 1)
          val t = ApplicableTag.v1(id, "", None, HashRefKey(a), Live)
          tags :+= t
          expect :+= s"$a  -->  $e"
        }
        val abb = new TagGroupTags(tags).abbreviations
        val actual = for (t <- tags) yield s"${t.name}  -->  ${abb(t)}"
        assertMultiline(actual.mkString("\n"), expect.mkString("\n"))
      }

      "status" - test(
        "needs"           -> "nee",
        "needs_"          -> "needs_",
        "needs_ab_de"     -> "needs_ab",
        "needs_abc_def"   -> "needs_abc",
        "needs_more_info" -> "needs_mor",
        "needs_analysis"  -> "needs_ana",
        "analysed"        -> "ana",
        "ready_for_dev"   -> "rea",
        "implemented"     -> "imp",
        "rejected"        -> "rej",
        "meat"            -> "meat",
        "mead"            -> "mead",
        "x"               -> "x",
        "xy"              -> "xy",
        "xyc"             -> "xyc",
        "xych"            -> "xych",
      )
    }
  }
}
