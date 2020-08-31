package shipreq.webapp.client.project.app.pages.config.fields

import shipreq.base.util.{Enabled, PotentialChange}
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import sourcecode.Line
import utest._

object DerivativeTagRuleEditorTest extends TestSuite {
  import DerivativeTagRuleEditor._
  import DerivativeTags.TagPair

  /*
      ------------------------------------------------------------------------------------------------------------------
      TagInTree(ApplicableTag.v1(priHigh , "High Priority",   None, "pri=high",   Live), Vector()),
      TagInTree(ApplicableTag.v1(priMed  , "Medium Priority", None, "pri=med",    Live), Vector()),
      ------------------------------------------------------------------------------------------------------------------
      TagInTree(ApplicableTag.v1(wip     , "WIP",             None, "wip",        Live), Vector()),
      TagInTree(ApplicableTag.v1(defer   , "Deferred",        None, "defer",      Live), Vector()),
      TagInTree(ApplicableTag.v1(prod    , "In Production",   None, "prod",       Live), Vector()),
      .
      TagInTree(ApplicableTag.v1(uat     , "In UAT #1",       None, "uat",        Dead), Vector()),
      TagInTree(ApplicableTag.v1(uat2    , "In UAT #2",       None, "uat2",       Dead), Vector()),
      TagInTree(ApplicableTag.v1(uat3    , "In UAT #3",       None, "uat3",       Dead), Vector()),
      ------------------------------------------------------------------------------------------------------------------
      TagInTree(ApplicableTag.v1(v1x     , "v1.x",            None, "v1.x",       Live), Vector(v10, v11, v12, v13)),
      TagInTree(ApplicableTag.v1(v10     , "v1.0",            v10d, "v1.0",       Live), Vector()),
      TagInTree(ApplicableTag.v1(v11     , "v1.1",            v11d, "v1.1",       Live), Vector()),
      TagInTree(ApplicableTag.v1(v12     , "v1.2",            None, "v1.2",       Live), Vector()),
      TagInTree(ApplicableTag.v1(v13     , "v1.3",            None, "v1.3",       Live), Vector()),
      TagInTree(ApplicableTag.v1(v2x     , "v2.x",            None, "v2.x",       Live), Vector()),
      .
      TagInTree(ApplicableTag.v1(v3x     , "v3.x",            None, "v3.x",       Dead), Vector()),
      TagInTree(ApplicableTag.v1(v4x     , "v4.x",            None, "v4.x",       Dead), Vector()),
      TagInTree(ApplicableTag.v1(v09     , "v0.9",            None, "v0.9",       Dead), Vector()),
      ------------------------------------------------------------------------------------------------------------------
   */
  private object Data1 extends SampleProject.Values {
    import SampleProject.projectConfig

    val dt = DerivativeTags(Enabled, Map(
      TagPair(wip, defer) -> prod, // in-scope
      TagPair(prod, defer) -> v10, // in-scope: out-of-scope result ok
      TagPair(wip, prod) -> uat, // in-scope: dead result ok
      TagPair(wip, uat) -> wip, // out-of-scope: dead key
      TagPair(wip, v10) -> wip, // out-of-scope: key no longer part of Status
    ))

    val tags         = projectConfig.tags
    val groupTags    = tags.tagGroupTagsFDV(statusTG)
    val liveTags     = groupTags(HideDead)
  //val deadTags     = groupTags(ShowDead)
    val initialState = State.init(dt, statusTG, tags)
    val validated    = initialState.validate(groupTags)

    implicit lazy val autoCompleteStrategies = DerivativeTagRuleEditor.autoComplete(liveTags)
  }

  override def tests = Tests {

    "initialState" - {
      import Data1._

      val initialText = IR.toText(initialState.ir, tags)

      assertMultiline(initialText,
        """defer + prod = v1.0
          |defer + wip  = prod
          |prod  + wip  = uat
          |""".stripMargin.trim + "\n")

      assertEq(initialState.ir.exists(_.isInstanceOf[IR.Atom.Word]), false)

      assertEq(initialState.initialScope, Set(TagPair(wip, defer), TagPair(wip, prod), TagPair(prod, defer)))

      val expectedValidated = Validated(
        validRules = Map(
          TagPair(defer, prod) -> v10,
          TagPair(defer, wip ) -> prod,
          TagPair(prod,  wip ) -> uat,
        ),
        warnings = Set(
          Warning.DeadTarget(uat),
          Warning.ExternalTarget(v10),
        ),
        invalidity = Set.empty,
      )

      assertEq(validated, expectedValidated)
    }

    "validation" - {
      import Data1._
      import Warning._
      import Invalidity._
      import IR.Atom._

      def test(text    : String)
              (expected: ((ApplicableTagId, ApplicableTagId), ApplicableTagId)*)
              (warnings: Warning*)
              (invalid : Invalidity*)
              (implicit l: Line): Unit = {
        val ir = IR.fromText(text, statusTG, tags)
        val s = State.empty.copy(ir = ir)
        val a = s.validate(groupTags)
        val e = Validated(expected.iterator.map(x => TagPair(x._1._1, x._1._2) -> x._2).toMap, warnings.toSet, invalid.toSet)
        assertEq(a, e)
      }

      "empty"       - test("")()()()
      "ok"          - test("wip + defer = wip")((wip, defer) -> wip)()()
      "abbr"        - test("pro + def = def")((prod, defer) -> defer)()()
      "abbrOver"    - test("pro + defe = defe")((prod, defer) -> defer)()()
      "blankLines"  - test("wip + defer = wip\n\n  \ndefer + prod = prod")((wip, defer) -> wip, (defer, prod) -> prod)()()
      "concise"     - test("wip+defer =wip")((wip, defer) -> wip)()()
      "space"       - test("   wip   +   defer   =   wip   \n")((wip, defer) -> wip)()()
      "deadTgt"     - test("wip + defer = uat")((wip, defer) -> uat)(DeadTarget(uat))()
      "extDeadTgt"  - test("wip + defer = v3.x")((wip, defer) -> v3x)(ExternalTarget(v3x))()
      "extTgt"      - test("wip + defer = v1.0")((wip, defer) -> v10)(ExternalTarget(v10))()
      "deadSrcL"    - test("uat + wip = wip")()()(DeadSource(uat))
      "deadSrcR"    - test("wip + uat = wip")()()(DeadSource(uat))
      "extSrcL"     - test("v1.0 + wip = wip")()()(ExternalSource(v10))
      "extSrcR"     - test("wip + v1.0 = wip")()()(ExternalSource(v10))
      "extDeadSrcL" - test("v3.x + wip = wip")()()(ExternalSource(v3x))
      "extDeadSrcR" - test("wip + v3.x = wip")()()(ExternalSource(v3x))
      "sameSrcsOk"  - test("wip + wip = wip")()(ExplicitRefl(wip))()
      "sameSrcsBad" - test("wip + wip = prod")()()(SameSources(wip))
      "poopPartial" - test("poop wip poops")()()(TagNotFound("poop"), TagNotFound("poops"))
      "poopFull"    - test("wip + poop = poops")()()(TagNotFound("poop"), TagNotFound("poops"))
      "badStmt"     - test("wip +prod")()()(BadStatement(ArraySeq(Tag(wip), Space(1), Plus, Tag(prod))))
      "dup"         - test("wip+defer = wip\n wip+defer = wip\n defer+wip = wip")((wip, defer) -> wip)()()
      "conflict"    - test("wip+defer = wip\ndefer+wip = prod")()()(Conflict(Set(
                        ArraySeq(Tag(wip), Plus, Tag(defer), Space(1), Equals, Space(1), Tag(wip)),
                        ArraySeq(Tag(defer), Plus, Tag(wip), Space(1), Equals, Space(1), Tag(prod)))))
    }

    "change" - {
      import Data1._
      val s2 = initialState.onTextChange("wip + defer = defer", statusTG, tags)
      val v  = s2.validate(groupTags)
      val pc = s2.potentialChange(v)
      assertEq(pc, PotentialChange.Success(
        Map(
          TagPair(wip, defer) -> defer, // new rule
          TagPair(wip, uat) -> wip, // out-of-scope: dead key
          TagPair(wip, v10) -> wip, // out-of-scope: key no longer part of Status
        )
      ))
    }

    "autoComplete" - {
      import AutoCompleteTestUtil._
      import Data1._

      val all = Vector(
        "defer",
        "prod",
        "wip",
      )

      def assertSuggestsAll(prefix: String, suffix: String)(implicit l: Line): Unit =
        assertSuggestionsAndSelectionFor(prefix)(all: _*)(prefix + all.head + suffix)

      "BOI"     - assertSuggestsAll("", " +")
      "space"   - assertSuggestsAll(" ", " +")
      "plus"    - assertSuggestsAll("qwe +", " =")
      "plus_"   - assertSuggestsAll("qwe + ", " =")
      "equals"  - assertSuggestsAll("qwe + zxc =", "\n")
      "equals_" - assertSuggestsAll("qwe + zxc = ", "\n")
      "newLine" - assertSuggestsAll("qwe + zxc = qwe\n", " +")
      "nlSpace" - assertSuggestsAll("qwe + zxc = qwe\n ", " +")
      "w"       - assertSuggestionsAndSelectionFor("w")("wip")("wip +")
      "+d"      - assertSuggestionsAndSelectionFor("wip + d")("defer")("wip + defer =")
      "=d"      - assertSuggestionsAndSelectionFor("wip + defer = d")("defer")("wip + defer = defer\n")
    }

  }
}
