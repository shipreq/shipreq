package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import sourcecode.Line
import shipreq.webapp.base.data._
import utest._
import shipreq.webapp.base.text.PlainText
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.test._

object VirtualProjectTagsTest extends TestSuite {
  import VirtualProjectTags.DerivativeTagFactor
  import VirtualProjectTags.DerivativeTagFactor.SourceType

  override def tests = Tests {
    "derivativeTags" - {
      "sc2" - derivativeTags_sc2()
      "sc4" - {
        "step1" - derivativeTags_sc4_step1()
        "step2" - derivativeTags_sc4_step2()
        "step3" - derivativeTags_sc4_step3()
        "step4" - derivativeTags_sc4_step4()
      }
    }
  }

  private def summariseDerivativeTags(p: Project,
                                      fieldId: CustomField.Tag.Id,
                                      reqTypeOrder: Vector[ReqTypeId]): String = {
    val tags = p.virtualTags
    def req(id: ReqId) = PlainText.pubid(p.content.reqs.need(id).pubid, p)
    def tag(id: ApplicableTagId) = p.config.tags.needApplicableTag(id).name
    def tagVec(ids: Vector[ApplicableTagId]) = ids.iterator.map(tag).mkString("{", " ", "}")

    val showType: SourceType => String = {
      case SourceType.Default => "default"
      case SourceType.Derived => "derived"
      case SourceType.Manual  => "manual"
    }

    val showFactor: DerivativeTagFactor => String = {

      case DerivativeTagFactor.EmptySelf =>
        "self: ∅"

      case DerivativeTagFactor.EmptyRelation(src, _) =>
        req(src) + ": ∅"

      case DerivativeTagFactor.Self(t, srcType) =>
        s"self: ${tag(t)} (${showType(srcType)})"

      case DerivativeTagFactor.Relation(r, _, t, srcType) =>
        s"${req(r)}: ${tag(t)} (${showType(srcType)})"
    }

    type Item = ((Int, Int), String)

    def unorderedReqTypes =
      p.config.reqTypes.allSortedByMnemonic.iterator.map(_.reqTypeId).filterNot(reqTypeOrder.contains)

    val reqTypeOrderMap =
      (reqTypeOrder ++ unorderedReqTypes).mapToOrder

    def perReq: Iterator[Item] = {
      p.content.reqs.reqIterator().map { r =>
        val pubid       = req(r.id)
        val liveResults = tags(r.id, HideDead).fieldOrdered(fieldId) |> tagVec
        val deadResults = tags(r.id, ShowDead).fieldOrdered(fieldId) |> tagVec

        val factors =
          MutableArray(
            tags(r.id).derivativeTagFactors(fieldId).iterator.map("\n  + " + showFactor(_))
          ).sort.mkString

        var result = pubid + factors + "\n  = " + liveResults

        if (deadResults != liveResults)
          result += s"\n    $deadResults (ShowDead)"

        (reqTypeOrderMap(r.reqTypeId), r.pubid.pos.value) -> result
      }
    }

    MutableArray(perReq).sortBy(_._1).iterator().map(_._2).mkString("\n")
  }

  private def assertDerivativeTags(p: Project,
                                   f: CustomField.Tag.Id,
                                   reqTypeOrder: Vector[ReqTypeId],
                                   alwaysSimplifyFirst: Boolean = false)(_expect: String)(implicit l: Line): Unit = {
    val actual = summariseDerivativeTags(p, f, reqTypeOrder)
    val expect = _expect.trim.replaceAll(" *//.+?(?=\n|$)", "")
    if (actual != expect) {

      def simplify(s: String): String =
        s.linesIterator
          .filterNot(_.startsWith("  +"))
          .filterNot(_.contains("ShowDead"))
          .mkString("\n")
          .replace("\n  =", " =")

//      println(actual)
      if (alwaysSimplifyFirst || !expect.contains("  +"))
        assertMultiline(simplify(actual), simplify(expect))
      assertMultiline(actual, expect)
    }
  }

  private def derivativeTags_sc2(): Unit = {
    import SampleDerivativeTags1._, Values._
    assertDerivativeTags(project, verField, Vector(fr, iv, mf, fb))(
      """FR-1
        |  + self: v1 (manual)
        |  = {v1}
        |FR-2
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v2}
        |FR-3
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v2}
        |FR-4
        |  + self: v1 (manual)
        |  = {v1}
        |IV-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1 v2}
        |IV-2
        |  + FR-3: v2 (derived)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v2}
        |IV-3
        |  + FR-4: v1 (manual)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1 v2}
        |MF-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + FR-3: v2 (derived)
        |  + FR-4: v1 (manual)
        |  + IV-1: v1 (derived)
        |  + IV-1: v2 (derived)
        |  + IV-2: v2 (derived)
        |  + IV-3: v1 (derived)
        |  + IV-3: v2 (derived)
        |  + self: v2 (manual)
        |  = {v1 v2}
        |FB-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + FR-3: v2 (derived)
        |  + FR-4: v1 (manual)
        |  + IV-1: v1 (derived)
        |  + IV-1: v2 (derived)
        |  + IV-2: v2 (derived)
        |  + IV-3: v1 (derived)
        |  + IV-3: v2 (derived)
        |  + MF-1: v1 (derived)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1 v2}
        |""".stripMargin
    )
  }

  private def derivativeTags_sc4_step1(): Unit = {
    import SampleDerivativeTags2._, Values._
    assertDerivativeTags(step1, statusField, Vector(fr, iv, mf, fb))(
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-4
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: needsAnalysis (default)
        |  = {needsAnalysis}
        |IV-3
        |  + FR-4: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: readyForDev (default)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: needsAnalysis (default)
        |  + IV-3: analysed (manual)
        |  + IV-3: readyForDev (derived)
        |  = {needsAnalysis}
        |FB-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: readyForDev (default)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: needsAnalysis (default)
        |  + IV-3: analysed (manual)
        |  + IV-3: readyForDev (derived)
        |  + MF-1: needsAnalysis (derived)
        |  = {needsAnalysis}
        |""".stripMargin
    )
  }

  private def derivativeTags_sc4_step2(): Unit = {
    import SampleDerivativeTags2._, Values._
    assertDerivativeTags(step2, statusField, Vector(fr, iv, mf, fb))(
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-4
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: readyForDev (default)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: readyForDev (derived)
        |  = {readyForDev}
        |FB-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: readyForDev (default)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: readyForDev (derived)
        |  + MF-1: readyForDev (derived)
        |  = {readyForDev}
        |""".stripMargin
    )
  }

  private def derivativeTags_sc4_step3(): Unit = {
    import SampleDerivativeTags2._, Values._
    assertDerivativeTags(step3, statusField, Vector(fr, iv, mf, fb))(
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-4
        |  + self: implemented (manual)
        |  = {implemented}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  = {readyForDev}
        |FB-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  + MF-1: readyForDev (derived)
        |  = {readyForDev}
        |""".stripMargin
    )
  }

  private def derivativeTags_sc4_step4(): Unit = {
    import SampleDerivativeTags2._, Values._
    println(Console.YELLOW_B + ("=" * 80) + Console.RESET)
    assertDerivativeTags(step4, statusField, Vector(fr, iv, mf, fb))(
      """FR-1
        |  + FR-3: implemented (derived)
        |  + self: implemented (manual)
        |  = {implemented}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev}
        |FR-3
        |  + FR-1: implemented (manual)
        |  = {implemented}
        |FR-4
        |  + self: implemented (manual)
        |  = {implemented}
        |IV-1
        |  + FR-1: implemented (manual)
        |  + FR-2: readyForDev (default)
        |  + FR-3: implemented (derived)
        |  + self: analysed (manual)
        |  = {analysed readyForDev} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: implemented (manual)
        |  + FR-2: readyForDev (default)
        |  + FR-3: implemented (derived)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  = {readyForDev}
        |FB-1
        |  + FR-1: implemented (manual)
        |  + FR-2: readyForDev (default)
        |  + FR-3: implemented (derived)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  + MF-1: readyForDev (derived)
        |  = {readyForDev}
        |""".stripMargin
    )
  }
}
