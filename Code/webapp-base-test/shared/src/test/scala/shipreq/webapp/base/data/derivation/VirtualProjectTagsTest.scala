package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.PlainText
import sourcecode.Line
import utest._

object VirtualProjectTagsTest extends TestSuite {
  import VirtualProjectTags.DerivativeTagFactor
  import VirtualProjectTags.DerivativeTagFactor.SourceType

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
                                   reqTypeOrder: Vector[ReqTypeId] = Vector.empty,
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

  // TODO move into multilibs
  private def assertMultiline(actual: String, expect: String)(implicit q: Line): Unit = {
    import japgolly.microlibs.testutil.TestUtilInternals._
    import scala.io.AnsiColor._

    if (actual != expect) withAtomicOutput {
      println()
      val AE = List(actual, expect).map(_.split("\n"))
      val List(as, es) = AE
      val lim = as.length max es.length
      val List(maxA,_) = AE.map(x => (0 :: x.iterator.map(_.length).toList).max)
      val maxL = lim.toString.length
      if (maxL == 0 || maxA == 0)
        assertEq(actual, expect)
      else {
        if (as.length == es.length) {
          println(s"${BRIGHT_YELLOW}assertMultiline:$RESET actual | expect")
          val fmtOK = s"${BRIGHT_BLACK}%${maxL}d: %-${maxA}s | | %s${RESET}\n"
          val fmtWS = s"${WHITE}%${maxL}d: ${RED_B}${BLACK}%-${maxA}s${RESET}${WHITE} |≈| ${GREEN_B}${BLACK}%s${RESET}\n"
          val fmtKO = s"${WHITE}%${maxL}d: ${BOLD_BRIGHT_RED}%-${maxA}s${RESET}${WHITE} |≠| ${BOLD_BRIGHT_GREEN}%s${RESET}\n"
          def removeWhitespace(s: String) = s.filterNot(_.isWhitespace)
          for (i <- 0 until lim) {
            val List(a, e) = AE.map(s => if (i >= s.length) "" else s(i))
            val fmt =
              if (a == e) fmtOK
              else if (removeWhitespace(a) == removeWhitespace(e)) fmtWS
              else fmtKO
            printf(fmt, i + 1, a, e)
          }
        } else {
          println(LineDiff(expect, actual).expectActualColoured)
        }
        println()
        fail("assertMultiline failed.")
      }
    }
  }

  override def tests = Tests {
    "derivativeTags" - {

      "sc2" - {
        import SampleDerivativeTags1._, Values._
        assertDerivativeTags(project, verField, virtualTagOrder)(virtualTags)
      }

      "sc4" - {
        import SampleDerivativeTags2._, Values._
        "step1" - assertDerivativeTags(step1.project, statusField, virtualTagOrder)(step1.virtualTags)
        "step2" - assertDerivativeTags(step2.project, statusField, virtualTagOrder)(step2.virtualTags)
        "step3" - assertDerivativeTags(step3.project, statusField, virtualTagOrder)(step3.virtualTags)
        "step4" - assertDerivativeTags(step4.project, statusField, virtualTagOrder)(step4.virtualTags)
        "step5" - assertDerivativeTags(step5.project, statusField, virtualTagOrder)(step5.virtualTags)
      }

      "sc6" - {
        import SampleDerivativeTags3._, Values._
        "status" - {
          "step1" - assertDerivativeTags(step1.project, statusField, statusOrder)(step1.virtualStatuses)
          "step2" - assertDerivativeTags(step2.project, statusField, statusOrder)(step2.virtualStatuses)
          "step3" - assertDerivativeTags(step3.project, statusField, statusOrder)(step3.virtualStatuses)
          "step4" - assertDerivativeTags(step4.project, statusField, statusOrder)(step4.virtualStatuses)
          "step5" - assertDerivativeTags(step5.project, statusField, statusOrder)(step5.virtualStatuses)
        }
        "ver" - {
          "step1" - assertDerivativeTags(step1.project, verField, verOrder)(step1.virtualVersions)
          "step2" - assertDerivativeTags(step2.project, verField, verOrder)(step2.virtualVersions)
          "step3" - assertDerivativeTags(step3.project, verField, verOrder)(step3.virtualVersions)
          "step4" - assertDerivativeTags(step4.project, verField, verOrder)(step4.virtualVersions)
          "step5" - assertDerivativeTags(step5.project, verField, verOrder)(step5.virtualVersions)
        }
      }

      "edge" - {
        import SampleDerivativeTags4._, Values._
        assertDerivativeTags(project, zField)(virtualTagsZ)
      }

    }
  }
}
