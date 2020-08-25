package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.Enabled
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.PlainText
import sourcecode.Line
import utest._

object VirtualProjectTagsTest extends TestSuite {
  import VirtualProjectTags.{DerivativeTagFactor, TagProvenance}

  private def summariseDerivativeTags(p: Project,
                                      fieldId: CustomField.Tag.Id,
                                      reqTypeOrder: Vector[ReqTypeId]): String = {
    val tags = p.virtualTags
    def req(id: ReqId) = PlainText.pubid(p.content.reqs.need(id).pubid, p)
    def tag(id: ApplicableTagId) = p.config.tags.needApplicableTag(id).name
    def resultTags(results: IterableOnce[String]) = results.iterator.mkString("{", " ", "}")

    val showProvenance: TagProvenance => String = {
      case TagProvenance.Default => "default"
      case TagProvenance.Derived => "derived"
      case TagProvenance.Manual  => "manual"
    }

    val provenanceSuffix: TagProvenance => String = {
      case TagProvenance.Default => "?"
      case TagProvenance.Derived => "+"
      case TagProvenance.Manual  => ""
    }

    val showFactor: DerivativeTagFactor => String = {

      case DerivativeTagFactor.EmptySelf =>
        "self: ∅"

      case DerivativeTagFactor.EmptyRelation(src, _) =>
        req(src) + ": ∅"

      case DerivativeTagFactor.Self(t, srcType) =>
        s"self: ${tag(t)} (${showProvenance(srcType)})"

      case DerivativeTagFactor.Relation(r, _, t, srcType) =>
        s"${req(r)}: ${tag(t)} (${showProvenance(srcType)})"
    }

    type Item = ((Int, Int), String)

    def unorderedReqTypes =
      p.config.reqTypes.allSortedByMnemonic.iterator.map(_.reqTypeId).filterNot(reqTypeOrder.contains)

    val reqTypeOrderMap =
      (reqTypeOrder ++ unorderedReqTypes).mapToOrder

    def perReq: Iterator[Item] = {
      p.content.reqs.reqIterator().map { r =>
        val pubid = req(r.id)
        val mono = tags(r.id)

        def results(fd: FilterDead): String =
          tags(r.id, fd)
            .fieldOrdered(fieldId)
            .iterator
            .map(t => tag(t) + provenanceSuffix(mono.provenance(fieldId)(t)))
            .|>(resultTags)

        val deadResults =
          results(ShowDead)

        val liveResults =
          if (r.live(p.config.reqTypes) is Dead)
            resultTags(Nil)
          else
            results(HideDead)

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

  private def assertDerivativeTags(p0: Project,
                                   f: CustomField.Tag.Id,
                                   reqTypeOrder: Vector[ReqTypeId] = Vector.empty,
                                   alwaysSimplifyFirst: Boolean = false)(_expect: String)(implicit l: Line): String = {

    val p = p0.copy() // avoid pre-computed virtualTags so that we can measure derivation time
    val startTime = System.nanoTime()
    p.virtualTags
    val endTime = System.nanoTime()
    val dur = Duration.ofNanos(endTime - startTime)

    val actual = summariseDerivativeTags(p, f, reqTypeOrder)

    val expect = _expect
      .trim
      .replaceAll(" *//.+?(\n|$)", "$1") // remove comments
      .replaceAll("(^|\n) *\n", "$1") // remove blank links
      .replaceAll("(\\d) * =", "$1\n  =") // expand concise no-factor lines
      .replaceAll("(\\{.*?\\}) / (\\{.*?\\})", "$1\n    $2 (ShowDead)") // expand multi-result lines

    if (actual != expect) {

      def simplify(s: String): String =
        s.linesIterator
          .filterNot(_.startsWith("  +"))
          .filterNot(_.contains("ShowDead"))
          .mkString("\n")
          .replace("\n  =", " =")

      if (expect.isEmpty)
        println(simplify(actual))

      //println(actual)
      if (alwaysSimplifyFirst || !(expect.contains("  +") || expect.contains("ShowDead")))
        assertMultiline(simplify(actual), simplify(expect))
      assertMultiline(actual, expect)
    }

    "Derivation took " + dur.conciseDesc
  }

  private def assertProps(p: Project)(implicit l: Line): Unit = {
    val fields   = p.config.liveCustomTagFields.filter(_.derivativeTags.enabled is Enabled)
    val reqTypes = p.config.reqTypes
    val tags     = p.virtualTags

    if (fields.nonEmpty)
      for (req <- p.content.reqs.reqIterator()) {
        val reqTags = tags(req.id)
        val reqLive = req.live(reqTypes)

        val reqChildren: ArraySeq[Req] =
          reqLive match {
            case Live =>
              (p.implicationSrcToTgtTC(req.id) - req.id).iterator
                .map(p.content.reqs.need)
                .filter(_.live(reqTypes) is Live)
                .to(ArraySeq)
            case Dead =>
              ArraySeq.empty
          }

        for (f <- fields)
          if (!f.fieldReqTypeRules(req.reqTypeId).isNA) {
            def desc = s"${PlainText.pubid(req.pubid, p)} ${p.config.tags.needTagGroup(f.tagId).name}"
            val c = reqTags.childrenSummary(f.id)
            reqLive match {
              case Live =>
                val relevantChildren = reqChildren.filterNot(r => f.fieldReqTypeRules(r.reqTypeId).isNA)
                assertEqWithTolerance(desc, c.aggregated.valuesIterator.sum, relevantChildren.length)
                assertEq(c.total, relevantChildren.length)

              case Dead =>
                assertEq(c.aggregated, Map.empty[Option[ApplicableTagId], Double])
                assertEq(c.total, 0)
            }
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
        "z" - assertDerivativeTags(project, zField)(virtualTagsZ)
        "y" - assertDerivativeTags(project, yField)(virtualTagsY)
        "x" - assertDerivativeTags(project, xField)(virtualTagsX)
        "w" - assertDerivativeTags(project, wField)(virtualTagsW)
      }
    }

    "childrenSummary" - {
      "props" - {
        "sdt1"   - assertProps(SampleDerivativeTags1.project)
        "sdt2-1" - assertProps(SampleDerivativeTags2.step1.project)
        "sdt2-2" - assertProps(SampleDerivativeTags2.step2.project)
        "sdt2-3" - assertProps(SampleDerivativeTags2.step3.project)
        "sdt2-4" - assertProps(SampleDerivativeTags2.step4.project)
        "sdt2-5" - assertProps(SampleDerivativeTags2.step5.project)
        "sdt3-1" - assertProps(SampleDerivativeTags3.step1.project)
        "sdt3-2" - assertProps(SampleDerivativeTags3.step2.project)
        "sdt3-3" - assertProps(SampleDerivativeTags3.step3.project)
        "sdt3-4" - assertProps(SampleDerivativeTags3.step4.project)
        "sdt3-5" - assertProps(SampleDerivativeTags3.step5.project)
        "sdt4"   - assertProps(SampleDerivativeTags4.project)
        "random" - RandomData.project.samples().take(3).foreach(assertProps)
      }
    }
  }
}
