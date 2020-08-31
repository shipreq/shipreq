package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Enabled, Invalid}
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.text.PlainText
import sourcecode.Line
import utest._

object VirtualProjectTagsTest extends TestSuite {
  import VirtualProjectTags.{DerivationDesc, DerivativeTagFactor, Provenance, VirtualTag}

  private def summariseDerivativeTags(p: Project,
                                      fieldId: CustomField.Tag.Id,
                                      reqTypeOrder: Vector[ReqTypeId]): String = {
    val tags = p.virtualTags
    def req(id: ReqId) = PlainText.pubid(p.content.reqs.need(id).pubid, p)
    def tag(id: ApplicableTagId) = p.config.tags.needApplicableTag(id).name
    def resultTags(results: IterableOnce[String]) = results.iterator.mkString("{", " ", "}")

    val showProvenance: Provenance => String = {
      case Provenance.Default      => "default"
      case Provenance.Derived      => "derived"
      case Provenance.ManualTag    => "manual"
      case Provenance.ManualInText => "text"
    }

    def describeTag(t: VirtualTag): String = {
      var desc = ""
      if (t.isManualInText) desc += "#"
      if (t.isDefault) desc += "?"
      if (t.isDerived) desc += "+"
      if (t.live is Dead) desc += "-"
      if (t.validity is Invalid) desc += "!"
      desc
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

        def results(fd: FilterDead): String = {
          val v = tags(r.id, fd)
          v.ordered(fieldId.asTagFieldId)
            .iterator
            .map(t => tag(t) + describeTag(v(t, fieldId.asTagFieldId)))
            .|>(resultTags)
        }

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
    val x = p.virtualTags
    val endTime = System.nanoTime()
    val dur = Duration.ofNanos(endTime - startTime)
    if (x eq null) ???

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
                assertEqWithTolerance(desc, c.byTag.valuesIterator.sum, relevantChildren.length)
                assertEqWithTolerance(desc, c.progressBar.iterator.map(_.portion).sum, relevantChildren.length)
                assertEq(c.total, relevantChildren.length)

              case Dead =>
                assertEq(c.byTag, Map.empty: c.ByTag)
                assertEq(c.progressBar, ArraySeq.empty: c.ProgressBar)
                assertEq(c.total, 0)
            }
          }
      }
  }

  private def assertProgressBar(p     : Project,
                                f     : CustomField.Tag.Id,
                                reqId : ReqId)
                               (expect: String)(implicit l: Line): Unit = {
    val actual =
      p.virtualTags(reqId)
        .childrenSummary(f)
        .progressBar
        .iterator
        .map(p => s"(${p.portion}) ${p.desc}")
        .mkString(", ")
    assertEq(PlainText.pubidByReqId(reqId, p), actual = actual, expect = expect)
  }

  private def assertDescDerivation(p     : Project,
                                   f     : TagFieldId,
                                   reqId : ReqId,
                                   tagId : ApplicableTagId)
                                  (expect: String)(implicit l: Line): Unit = {

    def render(d: DerivationDesc): String = {
      val result = new StringBuilder
      result.append("Factors\n")
      for (f <- d.factors) {
        result.append("\n  ")
        result.append(f.tag.fold("no tag")(p.config.tags.needApplicableTag(_).key.with_#))
        result.append(" - ")
        result.append(f.reqs)
      }
      if (d.steps.nonEmpty) {
        result.append("\n\nDerivation\n")
        for (i <- d.steps.indices) {
          val s = d.steps(i)
          val sep = if (i == d.steps.indices.last) " " else " + "
          result.append("\n  = ")
          result.append(s.tags.iterator.map(p.config.tags.needApplicableTag(_).key.with_#).mkString(sep))
        }
      }
      result.toString
    }

    val norm: String => String =
      _.trim.replaceAll(" {2,}", " ")

    for (fd <- FilterDead) {

      val actual =
        p.virtualTags(reqId, fd)(tagId, f)
          .derivationDesc
          .map(render)
          .orNull

      assertMultiline(
        PlainText.pubidByReqId(reqId, p),
        actual = norm(actual),
        expect = norm(expect))
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
        "progressBar" - {
          "ver" - {
            def p = step5.project
            def f = verField
            "fr3" - assertProgressBar(p, f, fr3)("(1.0) 100% v1")
            "fr1" - assertProgressBar(p, f, fr1)("(2.0) 100% v1")
            "mf1" - {
              // - FR-1: v1
              // - FR-2: v2
              // - FR-3: v1
              // - FR-4: v1
              // - IV-1: v1,v2
              // - IV-2: v2
              // - IV-3: v1,v2
              // - MF-1: v2 <--- v1 omitted because v2 is manual and v1 is derived
              // v1 = FR1  FR3 FR4 ½IV1 ½IV3 = 4.0
              // v2 = FR2 ½IV1 IV2 ½IV3 self = 4.0
              assertProgressBar(p, f, mf1)("(4.0) 50% v1, (4.0) 50% v2")
            }
            "fb1" - {
              // - FB-1: v1,v2
              // - FR-1: v1
              // - FR-2: v2
              // - FR-3: v1
              // - FR-4: v1
              // - IV-1: v1,v2
              // - IV-2: v2
              // - IV-3: v1,v2
              // - MF-1: v1,v2
              // v1 = FR1  FR3 FR4 ½IV1 ½IV3 ½MF-1 ½self = 5
              // v2 = FR2 ½IV1 IV2 ½IV3      ½MF-1 ½self = 4
              assertProgressBar(p, f, fb1)("(5.0) 56% v1, (4.0) 44% v2")
            }
          }
          "status" - {
            "step1" - {
              def p = step1.project
              def f = statusField
              "iv2" - assertProgressBar(p, f, iv2)("(1.0) 100% needsAnalysis")
            }
            "step4" - {
              def p = step4.project
              def f = statusField
              "fr3" - assertProgressBar(p, f, fr3)("(1.0) 100% readyForDev")
              "fr1" - assertProgressBar(p, f, fr1)("(1.0) 50% readyForDev, (1.0) 50% implemented")
            }
            "step5" - {
              def p = step5.project
              def f = statusField
              "fr3" - assertProgressBar(p, f, fr3)("(1.0) 100% implemented")
              "fr1" - assertProgressBar(p, f, fr1)("(2.0) 100% implemented")
              "iv1" - assertProgressBar(p, f, iv1)("(1.0) 25% analysed, (3.0) 75% implemented")
              "mf1" - {
                // - FR-1: implemented
                // - FR-2: implemented
                // - FR-3: implemented
                // - FR-4: implemented
                // - IV-1: analysed,implemented
                // - IV-2: rejected
                // - IV-3: analysed,implemented
                // - MF-1: implemented
                assertProgressBar(p, f, mf1)("(1.0) 13% analysed, (6.0) 75% implemented, (1.0) 13% rejected")
              }
              "fb1" - {
                // - FB-1: implemented
                // - FR-1: implemented
                // - FR-2: implemented
                // - FR-3: implemented
                // - FR-4: implemented
                // - IV-1: analysed,implemented
                // - IV-2: rejected
                // - IV-3: analysed,implemented
                // - MF-1: implemented
                assertProgressBar(p, f, fb1)("(1.0) 11% analysed, (7.0) 78% implemented, (1.0) 11% rejected")
              }
            }
          }
        }
        "descDerivation" - {
          "fb1" - {
            def p = step5.project
            "implemented" - {
              // + FR-1: implemented (manual)
              // + FR-2: implemented (manual)
              // + FR-3: implemented (manual)
              // + FR-4: implemented (manual)
              // + IV-1: analysed (manual)
              // + IV-1: implemented (derived)
              // + IV-2: rejected (manual)
              // + IV-3: analysed (manual)
              // + IV-3: implemented (derived)
              // + MF-1: implemented (derived)
              def test(f: TagFieldId) =
                assertDescDerivation(p, f, fb1, implemented)(
                  """
                    |Factors
                    |
                    |  #analysed    - IV-{1,3}
                    |  #implemented - FR-{1-4},IV-{1,3},MF-1
                    |  #rejected    - IV-2
                    |
                    |Derivation
                    |
                    |  = #analysed + #implemented + #rejected
                    |  = #implemented + #rejected
                    |  = #implemented
                    |
                    |""".stripMargin)
              "status" - test(TagFieldId.Custom(statusField))
              "all" - test(TagFieldId.All)
            }
            "v1" - {
              // + FR-1: v1 (manual)
              // + FR-2: v2 (derived)
              // + FR-3: v1 (derived)
              // + FR-4: v1 (manual)
              // + IV-1: v1 (derived)
              // + IV-1: v2 (derived)
              // + IV-2: v2 (derived)
              // + IV-3: v1 (derived)
              // + IV-3: v2 (derived)
              // + MF-1: v1 (derived)
              // + MF-1: v2 (manual)
              // + self: ∅
              // = {v1+ v2+}
              def test(f: TagFieldId) =
                assertDescDerivation(p, f, fb1, v1)(
                  """
                    |Factors
                    |
                    |  no tag - FB-1
                    |  #v1    - FR-{1,3,4},IV-{1,3},MF-1
                    |  #v2    - FR-2,IV-{1-3},MF-1
                    |
                    |Derivation
                    |
                    |  = #v1 #v2
                    |
                    |""".stripMargin)
              "ver" - test(TagFieldId.Custom(verField))
              "all" - test(TagFieldId.All)
            }
          }
        }
      }

      "edge1" - {
        import SampleDerivativeTags4._, Values._
        "z" - assertDerivativeTags(project, zField)(virtualTagsZ)
        "y" - assertDerivativeTags(project, yField)(virtualTagsY)
        "x" - assertDerivativeTags(project, xField)(virtualTagsX)
        "w" - assertDerivativeTags(project, wField)(virtualTagsW)
      }

      "edge2" - {
        import SampleDerivativeTags5._, Values._
        "z" - assertDerivativeTags(project, zField)(virtualTagsZ)
        "y" - assertDerivativeTags(project, yField)(virtualTagsY)
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
