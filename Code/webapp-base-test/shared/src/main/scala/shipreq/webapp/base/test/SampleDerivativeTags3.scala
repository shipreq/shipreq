package shipreq.webapp.base.test

import shipreq.base.util.Enabled
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._

/** https://shipreq.com/project/d6My#/reqs/SC-6
  *
  * This is a combination of
  *   - [[SampleDerivativeTags1]] / https://shipreq.com/project/d6My#/reqs/SC-2
  *   - [[SampleDerivativeTags2]] / https://shipreq.com/project/d6My#/reqs/SC-4
  */
object SampleDerivativeTags3 {

  object Values {
    val fb = CustomReqTypeId(1)
    val mf = CustomReqTypeId(2)
    val iv = CustomReqTypeId(3)
    val fr = CustomReqTypeId(4)

    val fb1 = GenericReqId(11)
    val mf1 = GenericReqId(21)
    val iv1 = GenericReqId(31)
    val iv2 = GenericReqId(32)
    val iv3 = GenericReqId(33)
    val fr1 = GenericReqId(41)
    val fr2 = GenericReqId(42)
    val fr3 = GenericReqId(43)
    val fr4 = GenericReqId(44)

    val statusField   = CustomField.Tag.Id(1)
    val status        = TagGroupId(100)
    val needsAnalysis = ApplicableTagId(1)
    val analysed      = ApplicableTagId(2)
    val readyForDev   = ApplicableTagId(3)
    val implemented   = ApplicableTagId(4)
    val rejected      = ApplicableTagId(5)

    val verField = CustomField.Tag.Id(2)
    val ver = TagGroupId(200)
    val v1 = ApplicableTagId(10)
    val v2 = ApplicableTagId(20)
  }

  import TestEvent._
  import Values._

  val statusFieldDefaults: FieldReqTypeRules.ForTagField =
    FieldReqTypeRules.empty
      .defaultTo(needsAnalysis)(fb, mf, iv)
      .defaultTo(readyForDev)(fr)

  val statusFieldDerivativeTags = DerivativeTags(Enabled, Map(
    (analysed     , implemented  ) -> implemented,
    (analysed     , needsAnalysis) -> needsAnalysis,
    (analysed     , readyForDev  ) -> readyForDev,
    (analysed     , rejected     ) -> analysed,
    (implemented  , needsAnalysis) -> needsAnalysis,
    (implemented  , readyForDev  ) -> readyForDev,
    (implemented  , rejected     ) -> implemented,
    (needsAnalysis, readyForDev  ) -> needsAnalysis,
    (needsAnalysis, rejected     ) -> needsAnalysis,
    (readyForDev  , rejected     ) -> readyForDev,
  ))

  def verOrder = SampleDerivativeTags1.virtualTagOrder
  def statusOrder = SampleDerivativeTags2.virtualTagOrder

  object step1 {
    val project =
      applyEventsSuccessfully(Project.empty,
        Event.FieldStaticAdd(StaticField.AllTags),

        Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "FB", Optional, ∅)),
        Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "MF", Optional, ∅)),
        Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "IV", Optional, ∅)),
        Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "FR", Optional, ∅)),

        tagGroupCreate(status, "Status"),
        applicableTagCreate(needsAnalysis, "needsAnalysis", parent = status),
        applicableTagCreate(analysed, "analysed", parent = status),
        applicableTagCreate(implemented, "implemented", parent = status),
        applicableTagCreate(readyForDev, "readyForDev", parent = status),
        applicableTagCreate(rejected, "rejected", parent = status),
        fieldCustomTagCreate(statusField, status, statusFieldDefaults, statusFieldDerivativeTags),

        tagGroupCreate(ver, "Version"),
        applicableTagCreate(v1, "v1", parent = ver),
        applicableTagCreate(v2, "v2", parent = ver),
        fieldCustomTagCreate(verField, ver, deriv = DerivativeTags.emptyDisabled.copy(enabled = Enabled)),

        genericReqCreate(fb1, fb),
        genericReqCreate(mf1, mf, impSrcs = fb1, tags = v2),
        genericReqCreate(iv1, iv, impSrcs = mf1, tags = analysed),
        genericReqCreate(iv2, iv, impSrcs = mf1),
        genericReqCreate(iv3, iv, impSrcs = mf1, tags = analysed),
        genericReqCreate(fr1, fr, impSrcs = iv1, tags = v1),
        genericReqCreate(fr2, fr, impSrcs = iv1),
        genericReqCreate(fr3, fr, impSrcs = fr1),
        genericReqCreate(fr4, fr, impSrcs = iv3, tags = v1),
      )

    def virtualStatuses = SampleDerivativeTags2.step1.virtualTags
    def virtualVersions =
      """FR-1
        |  + FR-3: v1 (derived)
        |  + self: v1 (manual)
        |  = {v1}
        |FR-2
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v2+}
        |FR-3
        |  + FR-1: v1 (manual)
        |  + self: ∅
        |  = {v1+}
        |FR-4
        |  + self: v1 (manual)
        |  = {v1}
        |IV-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + FR-3: v1 (derived)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1+ v2+}
        |IV-2
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v2+}
        |IV-3
        |  + FR-4: v1 (manual)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1+ v2+}
        |MF-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + FR-3: v1 (derived)
        |  + FR-4: v1 (manual)
        |  + IV-1: v1 (derived)
        |  + IV-1: v2 (derived)
        |  + IV-2: v2 (derived)
        |  + IV-3: v1 (derived)
        |  + IV-3: v2 (derived)
        |  + self: v2 (manual)
        |  = {v1+ v2}
        |FB-1
        |  + FR-1: v1 (manual)
        |  + FR-2: v2 (derived)
        |  + FR-3: v1 (derived)
        |  + FR-4: v1 (manual)
        |  + IV-1: v1 (derived)
        |  + IV-1: v2 (derived)
        |  + IV-2: v2 (derived)
        |  + IV-3: v1 (derived)
        |  + IV-3: v2 (derived)
        |  + MF-1: v1 (derived)
        |  + MF-1: v2 (manual)
        |  + self: ∅
        |  = {v1+ v2+}
        |""".stripMargin
  }

  object step2 {
    val project = applyEventSuccessfully(step1.project, reqTagsPatch(iv2, rejected))
    def virtualStatuses = SampleDerivativeTags2.step2.virtualTags
    def virtualVersions = step1.virtualVersions
  }

  object step3 {
    val project = applyEventSuccessfully(step2.project, reqTagsPatch(fr4, implemented))
    def virtualStatuses = SampleDerivativeTags2.step3.virtualTags
    def virtualVersions = step1.virtualVersions
  }

  object step4 {
    val project = applyEventSuccessfully(step3.project, reqTagsPatch(fr1, implemented))
    def virtualStatuses = SampleDerivativeTags2.step4.virtualTags
    def virtualVersions = step1.virtualVersions
  }

  object step5 {
    val project = applyEventsSuccessfully(step4.project, reqTagsPatch(fr2, implemented), reqTagsPatch(fr3, implemented))
    def virtualStatuses = SampleDerivativeTags2.step5.virtualTags
    def virtualVersions = step1.virtualVersions
  }

}
