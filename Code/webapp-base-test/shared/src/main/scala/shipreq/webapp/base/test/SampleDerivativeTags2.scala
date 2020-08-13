package shipreq.webapp.base.test

import shipreq.base.util.Enabled
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._

/** https://shipreq.com/project/d6My#/reqs/SC-4 */
object SampleDerivativeTags2 {

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
  }

  import TestEvent._
  import Values._

  val defaults: FieldReqTypeRules.ForTagField =
    FieldReqTypeRules.empty
      .defaultTo(needsAnalysis)(fb, mf, iv)
      .defaultTo(readyForDev)(fr)

  val derivativeTags = DerivativeTags(Enabled, Map(
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

  val step1 =
    applyEventsSuccessfully(Project.empty,

      Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "FB", Optional, ∅)),
      Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "MF", Optional, ∅)),
      Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "IV", Optional, ∅)),
      Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "FR", Optional, ∅)),

      tagGroupCreate(status),
      applicableTagCreate(needsAnalysis, "needsAnalysis", parent = status),
      applicableTagCreate(analysed,      "analysed",      parent = status),
      applicableTagCreate(implemented,   "implemented",   parent = status),
      applicableTagCreate(readyForDev,   "readyForDev",   parent = status),
      applicableTagCreate(rejected,      "rejected",      parent = status),

      fieldCustomTagCreate(statusField, status, defaults, derivativeTags),

      genericReqCreate(fb1, fb),
      genericReqCreate(mf1, mf, impSrcs = fb1),
      genericReqCreate(iv1, iv, impSrcs = mf1, tags = analysed),
      genericReqCreate(iv2, iv, impSrcs = mf1),
      genericReqCreate(iv3, iv, impSrcs = mf1, tags = analysed),
      genericReqCreate(fr1, fr, impSrcs = iv1),
      genericReqCreate(fr2, fr, impSrcs = iv1),
      genericReqCreate(fr3, fr, impSrcs = fr1),
      genericReqCreate(fr4, fr, impSrcs = iv3),
    )

  val step2 = applyEventSuccessfully(step1, reqTagsPatch(iv2, rejected))
  val step3 = applyEventSuccessfully(step2, reqTagsPatch(fr4, implemented))
  val step4 = applyEventSuccessfully(step3, reqTagsPatch(fr1, implemented))
  val step5 = applyEventsSuccessfully(step4, reqTagsPatch(fr2, implemented), reqTagsPatch(fr3, implemented))
}
