package shipreq.webapp.member.test.project

import shipreq.base.util.Enabled
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._

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

  import Values._
  import TestEvent._

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

  val virtualTagOrder = Vector(fr, iv, mf, fb)

  object step1 {

    val project =
      applyEventsSuccessfully(Project.empty,
        Event.FieldStaticAdd(StaticField.AllTags),

        Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "FB", Optional, ∅)),
        Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "MF", Optional, ∅)),
        Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "IV", Optional, ∅)),
        Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "FR", Optional, ∅)),

        tagGroupCreate(status),
        applicableTagCreate(needsAnalysis, "needsAnalysis", parent = status),
        applicableTagCreate(analysed,      "analysed",      parent = status),
        applicableTagCreate(readyForDev,   "readyForDev",   parent = status),
        applicableTagCreate(implemented,   "implemented",   parent = status),
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

    def virtualTags =
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-4
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: needsAnalysis (default)
        |  = {needsAnalysis?}
        |IV-3
        |  + FR-4: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
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
        |  = {needsAnalysis+}
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
        |  = {needsAnalysis+}
        |""".stripMargin
  }

  object step2 {
    val project = applyEventSuccessfully(step1.project, reqTagsPatch(iv2, rejected))
    def virtualTags =
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-4
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
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
        |  = {readyForDev+}
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
        |  = {readyForDev+}
        |""".stripMargin
  }

  object step3 {
    val project = applyEventSuccessfully(step2.project, reqTagsPatch(fr4, implemented))
    def virtualTags =
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-4
        |  + self: implemented (manual)
        |  = {implemented}
        |IV-1
        |  + FR-1: readyForDev (default)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented+} // "analysed" is here because we never remove manual values
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
        |  = {readyForDev+}
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
        |  = {readyForDev+}
        |""".stripMargin
  }

  object step4 {
    val project = applyEventSuccessfully(step3.project, reqTagsPatch(fr1, implemented))
    def virtualTags =
      """FR-1
        |  + FR-3: readyForDev (default)
        |  + self: implemented (manual)
        |  = {readyForDev+ implemented}
        |FR-2
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-3
        |  + self: readyForDev (default)
        |  = {readyForDev?}
        |FR-4
        |  + self: implemented (manual)
        |  = {implemented}
        |IV-1
        |  + FR-1: implemented (manual)
        |  + FR-1: readyForDev (derived)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + self: analysed (manual)
        |  = {analysed readyForDev+} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented+} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: implemented (manual)
        |  + FR-1: readyForDev (derived)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  = {readyForDev+}
        |FB-1
        |  + FR-1: implemented (manual)
        |  + FR-1: readyForDev (derived)
        |  + FR-2: readyForDev (default)
        |  + FR-3: readyForDev (default)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: readyForDev (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  + MF-1: readyForDev (derived)
        |  = {readyForDev+}
        |""".stripMargin
  }

  object step5 {
    val project = applyEventsSuccessfully(step4.project, reqTagsPatch(fr2, implemented), reqTagsPatch(fr3, implemented))
    def virtualTags =
      """FR-1
        |  + FR-3: implemented (manual)
        |  + self: implemented (manual)
        |  = {implemented}
        |FR-2
        |  + self: implemented (manual)
        |  = {implemented}
        |FR-3
        |  + self: implemented (manual)
        |  = {implemented}
        |FR-4
        |  + self: implemented (manual)
        |  = {implemented}
        |IV-1
        |  + FR-1: implemented (manual)
        |  + FR-2: implemented (manual)
        |  + FR-3: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented+} // "analysed" is here because we never remove manual values
        |IV-2
        |  + self: rejected (manual)
        |  = {rejected}
        |IV-3
        |  + FR-4: implemented (manual)
        |  + self: analysed (manual)
        |  = {analysed implemented+} // "analysed" is here because we never remove manual values
        |MF-1
        |  + FR-1: implemented (manual)
        |  + FR-2: implemented (manual)
        |  + FR-3: implemented (manual)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: implemented (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  = {implemented+}
        |FB-1
        |  + FR-1: implemented (manual)
        |  + FR-2: implemented (manual)
        |  + FR-3: implemented (manual)
        |  + FR-4: implemented (manual)
        |  + IV-1: analysed (manual)
        |  + IV-1: implemented (derived)
        |  + IV-2: rejected (manual)
        |  + IV-3: analysed (manual)
        |  + IV-3: implemented (derived)
        |  + MF-1: implemented (derived)
        |  = {implemented+}
        |""".stripMargin
  }

}
