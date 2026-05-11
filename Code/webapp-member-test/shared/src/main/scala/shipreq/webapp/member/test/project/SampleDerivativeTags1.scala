package shipreq.webapp.member.test.project

import shipreq.base.util.Enabled
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._

/** https://shipreq.com/project/d6My#/reqs/SC-2 */
object SampleDerivativeTags1 {

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

    val verField = CustomField.Tag.Id(1)
    val ver = TagGroupId(100)
    val v1 = ApplicableTagId(1)
    val v2 = ApplicableTagId(2)
  }

  import Values._
  import TestEvent._

  val project =
    applyEventsSuccessfully(emptyProject1,
      Event.FieldStaticAdd(StaticField.AllTags),

      Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "FB", Optional, ∅)),
      Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "MF", Optional, ∅)),
      Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "IV", Optional, ∅)),
      Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "FR", Optional, ∅)),

      tagGroupCreate(ver, "Version"),
      applicableTagCreate(v1, "v1", parent = ver),
      applicableTagCreate(v2, "v2", parent = ver),

      fieldCustomTagCreate(verField, ver, deriv = DerivativeTags.emptyDisabled.copy(enabled = Enabled)),

      genericReqCreate(fb1, fb),
      genericReqCreate(mf1, mf, impSrcs = fb1, tags = v2),
      genericReqCreate(iv1, iv, impSrcs = mf1),
      genericReqCreate(iv2, iv, impSrcs = mf1),
      genericReqCreate(iv3, iv, impSrcs = mf1),
      genericReqCreate(fr1, fr, impSrcs = iv1, tags = v1),
      genericReqCreate(fr2, fr, impSrcs = iv1),
      genericReqCreate(fr3, fr, impSrcs = iv2),
      genericReqCreate(fr4, fr, impSrcs = iv3, tags = v1),
    )

  val virtualTagOrder = Vector(fr, iv, mf, fb)

  def virtualTags =
    """FR-1
      |  + self: v1 (manual)
      |  = {v1}
      |FR-2
      |  + MF-1: v2 (manual)
      |  + self: ∅
      |  = {v2+}
      |FR-3
      |  + MF-1: v2 (manual)
      |  + self: ∅
      |  = {v2+}
      |FR-4
      |  + self: v1 (manual)
      |  = {v1}
      |IV-1
      |  + FR-1: v1 (manual)
      |  + FR-2: v2 (derived)
      |  + MF-1: v2 (manual)
      |  + self: ∅
      |  = {v1+ v2+}
      |IV-2
      |  + FR-3: v2 (derived)
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
      |  + FR-3: v2 (derived)
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
      |  = {v1+ v2+}
      |""".stripMargin

}
