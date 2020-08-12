package shipreq.webapp.base.test

import shipreq.base.util.Enabled
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._

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

  import TestEvent._
  import Values._

  val project =
    applyEventsSuccessfully(Project.empty,

      Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "FB", Optional, ∅)),
      Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "MF", Optional, ∅)),
      Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "IV", Optional, ∅)),
      Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "FR", Optional, ∅)),

      tagGroupCreate(ver),
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
}
