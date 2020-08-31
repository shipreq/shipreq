package shipreq.webapp.base.test

import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.text.Text

/** This is another collection of edge cases.
  *
  * A1 (conflicting in tags) -> A2 (empty)
  *
  * A3 (conflicting in title text & tags) -> A4 (empty)
  *
  * A5 (conflicting in live custom text field & tags) -> A6 (empty)
  *
  * A7 (conflicting in dead custom text field & tags) -> A8 (empty)
  *
  * B1 (conflicting in N/A custom text field & tags) -> B2 (empty)
  */
object SampleDerivativeTags5 {

  object Values {
    val a = CustomReqTypeId(1)
    val b = CustomReqTypeId(2)

    val a1 = GenericReqId(101)
    val a2 = GenericReqId(102)
    val a3 = GenericReqId(103)
    val a4 = GenericReqId(104)
    val a5 = GenericReqId(105)
    val a6 = GenericReqId(106)
    val a7 = GenericReqId(107)
    val a8 = GenericReqId(108)

    val b1 = GenericReqId(201)
    val b2 = GenericReqId(202)

    val zField = CustomField.Tag.Id(1)
    val z      = TagGroupId(10)
    val z1     = ApplicableTagId(11)
    val z2     = ApplicableTagId(12)

    val yField = CustomField.Tag.Id(2)
    val y      = TagGroupId(20)
    val y1     = ApplicableTagId(21)

    val tField = CustomField.Text.Id(3) // N/A to Bs
    val tFieldRules = FieldReqTypeRules.empty.notApplicable(b)

    val dField = CustomField.Text.Id(4) // DEAD
  }

  import TestEvent._
  import Values._

  val zRules = FieldReqTypeRules.empty
  val zDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val yRules = FieldReqTypeRules.empty
  val yDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val project = applyEventsSuccessfully(Project.empty,
    Event.FieldStaticAdd(StaticField.AllTags),

    Event.CustomReqTypeCreate(a, CustomReqTypeGD("A", "A", Optional, ∅)),
    Event.CustomReqTypeCreate(b, CustomReqTypeGD("B", "B", Optional, ∅)),

    tagGroupCreate(z, "Z", exclusivity = Exclusive),
    applicableTagCreate(z1, "z1", parent = z),
    applicableTagCreate(z2, "z2", parent = z),
    fieldCustomTagCreate(zField, z, zRules, zDerivativeTags),

    tagGroupCreate(y, "Y"),
    applicableTagCreate(y1, "y1", parent = y),
    fieldCustomTagCreate(yField, y, yRules, yDerivativeTags),

    fieldCustomTextCreate(tField, "T", tFieldRules),
    fieldCustomTextCreate(dField, "D"),

    // A1 (conflicting in tags) -> A2 (empty)
    genericReqCreate(a1, a, tags = Set(z1, z2, y1)),
    genericReqCreate(a2, a, impSrcs = a1),

    // A3 (conflicting in text & tags) -> A4 (empty)
    genericReqCreate(a3, a, titleTagRef = z1, tags = Set(z2, y1)),
    genericReqCreate(a4, a, impSrcs = a3),

    // A5 (conflicting in live custom text field & tags) -> A6 (empty)
    genericReqCreate(a5, a, tags = Set(z1, y1)),
    genericReqCreate(a6, a, impSrcs = a5),
    Event.ReqFieldCustomTextSet(a5, tField, ArraySeq1(Text.CustomTextField.TagRef(z2))),

    // A7 (conflicting in dead custom text field & tags) -> A8 (empty)
    genericReqCreate(a7, a, tags = Set(z1, y1)),
    genericReqCreate(a8, a, impSrcs = a7),
    Event.ReqFieldCustomTextSet(a7, dField, ArraySeq1(Text.CustomTextField.TagRef(z2))),

    // B1 (conflicting in N/A custom text field & tags) -> B2 (empty)
    genericReqCreate(b1, b, tags = Set(z1, y1)),
    genericReqCreate(b2, b, impSrcs = b1),
    Event.ReqFieldCustomTextSet(b1, tField, ArraySeq1(Text.CustomTextField.TagRef(z2))),

    // delete stuff
    Event.FieldCustomDelete(dField),
  )

  def virtualTagsZ =
    """A-1
      |  + A-2: ∅
      |  + self: z1 (manual)
      |  + self: z2 (manual)
      |  = {z1! z2!}
      |A-2
      |  + self: ∅
      |  = {}
      |A-3
      |  + A-4: ∅
      |  + self: z1 (text)
      |  + self: z2 (manual)
      |  = {z1#! z2!}
      |A-4
      |  + self: ∅
      |  = {}
      |A-5
      |  + A-6: ∅
      |  + self: z1 (manual)
      |  + self: z2 (text)
      |  = {z1! z2#!}
      |A-6
      |  + self: ∅
      |  = {}
      |A-7
      |  + A-8: z1 (derived)
      |  + self: z1 (manual)
      |  = {z1}
      |    {z1 z2#-} (ShowDead)
      |A-8
      |  + A-7: z1 (manual)
      |  + self: ∅
      |  = {z1+}
      |B-1
      |  + B-2: z1 (derived)
      |  + self: z1 (manual)
      |  = {z1}
      |    {z1 z2#-} (ShowDead)
      |B-2
      |  + B-1: z1 (manual)
      |  + self: ∅
      |  = {z1+}
      |""".stripMargin

  def virtualTagsY =
    """A-1
      |  + A-2: y1 (derived)
      |  + self: y1 (manual)
      |  = {y1}
      |A-2
      |  + A-1: y1 (manual)
      |  + self: ∅
      |  = {y1+}
      |A-3
      |  + A-4: y1 (derived)
      |  + self: y1 (manual)
      |  = {y1}
      |A-4
      |  + A-3: y1 (manual)
      |  + self: ∅
      |  = {y1+}
      |A-5
      |  + A-6: y1 (derived)
      |  + self: y1 (manual)
      |  = {y1}
      |A-6
      |  + A-5: y1 (manual)
      |  + self: ∅
      |  = {y1+}
      |A-7
      |  + A-8: y1 (derived)
      |  + self: y1 (manual)
      |  = {y1}
      |A-8
      |  + A-7: y1 (manual)
      |  + self: ∅
      |  = {y1+}
      |B-1
      |  + B-2: y1 (derived)
      |  + self: y1 (manual)
      |  = {y1}
      |B-2
      |  + B-1: y1 (manual)
      |  + self: ∅
      |  = {y1+}
      |""".stripMargin
}
