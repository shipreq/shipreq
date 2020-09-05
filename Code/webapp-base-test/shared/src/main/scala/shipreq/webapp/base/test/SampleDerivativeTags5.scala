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
  *
  * {B3 (manual), B6 (empty)} -> C1 (N/A) -> C2 (N/A) -> {B4 (manual), B5 (empty)}
  */
object SampleDerivativeTags5 {

  object Values {
    val a = CustomReqTypeId(1)
    val b = CustomReqTypeId(2)
    val c = CustomReqTypeId(3)

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
    val b3 = GenericReqId(203)
    val b4 = GenericReqId(204)
    val b5 = GenericReqId(205)
    val b6 = GenericReqId(206)

    val c1 = GenericReqId(301)
    val c2 = GenericReqId(302)

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

  val zRules = FieldReqTypeRules.empty.notApplicable(c)
  val zDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val yRules = FieldReqTypeRules.empty
  val yDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val project = applyEventsSuccessfully(Project.empty,
    Event.FieldStaticAdd(StaticField.AllTags),

    Event.CustomReqTypeCreate(a, CustomReqTypeGD("A", "A", Optional, ∅)),
    Event.CustomReqTypeCreate(b, CustomReqTypeGD("B", "B", Optional, ∅)),
    Event.CustomReqTypeCreate(c, CustomReqTypeGD("C", "C", Optional, ∅)),

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

    // {B3 (manual), B6 (empty)} -> C1 (N/A) -> C2 (N/A) -> {B4 (manual), B5 (empty)}
    genericReqCreate(b3, b, tags = z1),
    genericReqCreate(c1, c, impSrcs = b3),
    genericReqCreate(c2, c, impSrcs = c1),
    genericReqCreate(b4, b, impSrcs = c2, tags = z2),
    genericReqCreate(b5, b, impSrcs = c2),
    genericReqCreate(b6, b, impTgts = c1),

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
      |B-3
      |  + B-4: z2 (manual)
      |  + B-5: z1 (derived)
      |  + self: z1 (manual)
      |  = {z1 z2+}
      |B-4
      |  + self: z2 (manual)
      |  = {z2}
      |B-5
      |  + B-3: z1 (manual)
      |  + self: ∅
      |  = {z1+}
      |B-6
      |  + B-4: z2 (manual)
      |  + B-5: z1 (derived)
      |  + self: ∅
      |  = {z1+ z2+}
      |C-1 = {}
      |C-2 = {}
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
      |B-3
      |  + B-4: ∅
      |  + B-5: ∅
      |  + C-1: ∅
      |  + C-2: ∅
      |  + self: ∅
      |  = {}
      |B-4
      |  + self: ∅
      |  = {}
      |B-5
      |  + self: ∅
      |  = {}
      |B-6
      |  + B-4: ∅
      |  + B-5: ∅
      |  + C-1: ∅
      |  + C-2: ∅
      |  + self: ∅
      |  = {}
      |C-1
      |  + B-4: ∅
      |  + B-5: ∅
      |  + C-2: ∅
      |  + self: ∅
      |  = {}
      |C-2
      |  + B-4: ∅
      |  + B-5: ∅
      |  + self: ∅
      |  = {}
      |""".stripMargin
}
