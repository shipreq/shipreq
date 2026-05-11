package shipreq.webapp.member.test.project

import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._

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
  *
  * B7 (tag:N/A + ok) mustn't derive
  * B8 (text:N/A + ok) mustn't derive
  *
  * B10 (empty) -> A9 (N/A for B)
  * A10 (N/A for B) -> B11 (empty)
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
    val a9 = GenericReqId(109)
    val a10 = GenericReqId(110)

    val b1 = GenericReqId(201)
    val b2 = GenericReqId(202)
    val b3 = GenericReqId(203)
    val b4 = GenericReqId(204)
    val b5 = GenericReqId(205)
    val b6 = GenericReqId(206)
    val b7 = GenericReqId(207)
    val b8 = GenericReqId(208)
    val b9 = GenericReqId(209)
    val b10 = GenericReqId(210)
    val b11 = GenericReqId(211)

    val c1 = GenericReqId(301)
    val c2 = GenericReqId(302)

    val zField = CustomField.Tag.Id(1)
    val z      = TagGroupId(10)
    val z1     = ApplicableTagId(11)
    val z2     = ApplicableTagId(12)
    val z3     = ApplicableTagId(13) // N/A to Bs
    val z4     = ApplicableTagId(14)

    val yField = CustomField.Tag.Id(2)
    val y      = TagGroupId(20)
    val y1     = ApplicableTagId(21)

    val tField = CustomField.Text.Id(3) // N/A to Bs
    val tFieldRules = FieldReqTypeRules.empty.notApplicable(b)

    val dField = CustomField.Text.Id(4) // DEAD
  }

  import Values._
  import TestEvent._

  val zRules = FieldReqTypeRules.empty.notApplicable(c)
  val zDerivativeTags = DerivativeTags(Enabled, Map(
    (z1, z3) -> z4,
  ))

  val yRules = FieldReqTypeRules.empty
  val yDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val project = applyEventsSuccessfully(emptyProject1,
    Event.FieldStaticAdd(StaticField.AllTags),

    Event.CustomReqTypeCreate(a, CustomReqTypeGD("A", "A", Optional, ∅)),
    Event.CustomReqTypeCreate(b, CustomReqTypeGD("B", "B", Optional, ∅)),
    Event.CustomReqTypeCreate(c, CustomReqTypeGD("C", "C", Optional, ∅)),

    tagGroupCreate(z, "Z", exclusivity = Exclusive),
    applicableTagCreate(z1, "z1", parent = z),
    applicableTagCreate(z2, "z2", parent = z),
    applicableTagCreate(z3, "z3", parent = z, applicableReqTypes = ApplicableReqTypes.blacklist(b)),
    applicableTagCreate(z4, "z4", parent = z),
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

    // B7 (tag:N/A + ok) mustn't derive
    // B8 (text:N/A + ok) mustn't derive
    genericReqCreate(b7, b, tags = Set(z1, z3)),
    genericReqCreate(b8, b, tags = z1, titleTagRef = z3),
    genericReqCreate(b9, b, impSrcs = Set(b7, b8)),

    // B10 (empty) -> A9 (N/A for B)
    genericReqCreate(b10, b),
    genericReqCreate(a9, a, impSrcs = b10, tags = z3),

    // A10 (N/A for B) -> B11 (empty)
    genericReqCreate(a10, a, tags = z3),
    genericReqCreate(b11, b, impSrcs = a10),

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
      |A-9
      |  + self: z3 (manual)
      |  = {z3}
      |A-10
      |  + B-11: ∅
      |  + self: z3 (manual)
      |  = {z3}
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
      |B-7
      |  + B-9: z1 (derived)
      |  + self: z1 (manual)
      |  = {z1}
      |    {z1 z3!} (ShowDead)
      |B-8
      |  + B-9: z1 (derived)
      |  + self: z1 (manual)
      |  = {z1 z3#!}
      |B-9
      |  + B-7: z1 (manual)
      |  + B-8: z1 (manual)
      |  + self: ∅
      |  = {z1+}
      |B-10
      |  + A-9: z3 (manual)
      |  + self: ∅
      |  = {}
      |B-11
      |  + A-10: z3 (manual)
      |  + self: ∅
      |  = {}
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
      |A-9
      |  + self: ∅
      |  = {}
      |A-10
      |  + B-11: ∅
      |  + self: ∅
      |  = {}
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
      |B-7
      |  + B-9: ∅
      |  + self: ∅
      |  = {}
      |B-8
      |  + B-9: ∅
      |  + self: ∅
      |  = {}
      |B-9
      |  + self: ∅
      |  = {}
      |B-10
      |  + A-9: ∅
      |  + self: ∅
      |  = {}
      |B-11
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
