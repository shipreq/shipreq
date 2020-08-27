package shipreq.webapp.base.test

import shipreq.base.util.{Disabled, Enabled}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._

/** This is a collection of edge cases.
  *
  * A1 (default) -> B1 (empty)
  *
  * B2 (empty) -> A2 (default)
  *
  * A3 (default) -> C1 (N/A) -> B3 (empty)
  *
  * B4 (empty) -> C2 (N/A) -> A4 (default)
  *
  * A5 (manual) -> A6 (dead) -> A7 (default)
  *
  * A8 (default)
  *
  * A5 (manual) -> A9 (dead) -> A10 (2 manuals) -> A11 (default)
  *
  * B5 (empty) -> B6 (empty)
  *
  * A12 (dead->default) -> B7 (empty)
  *
  * B8 (empty) -> A13 (dead->default)
  *
  * Diamond:
  * B9 (empty) -> A14 (default) -> B10 (empty)
  *            -> A15 (manual)  ->
  *
  * Different roots:
  * A16 (manual) -> A17 (dead) -> A18 (manual) -> B11 (empty)
  *                               A19 (manual) ->
  *
  * Bad DT rules:
  * C3 (manual) -> C4 (manual) [DT -> dead]
  * C5 (manual) -> C6 (manual) [DT -> N/A]
  *
  * Bad RT rules:
  * D1 (default:dead)
  * E1 (default:external)
  * D2 (default:dead) -> E2 (default:external)
  * B12 (manual) -> D3 (default:dead) -> E3 (default:external) -> B13 (manual)
  *
  * A20 (text) -> B14 (empty)
  *
  * C7 (text:N/A) -> B15 (empty)
  *
  * Derivative tag field is dead (xField)
  *
  * Derivative tag field is disabled (wField)
  */
object SampleDerivativeTags4 {

  object Values {
    val a = CustomReqTypeId(1)
    val b = CustomReqTypeId(2)
    val c = CustomReqTypeId(3)
    val d = CustomReqTypeId(4)
    val e = CustomReqTypeId(5)

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
    val a11 = GenericReqId(111)
    val a12 = GenericReqId(112)
    val a13 = GenericReqId(113)
    val a14 = GenericReqId(114)
    val a15 = GenericReqId(115)
    val a16 = GenericReqId(116)
    val a17 = GenericReqId(117)
    val a18 = GenericReqId(118)
    val a19 = GenericReqId(119)
    val a20 = GenericReqId(120)

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
    val b12 = GenericReqId(212)
    val b13 = GenericReqId(213)
    val b14 = GenericReqId(214)
    val b15 = GenericReqId(215)

    val c1 = GenericReqId(301)
    val c2 = GenericReqId(302)
    val c3 = GenericReqId(303)
    val c4 = GenericReqId(304)
    val c5 = GenericReqId(305)
    val c6 = GenericReqId(306)
    val c7 = GenericReqId(307)

    val d1 = GenericReqId(401)
    val d2 = GenericReqId(402)
    val d3 = GenericReqId(403)

    val e1 = GenericReqId(501)
    val e2 = GenericReqId(502)
    val e3 = GenericReqId(503)

    val zField = CustomField.Tag.Id(1)
    val z      = TagGroupId(10)
    val z1     = ApplicableTagId(11)
    val z2     = ApplicableTagId(12)
    val z3     = ApplicableTagId(13)
    val z4     = ApplicableTagId(14) // (DEAD)

    // y1 + y2 = y3 (DEAD)
    // y1 + y4 = z1 (out of scope)
    val yField = CustomField.Tag.Id(2)
    val y      = TagGroupId(20)
    val y1     = ApplicableTagId(21)
    val y2     = ApplicableTagId(22)
    val y3     = ApplicableTagId(23) // (DEAD)
    val y4     = ApplicableTagId(24)
    val y5     = ApplicableTagId(25) // (DEAD)

    val xField = CustomField.Tag.Id(3) // (DEAD)
    val x      = TagGroupId(30)
    val x1     = ApplicableTagId(31)
    val x2     = ApplicableTagId(32)

    val wField = CustomField.Tag.Id(4)
    val w      = TagGroupId(40)
    val w1     = ApplicableTagId(41)
    val w2     = ApplicableTagId(42)
  }

  import TestEvent._
  import Values._

  val zRules = FieldReqTypeRules.empty
      .defaultTo(z1)(a)
      .notApplicable(c)

  val zDerivativeTags = DerivativeTags(Enabled, Map.empty)

  val yRules =
    FieldReqTypeRules.empty
      .defaultTo(y5)(d)
      .defaultTo(z1)(e)

  val yDerivativeTags = DerivativeTags(Enabled, Map(
    (y1, y2) -> y3,
    (y1, y4) -> z1,
  ))

  val xRules = FieldReqTypeRules.defaultTo(x1)

  val xDerivativeTags = DerivativeTags(Enabled, Map(
    (x1, x2) -> x1,
  ))

  val wRules = FieldReqTypeRules.defaultTo(w1)

  val wDerivativeTags = DerivativeTags(Disabled, Map(
    (w1, w2) -> w1,
  ))

  val project = applyEventsSuccessfully(Project.empty,
    Event.FieldStaticAdd(StaticField.AllTags),

    Event.CustomReqTypeCreate(a, CustomReqTypeGD("A", "A", Optional, ∅)),
    Event.CustomReqTypeCreate(b, CustomReqTypeGD("B", "B", Optional, ∅)),
    Event.CustomReqTypeCreate(c, CustomReqTypeGD("C", "C", Optional, ∅)),
    Event.CustomReqTypeCreate(d, CustomReqTypeGD("D", "D", Optional, ∅)),
    Event.CustomReqTypeCreate(e, CustomReqTypeGD("E", "E", Optional, ∅)),

    tagGroupCreate(z, "Z"),
    tagGroupCreate(y, "Y"),
    tagGroupCreate(x, "X"),
    tagGroupCreate(w, "W"),

    applicableTagCreate(y1, "y1", parent = y),
    applicableTagCreate(y2, "y2", parent = y),
    applicableTagCreate(y3, "y3", parent = y),
    applicableTagCreate(y4, "y4", parent = y),
    applicableTagCreate(y5, "y5", parent = y),
    applicableTagCreate(z1, "z1", parent = z),
    fieldCustomTagCreate(yField, y, yRules, yDerivativeTags),

    applicableTagUpdate(z1, parent = z),
    applicableTagCreate(z2, "z2", parent = z),
    applicableTagCreate(z3, "z3", parent = z),
    applicableTagCreate(z4, "z4", parent = z),
    fieldCustomTagCreate(zField, z, zRules, zDerivativeTags),

    applicableTagCreate(x1, "x1", parent = x),
    applicableTagCreate(x2, "x2", parent = x),
    fieldCustomTagCreate(xField, x, xRules, xDerivativeTags),
    Event.FieldCustomDelete(xField),

    applicableTagCreate(w1, "w1", parent = w),
    applicableTagCreate(w2, "w2", parent = w),
    fieldCustomTagCreate(wField, w, wRules, wDerivativeTags),

    // a1 (default) -> b1 (empty)
    genericReqCreate(a1, a),
    genericReqCreate(b1, b, impSrcs = a1),

    // b2 (empty) -> a2 (default)
    genericReqCreate(b2, b),
    genericReqCreate(a2, a, impSrcs = b2),

    // a3 (default) -> c1 (N/A) -> b3 (empty)
    genericReqCreate(a3, a),
    genericReqCreate(c1, c, impSrcs = a3),
    genericReqCreate(b3, b, impSrcs = c1),

    // b4 (empty) -> c2 (N/A) -> a4 (default)
    genericReqCreate(b4, b),
    genericReqCreate(c2, c, impSrcs = b4),
    genericReqCreate(a4, a, impSrcs = c2),

    // A5 (manual) -> A6 (dead) -> A7 (default)
    genericReqCreate(a5, a, tags = z2),
    genericReqCreate(a6, a, impSrcs = a5),
    genericReqCreate(a7, a, impSrcs = a6),
    reqsDelete(a6),

    // A8 (default)
    genericReqCreate(a8, a),

    // A5 (manual) -> A9 (dead) -> A10 (2 manuals) -> A11 (default)
    genericReqCreate(a9, a, impSrcs = a5),
    genericReqCreate(a10, a, impSrcs = a9, tags = Set(z2, z3)),
    genericReqCreate(a11, a, impSrcs = a10),
    reqsDelete(a9),

    // B5 (empty) -> B6 (empty)
    genericReqCreate(b5, b),
    genericReqCreate(b6, b, impSrcs = b5),

    // A12 (dead->default) -> B7 (empty)
    genericReqCreate(a12, a, tags = z4),
    genericReqCreate(b7, b, impSrcs = a12),

    // B8 (empty) -> A13 (dead->default)
    genericReqCreate(b8, b),
    genericReqCreate(a13, a, impSrcs = b8, tags = z4),

    // Diamond:
    // B9 (empty) -> A14 (default) -> B10 (empty)
    //            -> A15 (manual)  ->
    genericReqCreate(b9, b),
    genericReqCreate(b10, b),
    genericReqCreate(a14, a, impSrcs = b9, impTgts = b10),
    genericReqCreate(a15, a, impSrcs = b9, impTgts = b10, tags = z3),

    // Different roots:
    // A16 (manual) -> A17 (dead) -> A18 (manual) -> B11 (empty)
    //                               A19 (manual) ->
    genericReqCreate(a16, a, tags = z2),
    genericReqCreate(a17, a, tags = z2, impSrcs = a16),
    genericReqCreate(a18, a, tags = z3, impSrcs = a17),
    genericReqCreate(a19, a, tags = z1),
    genericReqCreate(b11, b, impSrcs = Set(a18, a19)),
    reqsDelete(a17),

    // Bad DT rules:
    // C3 (manual) -> C4 (manual) [DT -> dead]
    // C5 (manual) -> C6 (manual) [DT -> N/A]
    genericReqCreate(c3, c, tags = y1),
    genericReqCreate(c4, c, tags = y2, impSrcs = c3),
    genericReqCreate(c5, c, tags = y1),
    genericReqCreate(c6, c, tags = y4, impSrcs = c5),

    // D1 (default:dead)
    // E1 (default:external)
    // D2 (default:dead) -> E2 (default:external)
    // B12 (manual) -> D3 (default:dead) -> E3 (default:external) -> B13 (manual)
    genericReqCreate(b12, b, tags = y1),
    genericReqCreate(b13, b, tags = y2),
    genericReqCreate(d1, d),
    genericReqCreate(d2, d),
    genericReqCreate(d3, d, impSrcs = b12),
    genericReqCreate(e1, e),
    genericReqCreate(e2, e),
    genericReqCreate(e3, e, impSrcs = d3, impTgts = b13),

    // A20 (text) -> B14 (empty)
    genericReqCreate(a20, a, titleTagRef = z1),
    genericReqCreate(b14, b, impSrcs = a20),

    // C7 (text:N/A) -> B15 (empty)
    genericReqCreate(c7, c, titleTagRef = z1),
    genericReqCreate(b15, b, impSrcs = c7),

    // Delete tags
    Event.TagDelete(z4),
    Event.TagDelete(y3),
    Event.TagDelete(y5),
  )

  def virtualTagsZ =
    """A-1
      |  + B-1: z1 (derived)
      |  + self: z1 (default)
      |  = {z1?}
      |A-2
      |  + self: z1 (default)
      |  = {z1?}
      |A-3
      |  + B-3: z1 (derived)
      |  + self: z1 (default)
      |  = {z1?}
      |A-4
      |  + self: z1 (default)
      |  = {z1?}
      |A-5
      |  + self: z2 (manual)
      |  = {z2}
      |A-6 = {} / {z1?-}
      |A-7
      |  + self: z1 (default)
      |  = {z1?}
      |A-8
      |  // + self: z1 (default) // omitted because derivative tags not applied to this isolated req
      |  = {z1?}
      |A-9 = {} / {z1?-}
      |A-10
      |  + A-11: z2 (derived)
      |  + A-11: z3 (derived)
      |  + self: z2 (manual)
      |  + self: z3 (manual)
      |  = {z2 z3}
      |A-11
      |  + A-10: z2 (manual)
      |  + A-10: z3 (manual)
      |  = {z2+ z3+}
      |A-12
      |  + B-7: z1 (derived)
      |  + self: z1 (default)
      |  = {z1?}
      |    {z1? z4-} (ShowDead)
      |A-13
      |  + self: z1 (default)
      |  = {z1?}
      |    {z1? z4-} (ShowDead)
      |A-14
      |  + B-10: z1 (derived)
      |  + B-10: z3 (derived)
      |  = {z1+ z3+}
      |A-15
      |  + B-10: z1 (derived)
      |  + B-10: z3 (derived)
      |  + self: z3 (manual)
      |  = {z1+ z3}
      |A-16
      |  + self: z2 (manual)
      |  = {z2}
      |A-17 = {} / {z2}
      |A-18
      |  + B-11: z1 (derived)
      |  + B-11: z3 (derived)
      |  + self: z3 (manual)
      |  = {z1+ z3}
      |A-19
      |  + B-11: z1 (derived)
      |  + B-11: z3 (derived)
      |  + self: z1 (manual)
      |  = {z1 z3+}
      |A-20
      |  + B-14: z1 (derived)
      |  + self: z1 (text)
      |  = {z1#}
      |B-1
      |  + A-1: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-2
      |  + A-2: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-3
      |  + A-3: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-4
      |  + A-4: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-5
      |  + B-6: ∅
      |  + self: ∅
      |  = {}
      |B-6
      |  + self: ∅
      |  = {}
      |B-7
      |  + A-12: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-8
      |  + A-13: z1 (default)
      |  + self: ∅
      |  = {z1+}
      |B-9
      |  + A-14: z1 (derived)
      |  + A-14: z3 (derived)
      |  + A-15: z1 (derived)
      |  + A-15: z3 (manual)
      |  + B-10: z1 (derived)
      |  + B-10: z3 (derived)
      |  + self: ∅
      |  = {z1+ z3+}
      |B-10
      |  + A-14: z1 (default)
      |  + A-15: z3 (manual)
      |  + self: ∅
      |  = {z1+ z3+}
      |B-11
      |  + A-18: z3 (manual)
      |  + A-19: z1 (manual)
      |  + self: ∅
      |  = {z1+ z3+}
      |B-12
      |  + B-13: ∅
      |  + D-3: ∅
      |  + E-3: ∅
      |  + self: ∅
      |  = {}
      |B-13
      |  + self: ∅
      |  = {}
      |B-14
      |  + A-20: z1 (text)
      |  + self: ∅
      |  = {z1+}
      |B-15
      |  + self: ∅
      |  = {}
      |C-1 = {}
      |C-2 = {}
      |C-3 = {}
      |C-4 = {}
      |C-5 = {}
      |C-6 = {}
      |C-7
      |  = {z1#!}
      |D-1 = {}
      |D-2 = {}
      |D-3
      |  + B-13: ∅
      |  + E-3: ∅
      |  + self: ∅
      |  = {}
      |E-1 = {}
      |E-2 = {}
      |E-3
      |  + B-13: ∅
      |  + self: ∅
      |  = {}
      |""".stripMargin

  def virtualTagsY =
    """A-1
      |  + B-1: ∅
      |  + self: ∅
      |  = {}
      |A-2
      |  + self: ∅
      |  = {}
      |A-3
      |  + B-3: ∅
      |  + C-1: ∅
      |  + self: ∅
      |  = {}
      |A-4
      |  + self: ∅
      |  = {}
      |A-5
      |  + self: ∅
      |  = {}
      |A-6
      |  = {}
      |A-7
      |  + self: ∅
      |  = {}
      |A-8
      |  = {}
      |A-9
      |  = {}
      |A-10
      |  + A-11: ∅
      |  + self: ∅
      |  = {}
      |A-11
      |  + self: ∅
      |  = {}
      |A-12
      |  + B-7: ∅
      |  + self: ∅
      |  = {}
      |A-13
      |  + self: ∅
      |  = {}
      |A-14
      |  + B-10: ∅
      |  + self: ∅
      |  = {}
      |A-15
      |  + B-10: ∅
      |  + self: ∅
      |  = {}
      |A-16
      |  + self: ∅
      |  = {}
      |A-17
      |  = {}
      |A-18
      |  + B-11: ∅
      |  + self: ∅
      |  = {}
      |A-19
      |  + B-11: ∅
      |  + self: ∅
      |  = {}
      |A-20
      |  + B-14: ∅
      |  + self: ∅
      |  = {}
      |B-1
      |  + self: ∅
      |  = {}
      |B-2
      |  + A-2: ∅
      |  + self: ∅
      |  = {}
      |B-3
      |  + self: ∅
      |  = {}
      |B-4
      |  + A-4: ∅
      |  + C-2: ∅
      |  + self: ∅
      |  = {}
      |B-5
      |  + B-6: ∅
      |  + self: ∅
      |  = {}
      |B-6
      |  + self: ∅
      |  = {}
      |B-7
      |  + self: ∅
      |  = {}
      |B-8
      |  + A-13: ∅
      |  + self: ∅
      |  = {}
      |B-9
      |  + A-14: ∅
      |  + A-15: ∅
      |  + B-10: ∅
      |  + self: ∅
      |  = {}
      |B-10
      |  + self: ∅
      |  = {}
      |B-11
      |  + self: ∅
      |  = {}
      |B-12
      |  + B-13: y2 (manual)
      |  + D-3: y1 (derived)
      |  + D-3: y2 (derived)
      |  + E-3: y1 (derived)
      |  + E-3: y2 (derived)
      |  + self: y1 (manual)
      |  = {y1 y2+}
      |    {y1 y2+ y3+-} (ShowDead)
      |B-13
      |  + self: y2 (manual)
      |  = {y2}
      |B-14
      |  + self: ∅
      |  = {}
      |B-15
      |  + self: ∅
      |  = {}
      |C-1
      |  + B-3: ∅
      |  + self: ∅
      |  = {}
      |C-2
      |  + A-4: ∅
      |  + self: ∅
      |  = {}
      |C-3
      |  + C-4: y2 (manual)
      |  + self: y1 (manual)
      |  = {y1 y2+}
      |    {y1 y2+ y3+-} (ShowDead)
      |C-4
      |  + self: y2 (manual)
      |  = {y2}
      |C-5
      |  + C-6: y4 (manual)
      |  + self: y1 (manual)
      |  = {y1 y4+}
      |C-6
      |  + self: y4 (manual)
      |  = {y4}
      |C-7
      |  + B-15: ∅
      |  + self: ∅
      |  = {}
      |D-1
      |  = {}
      |    {y5?-} (ShowDead)
      |D-2
      |  = {}
      |    {y5?-} (ShowDead)
      |D-3
      |  + B-12: y1 (manual)
      |  + B-13: y2 (manual)
      |  + E-3: y1 (derived)
      |  + E-3: y2 (derived)
      |  + self: ∅
      |  = {y1+ y2+}
      |    {y1+ y2+ y3+- y5?-} (ShowDead)
      |E-1 = {}
      |E-2 = {}
      |E-3
      |  + B-12: y1 (manual)
      |  + B-13: y2 (manual)
      |  + self: ∅
      |  = {y1+ y2+}
      |    {y1+ y2+ y3+-} (ShowDead)
      |""".stripMargin

  def virtualTagsX =
    """A-1 = {} / {x1?-}
      |A-2 = {} / {x1?-}
      |A-3 = {} / {x1?-}
      |A-4 = {} / {x1?-}
      |A-5 = {} / {x1?-}
      |A-6 = {} / {x1?-}
      |A-7 = {} / {x1?-}
      |A-8 = {} / {x1?-}
      |A-9 = {} / {x1?-}
      |A-10 = {} / {x1?-}
      |A-11 = {} / {x1?-}
      |A-12 = {} / {x1?-}
      |A-13 = {} / {x1?-}
      |A-14 = {} / {x1?-}
      |A-15 = {} / {x1?-}
      |A-16 = {} / {x1?-}
      |A-17 = {} / {x1?-}
      |A-18 = {} / {x1?-}
      |A-19 = {} / {x1?-}
      |A-20 = {} / {x1?-}
      |B-1 = {} / {x1?-}
      |B-2 = {} / {x1?-}
      |B-3 = {} / {x1?-}
      |B-4 = {} / {x1?-}
      |B-5 = {} / {x1?-}
      |B-6 = {} / {x1?-}
      |B-7 = {} / {x1?-}
      |B-8 = {} / {x1?-}
      |B-9 = {} / {x1?-}
      |B-10 = {} / {x1?-}
      |B-11 = {} / {x1?-}
      |B-12 = {} / {x1?-}
      |B-13 = {} / {x1?-}
      |B-14 = {} / {x1?-}
      |B-15 = {} / {x1?-}
      |C-1 = {} / {x1?-}
      |C-2 = {} / {x1?-}
      |C-3 = {} / {x1?-}
      |C-4 = {} / {x1?-}
      |C-5 = {} / {x1?-}
      |C-6 = {} / {x1?-}
      |C-7 = {} / {x1?-}
      |D-1 = {} / {x1?-}
      |D-2 = {} / {x1?-}
      |D-3 = {} / {x1?-}
      |E-1 = {} / {x1?-}
      |E-2 = {} / {x1?-}
      |E-3 = {} / {x1?-}
      |""".stripMargin

  def virtualTagsW =
    """A-1 = {w1?}
      |A-2 = {w1?}
      |A-3 = {w1?}
      |A-4 = {w1?}
      |A-5 = {w1?}
      |A-6 = {} / {w1?-}
      |A-7 = {w1?}
      |A-8 = {w1?}
      |A-9 = {} / {w1?-}
      |A-10 = {w1?}
      |A-11 = {w1?}
      |A-12 = {w1?}
      |A-13 = {w1?}
      |A-14 = {w1?}
      |A-15 = {w1?}
      |A-16 = {w1?}
      |A-17 = {} / {w1?-}
      |A-18 = {w1?}
      |A-19 = {w1?}
      |A-20 = {w1?}
      |B-1 = {w1?}
      |B-2 = {w1?}
      |B-3 = {w1?}
      |B-4 = {w1?}
      |B-5 = {w1?}
      |B-6 = {w1?}
      |B-7 = {w1?}
      |B-8 = {w1?}
      |B-9 = {w1?}
      |B-10 = {w1?}
      |B-11 = {w1?}
      |B-12 = {w1?}
      |B-13 = {w1?}
      |B-14 = {w1?}
      |B-15 = {w1?}
      |C-1 = {w1?}
      |C-2 = {w1?}
      |C-3 = {w1?}
      |C-4 = {w1?}
      |C-5 = {w1?}
      |C-6 = {w1?}
      |C-7 = {w1?}
      |D-1 = {w1?}
      |D-2 = {w1?}
      |D-3 = {w1?}
      |E-1 = {w1?}
      |E-2 = {w1?}
      |E-3 = {w1?}
      |""".stripMargin

}
