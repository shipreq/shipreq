package shipreq.webapp.member.project.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.base.util.{Enabled, NonExclusive}
import shipreq.webapp.member.project.data
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.{FieldReqTypeRules => FRTR}
import shipreq.webapp.member.project.event.ApplyEventTestFns._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.RetiredGenericData._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.SampleProject
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

object CustomFieldEventV1TestHelpers {
  val onlyUC  = onlyReqTypes(StaticReqType.UseCase)
  val onlyRT1 = onlyReqTypes(1)
  val notRT2  = notReqTypes(2)
}

object CustomFieldEventTestHelpers {
  val onlyUC      = FieldReqTypeRules.notApplicable.optional(StaticReqType.UseCase)
  val onlyRT1     = FieldReqTypeRules.notApplicable.optional(1)
  val notRT2      = FieldReqTypeRules.optional.notApplicable(2)
}

object CustomFieldEventTest extends TestSuite {
  import StaticField._

  implicit val init = InitialEvents(CustomTextFieldEventV1Test.c1, CustomTextFieldEventV1Test.c2)

  val t1 = CustomTextFieldEventV1Test.c1.id
  val t2 = CustomTextFieldEventV1Test.c2.id

  override def tests = Tests {

    "reposition" - {
      var es = Vector[Event]()
      def o = _assertPass(es: _*).config.fields.order
      assertEq(o, Vector(OtherTags, ImplicationGraph, NormalAltStepTree, ExceptionStepTree, StepGraph, t1, t2))

      es :+= FieldReposition(t1, Some(ExceptionStepTree))
      assertEq(o, Vector(OtherTags, ImplicationGraph, NormalAltStepTree, t1, ExceptionStepTree, StepGraph, t2))

      es :+= FieldReposition(ExceptionStepTree, None)
      assertEq(o, Vector(OtherTags, ImplicationGraph, NormalAltStepTree, t1, StepGraph, t2, ExceptionStepTree))
    }

    "addStaticField" - {
      "ok" - {
        for (f <- optional)
          if (FieldSet.empty.includes(f))
            assertPass(FieldStaticRemove(f), FieldStaticAdd(f))
          else
            assertPass(FieldStaticAdd(f))
      }
      "failIfExists" - assertFail("exists")(FieldStaticAdd(StepGraph))
    }

    "deleteStatic" - {
      // assertQty allows less detail here
      "ok" - {
        for (f <- optional)
          if (FieldSet.empty.includes(f))
            assertPass(FieldStaticRemove(f))
          else
            assertPass(FieldStaticAdd(f), FieldStaticRemove(f))
      }
      "twice" - {
        val d = FieldStaticRemove(StepGraph)
        assertFail("not found")(d, d)
      }
      "delAddDel" - assertPass(FieldStaticRemove(StepGraph), FieldStaticAdd(StepGraph), FieldStaticRemove(StepGraph))
    }
  }
}

// =====================================================================================================================
trait CustomTextFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomTextFieldGDv1._

  val c1Name = "Stuff"
  val c1Key = "stf"
  type CE = FieldCustomTextCreateV1
  val c1  = FieldCustomTextCreateV1(1, nev(Name(c1Name), Key(c1Key), Mandatory(true), ApplicableReqTypes(allReqTypes)))
  val c2  = FieldCustomTextCreateV1(2, nev(Name("Roar"), Key("r"), Mandatory(false), ApplicableReqTypes(onlyUC)))
  val u1  = FieldCustomTextUpdateV1(1, nev(Key("stuff")))
  val sd1 = FieldCustomDelete(1.CFText)
  val r1  = FieldCustomRestore(1.CFText)
}

object CustomTextFieldEventSharedTests extends SharedTests()(NoInitialEvents.init) with CustomTextFieldEventsV1 {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTextFieldEventV1Test extends TestSuite with CustomTextFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomTextFieldGDv1._
  import NoInitialEvents._

  implicit class FieldCustomTextCreateExt(private val a: FieldCustomTextCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needName"      - assertFail("Name")     (c1.mod(_ - Name))
      "needKey"       - assertFail("Key")      (c1.mod(_ - Key))
      "needMandatory" - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      "needReqTypes"  - assertFail("Types")    (c1.mod(_ - ApplicableReqTypes))
      "badName"       - assertFail("blank")    (c1.mod(_ + Name("")))
    //'badKey        - assertFail("Key")      (c1.mod(_ + Key("?")))
      "badReqTypes"   - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(onlyRT1))) // RT1 doesn't exist
      "dupName"       - assertFail("unique")   (c1, c2.mod(_ + Name(c1Name)))
    //'dupKey        - assertFail("unique")   (c1, c2.mod(_ + Key(c1Key)))
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Text.v1(1, c1Name, "stuff", true, allReqTypes, Live))

        es :+= FieldCustomTextUpdateV1(1, nev(Name("AH"), Mandatory(false)))
        assertEq(r, CustomField.Text.v1(1, "AH", "stuff", false, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTextUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Text.v1(1, "AH", "stuff", false, onlyRT1, Live))

      }
      "badName"     - assertFail("blank") (c1, FieldCustomTextUpdateV1(1, nev(Name(""))))
    //'badKey      - assertFail("Key")   (c1, FieldCustomTextUpdateV1(1, nev(Key("?"))))
      "badReqTypes" - assertFail("Types") (c1, FieldCustomTextUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      "dupName"     - assertFail("unique")(c1, c2, FieldCustomTextUpdateV1(2, nev(Name(c1Name))))
    //'dupKey      - assertFail("unique")(c1, c2, FieldCustomTextUpdateV1(2, nev(Key(c1Key))))
    }
  }
}

trait CustomTextFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomTextFieldGD._

  val c1Name = "Stuff"
  type CE = FieldCustomTextCreate
  val c1  = FieldCustomTextCreate(1, nev(Name(c1Name), FieldReqTypeRules(FRTR.mandatory)))
  val c2  = FieldCustomTextCreate(2, nev(Name("Roar"), FieldReqTypeRules(onlyUC)))
  val u1  = FieldCustomTextUpdate(1, nev(FieldReqTypeRules(FRTR.optional)))
  val sd1 = FieldCustomDelete(1.CFText)
  val r1  = FieldCustomRestore(1.CFText)
}

object CustomTextFieldEventTest extends TestSuite with CustomTextFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomTextFieldGD._
  import NoInitialEvents._

  implicit class FieldCustomTextCreateExt(private val a: FieldCustomTextCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needName"    - assertFail("Name")     (c1.mod(_ - Name))
      "needRules"   - assertFail("rules")    (c1.mod(_ - FieldReqTypeRules))
      "badName"     - assertFail("blank")    (c1.mod(_ + Name("")))
      "badReqTypes" - assertFail("Types")    (c1.mod(_ + FieldReqTypeRules(onlyRT1))) // RT1 doesn't exist
      "dupName"     - assertFail("unique")   (c1, c2.mod(_ + Name(c1Name)))
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Text(1, c1Name, FRTR.optional, Live))

        es :+= FieldCustomTextUpdate(1, nev(Name("AH"), FieldReqTypeRules(onlyUC)))
        assertEq(r, CustomField.Text(1, "AH", onlyUC, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTextUpdate(1, nev(FieldReqTypeRules(onlyRT1)))
        assertEq(r, CustomField.Text(1, "AH", onlyRT1, Live))

      }
      "badName"     - assertFail("blank") (c1, FieldCustomTextUpdate(1, nev(Name(""))))
      "badReqTypes" - assertFail("Types") (c1, FieldCustomTextUpdate(1, nev(FieldReqTypeRules(onlyRT1)))) // RT1 doesn't exist
      "dupName"     - assertFail("unique")(c1, c2, FieldCustomTextUpdate(2, nev(Name(c1Name))))
    }
  }
}

// =====================================================================================================================
trait CustomTagFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomTagFieldGDv1._

  def mkC1(tagId: TagGroupId) = FieldCustomTagCreateV1(1, nev(TagId(tagId), Mandatory(true), ApplicableReqTypes(allReqTypes)))

  type CE = FieldCustomTagCreateV1
  val c1  = mkC1(1.TG)
  val c2  = FieldCustomTagCreateV1(2, nev(TagId(2.TG), Mandatory(false), ApplicableReqTypes(onlyUC)))
  val u1  = FieldCustomTagUpdateV1(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFTag)
  val r1  = FieldCustomRestore(1.CFTag)
}

object CustomTagFieldEventSharedTests extends SharedTests()(CustomTagFieldEventV1Test.init) with CustomTagFieldEventsV1 {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTagFieldEventV1Test extends TestSuite with CustomTagFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomTagFieldGDv1._

  val createTG2 = TagGroupCreate(2, TagGroupGD("c2", Some("r"), NonExclusive, ∅, ∅))
  val softDelTG1 = TagGroupEventTest.sd1
  implicit val init = InitialEvents(TagGroupEventTest.c1, createTG2)

  implicit class FieldCustomTagCreateExt(private val a: FieldCustomTagCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needTagId"     - assertFail("TagId")    (c1.mod(_ - TagId))
      "needMandatory" - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      "needReqTypes"  - assertFail("Types")    (c1.mod(_ - ApplicableReqTypes))
      "tagIdNotFound" - assertFail("Tag")      (c1.mod(_ + TagId(9.TG)))
      "tagIdDead"     - assertFail("dead")     (softDelTG1, c1)
      "badReqTypes"   - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(onlyRT1))) // RT1 doesn't exist
      "dupTagId"      - assertFail("unique")   (c1, c2.mod(_ + TagId(1.TG)))
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Tag.v1(1, 1.TG, false, allReqTypes, Live))

        es :+= FieldCustomTagUpdateV1(1, nev(TagId(2.TG), Mandatory(true)))
        assertEq(r, CustomField.Tag.v1(1, 2.TG, true, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTagUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Tag.v1(1, 2.TG, true, onlyRT1, Live))
      }
      "tagIdNotFound" - assertFail("Tag")   (c1, FieldCustomTagUpdateV1(1, nev(TagId(9.TG))))
      "badReqTypes"   - assertFail("Types") (c1, FieldCustomTagUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      "dupTagId"      - assertFail("unique")(c1, c2, FieldCustomTagUpdateV1(2, nev(TagId(1.TG))))
    }
  }
}

trait CustomTagFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomTagFieldGD._

  def mkC1(tagId: TagGroupId) = FieldCustomTagCreate(1, tagId, nev(FieldReqTypeRules(FRTR.mandatory)))

  type CE = FieldCustomTagCreate
  val c1  = mkC1(1.TG)
  val c2  = FieldCustomTagCreate(2, 2.TG, nev(FieldReqTypeRules(onlyUC)))
  val u1  = FieldCustomTagUpdate(1, nev(FieldReqTypeRules(FRTR.optional)))
  val sd1 = FieldCustomDelete(1.CFTag)
  val r1  = FieldCustomRestore(1.CFTag)
}

object CustomTagFieldEventTest extends TestSuite with CustomTagFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomTagFieldGD._

  val createAT2 = {
    import ApplicableTagGD._
    ApplicableTagCreate(2, nev(Key("c2"), Desc(Some("r")), Colour(None), ApplicableReqTypes(allReqTypes)))
  }
  val createAT3 = {
    import ApplicableTagGD._
    ApplicableTagCreate(3, nev(Key("c3"), Desc(Some("r")), Colour(None), ApplicableReqTypes(allReqTypes)))
  }
  val softDelTG1 = TagGroupEventTest.sd1
  implicit val init = InitialEvents(TagGroupEventTest.c1, createAT2)

  implicit class FieldCustomTagCreateExt(private val a: FieldCustomTagCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needRules"     - assertFail("rules")    (c1.mod(_ - FieldReqTypeRules))
      "tagIdNotFound" - assertFail("Tag")      (c1.copy(tagId = 9.TG))
      "tagIdDead"     - assertFail("dead")     (softDelTG1, c1)
      "badReqTypes"   - assertFail("Types")    (c1.mod(_ + FieldReqTypeRules(onlyRT1))) // RT1 doesn't exist
      "dupTagId"      - assertFail("unique")   (c1, c2.copy(tagId = 1.TG))

      "defaults" - {
        import SampleProject.Values._
        val p0 = Project.fields.replace(FieldSet.empty)(SampleProject.project)

        def assertBad(parent: TagGroupId, id: ApplicableTagId) = {
          val c = FieldCustomTagCreate(1, parent, nev(FieldReqTypeRules(FRTR.defaultTo(id))))
          assertEventFails(p0, c)
        }

        "ok" - {
          val c = FieldCustomTagCreate(1, priTG, nev(FieldReqTypeRules(FRTR.defaultTo(priHigh))))
          val p = applyEventSuccessfully(p0, c)
          val f = p.config.fields.customFields.need(c.id)
          assertEq(f, CustomField.Tag.v2(c.id, priTG, FRTR.defaultTo(priHigh), Live))
        }

        "notFound"  - assertBad(priTG, 1234.AT)
//        'dead      - assertBad(statusTG, uat2)
//        'notAChild - assertBad(statusTG, priHigh)
      }

      "derivativeTags" - {
        import data.DerivativeTags.TagPair
        implicit val init = CustomTagFieldEventTest.init.add(createAT3)

        "ok" - {
          import SampleProject.Values._
          val r  = FRTR.defaultTo(priHigh)
          val d  = data.DerivativeTags(Enabled, Map(TagPair(v10, v11) -> v12))
          val c  = FieldCustomTagCreate(1, priTG, nev(FieldReqTypeRules(r), DerivativeTags(d)))
          val p0 = Project.fields.replace(FieldSet.empty)(SampleProject.project)
          val p  = applyEventSuccessfully(p0, c)
          val f  = p.config.fields.customFields.need(c.id)
          assertEq(f, CustomField.Tag(c.id, priTG, r, d, Live))
        }

        "badTagId1" - assertFail("not found")(c1.mod(_ + DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(9.AT, 2.AT) -> 2.AT)))))
        "badTagId2" - assertFail("not found")(c1.mod(_ + DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(2.AT, 9.AT) -> 2.AT)))))
        "badTagId3" - assertFail("not found")(c1.mod(_ + DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(2.AT, 3.AT) -> 9.AT)))))
      }
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Tag.v2(1, 1.TG, FRTR.optional, Live))

        es :+= FieldCustomTagUpdate(1, nev(FieldReqTypeRules(FRTR.mandatory)))
        assertEq(r, CustomField.Tag.v2(1, 1.TG, FRTR.mandatory, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTagUpdate(1, nev(FieldReqTypeRules(onlyRT1)))
        assertEq(r, CustomField.Tag.v2(1, 1.TG, onlyRT1, Live))
      }
      "badReqTypes" - assertFail("Types") (c1, FieldCustomTagUpdate(1, nev(FieldReqTypeRules(onlyRT1)))) // RT1 doesn't exist

      "defaults" - {
        import SampleProject.Values._
        val id = 1
        val p0 = applyEventsSuccessfully(
                   Project.fields.replace(FieldSet.empty)(SampleProject.project),
                   FieldCustomTagCreate(id, statusTG, nev(FieldReqTypeRules(FRTR.optional))))

        def assertBad(default: ApplicableTagId) = {
          val u = FieldCustomTagUpdate(id, nev(FieldReqTypeRules(FRTR.defaultTo(default))))
          assertEventFails(p0, u)
        }

        "ok" - {
          val u = FieldCustomTagUpdate(id, nev(FieldReqTypeRules(FRTR.defaultTo(wip))))
          val p = applyEventSuccessfully(p0, u)
          val f = p.config.fields.customFields.need(u.id)
          assertEq(f, CustomField.Tag.v2(id, statusTG, FRTR.defaultTo(wip), Live))
        }

        "notFound"  - assertBad(1234.AT)
//        'dead      - assertBad(uat2)
//        'notAChild - assertBad(priHigh)
      }

      "derivativeTags" - {
        import data.DerivativeTags.TagPair
        implicit val init = CustomTagFieldEventTest.init.add(createAT3)

        "ok" - {
          import SampleProject.Values._
          val d  = data.DerivativeTags(Enabled, Map(TagPair(v10, v11) -> v12))
          val u  = FieldCustomTagUpdate(1, nev(DerivativeTags(d)))
          val p0 = Project.fields.replace(FieldSet.empty)(SampleProject.project)
          val p  = applyEventsSuccessfully(p0, c1, u)
          val f  = p.config.fields.customFields.need(u.id)
          assertEq(f, CustomField.Tag(u.id, priTG, FRTR.mandatory, d, Live))
        }

        "badTagId1" - assertFail("not found")(c1, FieldCustomTagUpdate(1, nev(DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(9.AT, 2.AT) -> 2.AT))))))
        "badTagId2" - assertFail("not found")(c1, FieldCustomTagUpdate(1, nev(DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(2.AT, 9.AT) -> 2.AT))))))
        "badTagId3" - assertFail("not found")(c1, FieldCustomTagUpdate(1, nev(DerivativeTags(data.DerivativeTags(Enabled, Map(TagPair(2.AT, 3.AT) -> 9.AT))))))
      }
    }
  }
}

// =====================================================================================================================
trait CustomImpFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomImpFieldGDv1._

  type CE = FieldCustomImpCreateV1
  val c1  = FieldCustomImpCreateV1(1, nev(ReqTypeId(1), Mandatory(true), ApplicableReqTypes(onlyUC)))
  val c2  = FieldCustomImpCreateV1(2, nev(ReqTypeId(StaticReqType.UseCase), Mandatory(false), ApplicableReqTypes(allReqTypes)))
  val u1  = FieldCustomImpUpdateV1(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFImp)
  val r1  = FieldCustomRestore(1.CFImp)
}

object CustomImpFieldEventSharedTests extends SharedTests()(CustomImpFieldEventV1Test.init) with CustomImpFieldEventsV1 {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomImpFieldEventV1Test extends TestSuite with CustomImpFieldEventsV1 {
  import CustomFieldEventV1TestHelpers._
  import CustomImpFieldGDv1._

  implicit val init = InitialEvents(CustomReqTypeEventTest.c1, CustomReqTypeEventTest.gr1)

  val softDelRT1 = CustomReqTypeEventTest.sd1

  implicit class FieldCustomImpCreateExt(private val a: FieldCustomImpCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needReqTypeId"     - assertFail("ReqTypeId")(c1.mod(_- ReqTypeId))
      "needMandatory"     - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      "needReqTypes"      - assertFail("Types")    (c1.mod(_- ApplicableReqTypes))
      "reqTypeIdNotFound" - assertFail("ReqType")  (c1.mod(_ + ReqTypeId(9)))
      "reqTypeIdDead"     - assertFail("dead")     (softDelRT1, c1)
      "badReqTypes"       - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(notRT2))) // RT2 doesn't exist
      "dupReqTypeId"      - assertFail("unique")   (c1, c2.mod(_ + ReqTypeId(1)))
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Implication.v1(1, 1, false, onlyUC, Live))

        es :+= CustomReqTypeEventTest.c2
        es :+= FieldCustomImpUpdateV1(1, nev(ApplicableReqTypes(notRT2)))
        assertEq(r, CustomField.Implication.v1(1, 1, false, notRT2, Live))
      }
      "reqTypeIdNotFound" - assertFail("Imp")   (c1, FieldCustomImpUpdateV1(1, nev(ReqTypeId(9))))
      "badReqTypes"       - assertFail("Types") (c1, FieldCustomImpUpdateV1(1, nev(ApplicableReqTypes(notRT2)))) // RT2 doesn't exist
      "dupReqTypeId"      - assertFail("unique")(c1, c2, FieldCustomImpUpdateV1(2, nev(ReqTypeId(1))))
    }
  }
}

trait CustomImpFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomImpFieldGD._

  type CE = FieldCustomImpCreate
  val c1  = FieldCustomImpCreate(1, 1, nev(FieldReqTypeRules(onlyUC)))
  val c2  = FieldCustomImpCreate(2, StaticReqType.UseCase, nev(FieldReqTypeRules(FRTR.optional)))
  val u1  = FieldCustomImpUpdate(1, nev(FieldReqTypeRules(FRTR.mandatory)))
  val sd1 = FieldCustomDelete(1.CFImp)
  val r1  = FieldCustomRestore(1.CFImp)
}

object CustomImpFieldEventTest extends TestSuite with CustomImpFieldEvents {
  import CustomFieldEventTestHelpers._
  import CustomImpFieldGD._

  implicit val init = InitialEvents(CustomReqTypeEventTest.c1, CustomReqTypeEventTest.gr1)

  val softDelRT1 = CustomReqTypeEventTest.sd1

  implicit class FieldCustomImpCreateExt(private val a: FieldCustomImpCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needRules"         - assertFail("rules")    (c1.mod(_- FieldReqTypeRules))
      "reqTypeIdNotFound" - assertFail("ReqType")  (c1.copy(reqTypeId = 9))
      "reqTypeIdDead"     - assertFail("dead")     (softDelRT1, c1)
      "badReqTypes"       - assertFail("Types")    (c1.mod(_ + FieldReqTypeRules(notRT2))) // RT2 doesn't exist
      "dupReqTypeId"      - assertFail("unique")   (c1, c2.copy(reqTypeId = 1))
    }

    "update" - {
      "ok" - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Implication(1, 1, FRTR.mandatory, Live))

        es :+= CustomReqTypeEventTest.c2
        es :+= FieldCustomImpUpdate(1, nev(FieldReqTypeRules(notRT2)))
        assertEq(r, CustomField.Implication(1, 1, notRT2, Live))
      }
      "badReqTypes" - assertFail("Types") (c1, FieldCustomImpUpdate(1, nev(FieldReqTypeRules(notRT2)))) // RT2 doesn't exist
    }
  }
}
