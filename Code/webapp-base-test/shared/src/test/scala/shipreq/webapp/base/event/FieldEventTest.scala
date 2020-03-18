package shipreq.webapp.base.event

import utest._
import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import CustomFieldEventTestHelpers._
import Event._
import RetiredGenericData._

object CustomFieldEventTestHelpers {
  val onlyUC = onlyReqTypes(StaticReqType.UseCase)
  val onlyRT1 = onlyReqTypes(1)
  val notRT2 = notReqTypes(2)
}

object CustomFieldEventTest extends TestSuite {
  import StaticField._

  implicit val init = InitialEvents(CustomTextFieldEventTestV1.c1, CustomTextFieldEventTestV1.c2)

  val t1 = CustomTextFieldEventTestV1.c1.id
  val t2 = CustomTextFieldEventTestV1.c2.id

  override def tests = Tests {

    'reposition {
      var es = Vector[Event]()
      def o = _assertPass(es: _*).config.fields.order
      assertEq(o, Vector(ImplicationGraph, NormalAltStepTree, ExceptionStepTree, StepGraph, t1, t2))

      es :+= FieldReposition(t1, Some(ExceptionStepTree))
      assertEq(o, Vector(ImplicationGraph, NormalAltStepTree, t1, ExceptionStepTree, StepGraph, t2))

      es :+= FieldReposition(ExceptionStepTree, None)
      assertEq(o, Vector(ImplicationGraph, NormalAltStepTree, t1, StepGraph, t2, ExceptionStepTree))
    }

    'addStaticField {
      'ok - assertPass(FieldStaticRemove(StepGraph), FieldStaticAdd(StepGraph))
      'failIfExists     - assertFail("exists")(FieldStaticAdd(StepGraph))
      'failIfNotDelable - assertFail("exists")(FieldStaticAdd(NormalAltStepTree))
    }

    'deleteStatic {
      // assertQty allows less detail here
      'ok - {
        for (f <- values.whole.filter(_.deletable is Deletable))
          assertPass(FieldStaticRemove(f))
      }
      'undeletable - {
        for (f <- values.whole.filterNot(_.deletable is Deletable))
          assertFail("delet")(FieldStaticRemove(f))
      }
      'twice - {
        val d = FieldStaticRemove(StepGraph)
        assertFail("not found")(d, d)
      }
      'delAddDel - assertPass(FieldStaticRemove(StepGraph), FieldStaticAdd(StepGraph), FieldStaticRemove(StepGraph))
    }
  }
}

// =====================================================================================================================
trait CustomTextFieldEvents {
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

object CustomTextFieldEventSharedTests extends SharedTests()(NoInitialEvents.init) with CustomTextFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTextFieldEventTestV1 extends TestSuite with CustomTextFieldEvents {
  import CustomTextFieldGDv1._
  import NoInitialEvents._

  implicit class FieldCustomTextCreateExt(private val a: FieldCustomTextCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    'create {
      'needName      - assertFail("Name")     (c1.mod(_ - Name))
      'needKey       - assertFail("Key")      (c1.mod(_ - Key))
      'needMandatory - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes  - assertFail("Types")    (c1.mod(_ - ApplicableReqTypes))
      'badName       - assertFail("blank")    (c1.mod(_ + Name("")))
    //'badKey        - assertFail("Key")      (c1.mod(_ + Key("?")))
      'badReqTypes   - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(onlyRT1))) // RT1 doesn't exist
      'dupName       - assertFail("unique")   (c1, c2.mod(_ + Name(c1Name)))
    //'dupKey        - assertFail("unique")   (c1, c2.mod(_ + Key(c1Key)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Text.v1(1, c1Name, "stuff", true, allReqTypes, Live))

        es :+= FieldCustomTextUpdateV1(1, nev(Name("AH"), Mandatory(false)))
        assertEq(r, CustomField.Text.v1(1, "AH", "stuff", false, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTextUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Text.v1(1, "AH", "stuff", false, onlyRT1, Live))

      }
      'badName     - assertFail("blank") (c1, FieldCustomTextUpdateV1(1, nev(Name(""))))
    //'badKey      - assertFail("Key")   (c1, FieldCustomTextUpdateV1(1, nev(Key("?"))))
      'badReqTypes - assertFail("Types") (c1, FieldCustomTextUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupName     - assertFail("unique")(c1, c2, FieldCustomTextUpdateV1(2, nev(Name(c1Name))))
    //'dupKey      - assertFail("unique")(c1, c2, FieldCustomTextUpdateV1(2, nev(Key(c1Key))))
    }
  }
}

// =====================================================================================================================
trait CustomTagFieldEventsV1 {
  import CustomTagFieldGDv1._

  def mkC1(tagId: TagId) = FieldCustomTagCreateV1(1, nev(TagId(tagId), Mandatory(true), ApplicableReqTypes(allReqTypes)))

  type CE = FieldCustomTagCreateV1
  val c1  = mkC1(1.TG)
  val c2  = FieldCustomTagCreateV1(2, nev(TagId(2.AT), Mandatory(false), ApplicableReqTypes(onlyUC)))
  val u1  = FieldCustomTagUpdateV1(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFTag)
  val r1  = FieldCustomRestore(1.CFTag)
}

object CustomTagFieldEventSharedTests extends SharedTests()(CustomTagFieldEventTestV1.init) with CustomTagFieldEventsV1 {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTagFieldEventTestV1 extends TestSuite with CustomTagFieldEventsV1 {
  import CustomTagFieldGDv1._

  val createAT2 = {
    import ApplicableTagGD._
    ApplicableTagCreate(2, nev(Key("c2"), Desc(Some("r")), Colour(None), ApplicableReqTypes(allReqTypes)))
  }
  val softDelTG1 = TagGroupEventTest.sd1
  implicit val init = InitialEvents(TagGroupEventTest.c1, createAT2)

  implicit class FieldCustomTagCreateExt(private val a: FieldCustomTagCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    'create {
      'needTagId     - assertFail("TagId")    (c1.mod(_ - TagId))
      'needMandatory - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes  - assertFail("Types")    (c1.mod(_ - ApplicableReqTypes))
      'tagIdNotFound - assertFail("Tag")      (c1.mod(_ + TagId(9.TG)))
      'tagIdDead     - assertFail("dead")     (softDelTG1, c1)
      'badReqTypes   - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(onlyRT1))) // RT1 doesn't exist
      'dupTagId      - assertFail("unique")   (c1, c2.mod(_ + TagId(1.TG)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Tag.v1(1, 1.TG, false, allReqTypes, Live))

        es :+= FieldCustomTagUpdateV1(1, nev(TagId(2.AT), Mandatory(true)))
        assertEq(r, CustomField.Tag.v1(1, 2.AT, true, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTagUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Tag.v1(1, 2.AT, true, onlyRT1, Live))
      }
      'tagIdNotFound - assertFail("Tag")   (c1, FieldCustomTagUpdateV1(1, nev(TagId(9.TG))))
      'badReqTypes   - assertFail("Types") (c1, FieldCustomTagUpdateV1(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupTagId      - assertFail("unique")(c1, c2, FieldCustomTagUpdateV1(2, nev(TagId(1.TG))))
    }
  }
}

// =====================================================================================================================
trait CustomImpFieldEventsV1 {
  import CustomImpFieldGDv1._

  type CE = FieldCustomImpCreateV1
  val c1  = FieldCustomImpCreateV1(1, nev(ReqTypeId(1), Mandatory(true), ApplicableReqTypes(onlyUC)))
  val c2  = FieldCustomImpCreateV1(2, nev(ReqTypeId(StaticReqType.UseCase), Mandatory(false), ApplicableReqTypes(allReqTypes)))
  val u1  = FieldCustomImpUpdateV1(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFImp)
  val r1  = FieldCustomRestore(1.CFImp)
}

object CustomImpFieldEventSharedTests extends SharedTests()(CustomImpFieldEventTestV1.init) with CustomImpFieldEventsV1 {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomImpFieldEventTestV1 extends TestSuite with CustomImpFieldEventsV1 {
  import CustomImpFieldGDv1._

  implicit val init = InitialEvents(CustomReqTypeEventTest.c1, CustomReqTypeEventTest.use1)

  val softDelRT1 = CustomReqTypeEventTest.sd1

  implicit class FieldCustomImpCreateExt(private val a: FieldCustomImpCreateV1) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    'create {
      'needReqTypeId     - assertFail("ReqTypeId")(c1.mod(_- ReqTypeId))
      'needMandatory     - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes      - assertFail("Types")    (c1.mod(_- ApplicableReqTypes))
      'reqTypeIdNotFound - assertFail("ReqType")  (c1.mod(_ + ReqTypeId(9)))
      'reqTypeIdDead     - assertFail("dead")     (softDelRT1, c1)
      'badReqTypes       - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(notRT2))) // RT2 doesn't exist
      'dupReqTypeId      - assertFail("unique")   (c1, c2.mod(_ + ReqTypeId(1)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Implication.v1(1, 1, false, onlyUC, Live))

        es :+= CustomReqTypeEventTest.c2
        es :+= FieldCustomImpUpdateV1(1, nev(ApplicableReqTypes(notRT2)))
        assertEq(r, CustomField.Implication.v1(1, 1, false, notRT2, Live))
      }
      'reqTypeIdNotFound - assertFail("Imp")   (c1, FieldCustomImpUpdateV1(1, nev(ReqTypeId(9))))
      'badReqTypes       - assertFail("Types") (c1, FieldCustomImpUpdateV1(1, nev(ApplicableReqTypes(notRT2)))) // RT2 doesn't exist
      'dupReqTypeId      - assertFail("unique")(c1, c2, FieldCustomImpUpdateV1(2, nev(ReqTypeId(1))))
    }
  }
}
