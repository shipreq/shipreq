package shipreq.webapp.base.event

import utest._
import shipreq.base.util.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import DeletionAction._
import CustomFieldEventTestHelpers._

object CustomFieldEventTestHelpers {
  val onlyUC = onlyReqTypes(StaticReqType.UseCase)
  val onlyRT1 = onlyReqTypes(1)
  val notRT2 = notReqTypes(2)
}

object CustomFieldEventTest extends TestSuite {
  import StaticField._

  implicit val init = InitialEvents(CustomTextFieldEventTest.c1, CustomTextFieldEventTest.c2)

  val t1 = CustomTextFieldEventTest.c1.id
  val t2 = CustomTextFieldEventTest.c2.id

  override def tests = TestSuite {

    'reposition {
      var es = Vector[Event]()
      def o = _assertPass(es: _*).config.fields.order
      assertEq(o, Vector(NormalAltStepTree, ExceptionStepTree, StepGraph, t1, t2))

      es :+= RepositionField(t1, Some(ExceptionStepTree))
      assertEq(o, Vector(NormalAltStepTree, t1, ExceptionStepTree, StepGraph, t2))

      es :+= RepositionField(ExceptionStepTree, None)
      assertEq(o, Vector(NormalAltStepTree, t1, StepGraph, t2, ExceptionStepTree))
    }

    'addStaticField {
      'ok - assertPass(DeleteStaticField(StepGraph), AddStaticField(StepGraph))
      'failIfExists     - assertFail("exists")(AddStaticField(StepGraph))
      'failIfNotDelable - assertFail("exists")(AddStaticField(NormalAltStepTree))
    }

    'deleteStatic {
      // assertQty allows less detail here
      'ok - {
        for (f <- values.whole.filter(_.deletable :: Deletable))
          assertPass(DeleteStaticField(f))
      }
      'undeletable - {
        for (f <- values.whole.filterNot(_.deletable :: Deletable))
          assertFail("delet")(DeleteStaticField(f))
      }
      'twice - {
        val d = DeleteStaticField(StepGraph)
        assertFail("not found")(d, d)
      }
      'delAddDel - assertPass(DeleteStaticField(StepGraph), AddStaticField(StepGraph), DeleteStaticField(StepGraph))
    }
  }
}

// =====================================================================================================================
trait CustomTextFieldEvents {
  import CustomTextFieldGD._

  val c1Name = "Stuff"
  val c1Key = FieldRefKey("stf")
  type CE = CreateCustomTextField
  val c1  = CreateCustomTextField(1, nev(Name(c1Name), Key(c1Key), Mandatory(true), ReqTypes(allReqTypes)))
  val c2  = CreateCustomTextField(2, nev(Name("Roar"), Key("r"), Mandatory(false), ReqTypes(onlyUC)))
  val u1  = UpdateCustomTextField(1, nev(Key("stuff")))
  val sd1 = DeleteCustomField(1.CFText, Delete)
  val r1  = DeleteCustomField(1.CFText, Restore)
}

object CustomTextFieldEventSharedTests extends SharedTests()(NoInitialEvents.init) with CustomTextFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTextFieldEventTest extends TestSuite with CustomTextFieldEvents {
  import CustomTextFieldGD._
  import NoInitialEvents._

  implicit class CreateCustomTextFieldExt(private val a: CreateCustomTextField) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needName      - assertFail("Name")     (c1.mod(_ - Name))
      'needKey       - assertFail("Key")      (c1.mod(_ - Key))
      'needMandatory - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes  - assertFail("Types")    (c1.mod(_ - ReqTypes))
      'badName       - assertFail("blank")    (c1.mod(_ + Name("")))
      'badKey        - assertFail("Key")      (c1.mod(_ + Key("?")))
      'badReqTypes   - assertFail("Types")    (c1.mod(_ + ReqTypes(onlyRT1))) // RT1 doesn't exist
      'dupName       - assertFail("unique")   (c1, c2.mod(_ + Name(c1Name)))
      'dupKey        - assertFail("unique")   (c1, c2.mod(_ + Key(c1Key)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Text(1, c1Name, "stuff", true, allReqTypes, Live))

        es :+= UpdateCustomTextField(1, nev(Name("AH"), Mandatory(false)))
        assertEq(r, CustomField.Text(1, "AH", "stuff", false, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= UpdateCustomTextField(1, nev(ReqTypes(onlyRT1)))
        assertEq(r, CustomField.Text(1, "AH", "stuff", false, onlyRT1, Live))

      }
      'badName     - assertFail("blank") (c1, UpdateCustomTextField(1, nev(Name(""))))
      'badKey      - assertFail("Key")   (c1, UpdateCustomTextField(1, nev(Key("?"))))
      'badReqTypes - assertFail("Types") (c1, UpdateCustomTextField(1, nev(ReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupName     - assertFail("unique")(c1, c2, UpdateCustomTextField(2, nev(Name(c1Name))))
      'dupKey      - assertFail("unique")(c1, c2, UpdateCustomTextField(2, nev(Key(c1Key))))
    }
  }
}

// =====================================================================================================================
trait CustomTagFieldEvents {
  import CustomTagFieldGD._

  def mkC1(tagId: TagId) = CreateCustomTagField(1, nev(TagId(tagId), Mandatory(true), ReqTypes(allReqTypes)))

  type CE = CreateCustomTagField
  val c1  = mkC1(1.TG)
  val c2  = CreateCustomTagField(2, nev(TagId(2.AT), Mandatory(false), ReqTypes(onlyUC)))
  val u1  = UpdateCustomTagField(1, nev(Mandatory(false)))
  val sd1 = DeleteCustomField(1.CFTag, Delete)
  val r1  = DeleteCustomField(1.CFTag, Restore)
}

object CustomTagFieldEventSharedTests extends SharedTests()(CustomTagFieldEventTest.init) with CustomTagFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTagFieldEventTest extends TestSuite with CustomTagFieldEvents {
  import CustomTagFieldGD._

  val createAT2 = {
    import ApplicableTagGD._
    CreateApplicableTag(2, nev(Name("Released"), Desc(Some("r")), Key("c2")))
  }
  val softDelTG1 = TagGroupEventTest.sd1
  implicit val init = InitialEvents(TagGroupEventTest.c1, createAT2)

  implicit class CreateCustomTagFieldExt(private val a: CreateCustomTagField) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needTagId     - assertFail("TagId")    (c1.mod(_ - TagId))
      'needMandatory - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes  - assertFail("Types")    (c1.mod(_ - ReqTypes))
      'tagIdNotFound - assertFail("Tag")      (c1.mod(_ + TagId(9.TG)))
      'tagIdDead     - assertFail("dead")     (softDelTG1, c1)
      'badReqTypes   - assertFail("Types")    (c1.mod(_ + ReqTypes(onlyRT1))) // RT1 doesn't exist
      'dupTagId      - assertFail("unique")   (c1, c2.mod(_ + TagId(1.TG)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Tag(1, 1.TG, false, allReqTypes, Live))

        es :+= UpdateCustomTagField(1, nev(TagId(2.AT), Mandatory(true)))
        assertEq(r, CustomField.Tag(1, 2.AT, true, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= UpdateCustomTagField(1, nev(ReqTypes(onlyRT1)))
        assertEq(r, CustomField.Tag(1, 2.AT, true, onlyRT1, Live))
      }
      'tagIdNotFound - assertFail("Tag")      (c1, UpdateCustomTagField(1, nev(TagId(9.TG))))
      'badReqTypes   - assertFail("Types")    (c1, UpdateCustomTagField(1, nev(ReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupTagId      - assertFail("unique")   (c1, c2, UpdateCustomTagField(2, nev(TagId(1.TG))))
    }
  }
}

// =====================================================================================================================
trait CustomImpFieldEvents {
  import CustomImpFieldGD._

  type CE = CreateCustomImpField
  val c1  = CreateCustomImpField(1, nev(ReqTypeId(1), Mandatory(true), ReqTypes(onlyUC)))
  val c2  = CreateCustomImpField(2, nev(ReqTypeId(StaticReqType.UseCase), Mandatory(false), ReqTypes(allReqTypes)))
  val u1  = UpdateCustomImpField(1, nev(Mandatory(false)))
  val sd1 = DeleteCustomField(1.CFImp, Delete)
  val r1  = DeleteCustomField(1.CFImp, Restore)
}

object CustomImpFieldEventSharedTests extends SharedTests()(CustomImpFieldEventTest.init) with CustomImpFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomImpFieldEventTest extends TestSuite with CustomImpFieldEvents {
  import CustomImpFieldGD._

  implicit val init = InitialEvents(CustomReqTypeEventTest.c1)

  val softDelRT1 = CustomReqTypeEventTest.sd1

  implicit class CreateCustomImpFieldExt(private val a: CreateCustomImpField) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needReqTypeId     - assertFail("ReqTypeId")(c1.mod(_ - ReqTypeId))
      'needMandatory     - assertFail("Mandatory")(c1.mod(_ - Mandatory))
      'needReqTypes      - assertFail("Types")    (c1.mod(_ - ReqTypes))
      'reqTypeIdNotFound - assertFail("ReqType")  (c1.mod(_ + ReqTypeId(9)))
      'reqTypeIdDead     - assertFail("dead")     (softDelRT1, c1)
      'badReqTypes       - assertFail("Types")    (c1.mod(_ + ReqTypes(notRT2))) // RT2 doesn't exist
      'dupReqTypeId      - assertFail("unique")   (c1, c2.mod(_ + ReqTypeId(1)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Implication(1, 1, false, onlyUC, Live))

        es :+= CustomReqTypeEventTest.c2
        es :+= UpdateCustomImpField(1, nev(ReqTypes(notRT2)))
        assertEq(r, CustomField.Implication(1, 1, false, notRT2, Live))
      }
      'reqTypeIdNotFound - assertFail("Imp")   (c1, UpdateCustomImpField(1, nev(ReqTypeId(9))))
      'badReqTypes       - assertFail("Types") (c1, UpdateCustomImpField(1, nev(ReqTypes(notRT2)))) // RT2 doesn't exist
      'dupReqTypeId      - assertFail("unique")(c1, c2, UpdateCustomImpField(2, nev(ReqTypeId(1))))
    }
  }
}
