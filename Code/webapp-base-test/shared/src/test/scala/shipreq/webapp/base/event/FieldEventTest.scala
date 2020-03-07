package shipreq.webapp.base.event

import utest._
import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import CustomFieldEventTestHelpers._
import Event._

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
  import CustomTextFieldGD._

  val c1Name = "Stuff"
  val c1Key = FieldRefKey("stf")
  type CE = FieldCustomTextCreate
  val c1  = FieldCustomTextCreate(1, nev(Name(c1Name), Key(c1Key), Mandatory(true), ApplicableReqTypes(allReqTypes)))
  val c2  = FieldCustomTextCreate(2, nev(Name("Roar"), Key("r"), Mandatory(false), ApplicableReqTypes(onlyUC)))
  val u1  = FieldCustomTextUpdate(1, nev(Key("stuff")))
  val sd1 = FieldCustomDelete(1.CFText)
  val r1  = FieldCustomRestore(1.CFText)
}

object CustomTextFieldEventSharedTests extends SharedTests()(NoInitialEvents.init) with CustomTextFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTextFieldEventTest extends TestSuite with CustomTextFieldEvents {
  import CustomTextFieldGD._
  import NoInitialEvents._

  implicit class FieldCustomTextCreateExt(private val a: FieldCustomTextCreate) extends AnyVal {
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
      'badKey        - assertFail("Key")      (c1.mod(_ + Key("?")))
      'badReqTypes   - assertFail("Types")    (c1.mod(_ + ApplicableReqTypes(onlyRT1))) // RT1 doesn't exist
      'dupName       - assertFail("unique")   (c1, c2.mod(_ + Name(c1Name)))
      'dupKey        - assertFail("unique")   (c1, c2.mod(_ + Key(c1Key)))
    }

    'update {
      'ok - {
        var es = Vector[Event](c1, u1)
        def r = _assertPass(es: _*).config.fields.customFields.get(c1.id).get
        assertEq(r, CustomField.Text(1, c1Name, "stuff", true, allReqTypes, Live))

        es :+= FieldCustomTextUpdate(1, nev(Name("AH"), Mandatory(false)))
        assertEq(r, CustomField.Text(1, "AH", "stuff", false, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTextUpdate(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Text(1, "AH", "stuff", false, onlyRT1, Live))

      }
      'badName     - assertFail("blank") (c1, FieldCustomTextUpdate(1, nev(Name(""))))
      'badKey      - assertFail("Key")   (c1, FieldCustomTextUpdate(1, nev(Key("?"))))
      'badReqTypes - assertFail("Types") (c1, FieldCustomTextUpdate(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupName     - assertFail("unique")(c1, c2, FieldCustomTextUpdate(2, nev(Name(c1Name))))
      'dupKey      - assertFail("unique")(c1, c2, FieldCustomTextUpdate(2, nev(Key(c1Key))))
    }
  }
}

// =====================================================================================================================
trait CustomTagFieldEvents {
  import CustomTagFieldGD._

  def mkC1(tagId: TagId) = FieldCustomTagCreate(1, nev(TagId(tagId), Mandatory(true), ApplicableReqTypes(allReqTypes)))

  type CE = FieldCustomTagCreate
  val c1  = mkC1(1.TG)
  val c2  = FieldCustomTagCreate(2, nev(TagId(2.AT), Mandatory(false), ApplicableReqTypes(onlyUC)))
  val u1  = FieldCustomTagUpdate(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFTag)
  val r1  = FieldCustomRestore(1.CFTag)
}

object CustomTagFieldEventSharedTests extends SharedTests()(CustomTagFieldEventTest.init) with CustomTagFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomTagFieldEventTest extends TestSuite with CustomTagFieldEvents {
  import CustomTagFieldGD._

  val createAT2 = {
    import ApplicableTagGD._
    ApplicableTagCreate(2, nev(Key("c2"), Desc(Some("r")), Colour(None), ApplicableReqTypes(allReqTypes)))
  }
  val softDelTG1 = TagGroupEventTest.sd1
  implicit val init = InitialEvents(TagGroupEventTest.c1, createAT2)

  implicit class FieldCustomTagCreateExt(private val a: FieldCustomTagCreate) extends AnyVal {
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
        assertEq(r, CustomField.Tag(1, 1.TG, false, allReqTypes, Live))

        es :+= FieldCustomTagUpdate(1, nev(TagId(2.AT), Mandatory(true)))
        assertEq(r, CustomField.Tag(1, 2.AT, true, allReqTypes, Live))

        es :+= CustomReqTypeEventTest.c1
        es :+= FieldCustomTagUpdate(1, nev(ApplicableReqTypes(onlyRT1)))
        assertEq(r, CustomField.Tag(1, 2.AT, true, onlyRT1, Live))
      }
      'tagIdNotFound - assertFail("Tag")      (c1, FieldCustomTagUpdate(1, nev(TagId(9.TG))))
      'badReqTypes   - assertFail("Types")    (c1, FieldCustomTagUpdate(1, nev(ApplicableReqTypes(onlyRT1)))) // RT1 doesn't exist
      'dupTagId      - assertFail("unique")   (c1, c2, FieldCustomTagUpdate(2, nev(TagId(1.TG))))
    }
  }
}

// =====================================================================================================================
trait CustomImpFieldEvents {
  import CustomImpFieldGD._

  type CE = FieldCustomImpCreate
  val c1  = FieldCustomImpCreate(1, nev(ReqTypeId(1), Mandatory(true), ApplicableReqTypes(onlyUC)))
  val c2  = FieldCustomImpCreate(2, nev(ReqTypeId(StaticReqType.UseCase), Mandatory(false), ApplicableReqTypes(allReqTypes)))
  val u1  = FieldCustomImpUpdate(1, nev(Mandatory(false)))
  val sd1 = FieldCustomDelete(1.CFImp)
  val r1  = FieldCustomRestore(1.CFImp)
}

object CustomImpFieldEventSharedTests extends SharedTests()(CustomImpFieldEventTest.init) with CustomImpFieldEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomImpFieldEventTest extends TestSuite with CustomImpFieldEvents {
  import CustomImpFieldGD._

  implicit val init = InitialEvents(CustomReqTypeEventTest.c1, CustomReqTypeEventTest.use1)

  val softDelRT1 = CustomReqTypeEventTest.sd1

  implicit class FieldCustomImpCreateExt(private val a: FieldCustomImpCreate) extends AnyVal {
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
        assertEq(r, CustomField.Implication(1, 1, false, onlyUC, Live))

        es :+= CustomReqTypeEventTest.c2
        es :+= FieldCustomImpUpdate(1, nev(ApplicableReqTypes(notRT2)))
        assertEq(r, CustomField.Implication(1, 1, false, notRT2, Live))
      }
      'reqTypeIdNotFound - assertFail("Imp")   (c1, FieldCustomImpUpdate(1, nev(ReqTypeId(9))))
      'badReqTypes       - assertFail("Types") (c1, FieldCustomImpUpdate(1, nev(ApplicableReqTypes(notRT2)))) // RT2 doesn't exist
      'dupReqTypeId      - assertFail("unique")(c1, c2, FieldCustomImpUpdate(2, nev(ReqTypeId(1))))
    }
  }
}
