package shipreq.webapp.base.event

import utest._
import shipreq.base.util.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import CustomReqTypeGD._
import DeletionAction._
import NoInitialEvents._

trait CustomReqTypeEvents {
  val mfName = "Major Feature"
  type CE = CreateCustomReqType
  val c1  = CreateCustomReqType(1, nev(Mnemonic("MF"), Name(mfName), Imp(ImplicationRequired)))
  val c2  = CreateCustomReqType(2, nev(Mnemonic("FR"), Name("Functional Req"), Imp(ImplicationRequired.Not)))
  val u1  = UpdateCustomReqType(1, nev(Mnemonic("M")))
  val sd1 = DeleteCustomReqType(1, SoftDel)
  val hd1 = DeleteCustomReqType(1, HardDel)
  val r1  = DeleteCustomReqType(1, Restore)
}

object CustomReqTypeEventSharedTests extends SharedTests with CustomReqTypeEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomReqTypeEventTest extends TestSuite with CustomReqTypeEvents {

  implicit class CreateCustomReqTypeExt(private val a: CreateCustomReqType) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needName - assertFail("Name")          (c1.mod(_ - Name))
      'needMne  - assertFail("Mnemonic")      (c1.mod(_ - Mnemonic))
      'needImp  - assertFail("Imp")           (c1.mod(_ - Imp))
      'badName  - assertFail("blank")         (c1.mod(_ + Name("")))
      'badMne   - assertFail("Mnemonic")      (c1.mod(_ + Mnemonic("?")))
      'dupName  - assertFail("unique")        (c1, c2.mod(_ + Name(mfName)))
      'dupMne   - assertFail("unique")        (c1, c2.mod(_ + Mnemonic("MF")))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r = _assertPass(es: _*).config.customReqTypes.data.get(1).get
        assertEq(r, CustomReqType(1, "M", Set("MF"), mfName, ImplicationRequired, Live))

        es :+= UpdateCustomReqType(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r, CustomReqType(1, "X", Set("MF", "M"), "xxx", ImplicationRequired, Live))

        es :+= UpdateCustomReqType(1, nev(Mnemonic("MF"), Imp(ImplicationRequired.Not)))
        assertEq(r ,CustomReqType(1, "MF", Set("M", "X"), "xxx", ImplicationRequired.Not, Live))
      }
      'badName  - assertFail("blank")    (c1, UpdateCustomReqType(1, nev(Name(""))))
      'badMne   - assertFail("Mnemonic") (c1, UpdateCustomReqType(1, nev(Mnemonic("?"))))
      'dupName  - assertFail("unique")   (c1, c2, UpdateCustomReqType(2, nev(Name(mfName))))
      'dupMne   - assertFail("unique")   (c1, c2, UpdateCustomReqType(2, nev(Mnemonic("MF"))))
    }

    'delete {
      'whenLiveImpFieldS - assertFail("")(c1, CustomImpFieldEventTest.c1, sd1)
      'whenLiveImpFieldH - assertFail("")(c1, CustomImpFieldEventTest.c1, hd1)
      'whenDeadImpFieldS - assertPass    (c1, CustomImpFieldEventTest.c1, CustomImpFieldEventTest.sd1, sd1)
      'whenDeadImpFieldH - assertFail("")(c1, CustomImpFieldEventTest.c1, CustomImpFieldEventTest.sd1, hd1)
    }
    // TODO Add tests of HardDeletion failing when subject in use
  }
}
