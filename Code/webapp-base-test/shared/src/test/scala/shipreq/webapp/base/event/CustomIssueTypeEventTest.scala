package shipreq.webapp.base.event

import utest._
import shipreq.base.util.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.BaseTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import CustomIssueTypeGD._
import DeletionAction._

trait CustomIssueTypeEvents {
  type CE = CreateCustomIssueType
  val c1  = CreateCustomIssueType(1, nev(Key("TBD"), Desc(None)))
  val c2  = CreateCustomIssueType(2, nev(Key("PEND"), Desc(Some("pending"))))
  val u1  = UpdateCustomIssueType(1, nev(Key("TD")))
  val sd1 = DeleteCustomIssueType(1, SoftDel)
  val hd1 = DeleteCustomIssueType(1, HardDel)
  val r1  = DeleteCustomIssueType(1, Restore)
}

object CustomIssueTypeEventSharedTests extends SharedTests with CustomIssueTypeEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomIssueTypeEventTest extends TestSuite with CustomIssueTypeEvents {

  implicit class CreateCustomIssueTypeExt(private val a: CreateCustomIssueType) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needKey  - assertFail("Key")   (c1.mod(_ - Key))
      'needDesc - assertFail("Desc")  (c1.mod(_ - Desc))
      'badKey   - assertFail("Key")   (c1.mod(_ + Key("?")))
      'badDesc  - assertFail("Desc")  (c1.mod(_ + Desc(tooLongStr)))
      'dupKey   - assertFail("unique")(c1, c2.mod(_ + Key("TBD")))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r = _assertPass(es: _*).config.customIssueTypes.data.get(1).get
        assertEq(r, CustomIssueType(1, "TD", None, Live))

        es :+= UpdateCustomIssueType(1, nev(Key("X"), Desc("xxx")))
        assertEq(r, CustomIssueType(1, "X", Some("xxx"), Live))
      }

      'badKey  - assertFail("Key")   (c1, UpdateCustomIssueType(1, nev(Key("?"))))
      'badDesc - assertFail("Desc")  (c1, UpdateCustomIssueType(1, nev(Desc(tooLongStr))))
      'dupKey  - assertFail("unique")(c1, c2, UpdateCustomIssueType(2, nev(Key("TBD"))))
    }

    // TODO Add tests of HardDeletion failing when subject in use
  }
}
