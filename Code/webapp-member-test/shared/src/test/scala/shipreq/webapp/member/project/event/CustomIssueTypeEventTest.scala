package shipreq.webapp.member.project.event

import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.ApplyEventTestFns._
import shipreq.webapp.member.project.event.CustomIssueTypeGD._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.NoInitialEvents._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

trait CustomIssueTypeEvents {
  type CE = CustomIssueTypeCreate
  val c1  = CustomIssueTypeCreate(1, nev(Key("TBD"), Desc(None)))
  val c2  = CustomIssueTypeCreate(2, nev(Key("PEND"), Desc(Some("pending"))))
  val u1  = CustomIssueTypeUpdate(1, nev(Key("TD")))
  val sd1 = CustomIssueTypeDelete(1)
  val r1  = CustomIssueTypeRestore(1)
}

object CustomIssueTypeEventSharedTests extends SharedTests with CustomIssueTypeEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomIssueTypeEventTest extends TestSuite with CustomIssueTypeEvents {

  implicit class CustomIssueTypeCreateExt(private val a: CustomIssueTypeCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = Tests {
    "create" - {
      "needKey"  - assertFail("Key")   (c1.mod(_ - Key))
      "needDesc" - assertFail("Desc")  (c1.mod(_ - Desc))
      "badKey"   - assertFail("Key")   (c1.mod(_ + Key("?")))
      "badDesc"  - assertFail("Desc")  (c1.mod(_ + Desc(tooLongStr)))
      "dupKey"   - assertFail("unique")(c1, c2.mod(_ + Key("TBD")))
    }

    "update" - {
      "ok" - {
        var es = Vector(c1, u1)
        def r = _assertPass(es: _*).config.customIssueTypes.get(1).get
        assertEq(r, CustomIssueType(1, "TD", None, Live))

        es :+= CustomIssueTypeUpdate(1, nev(Key("X"), Desc("xxx")))
        assertEq(r, CustomIssueType(1, "X", Some("xxx"), Live))
      }

      "badKey"  - assertFail("Key")   (c1, CustomIssueTypeUpdate(1, nev(Key("?"))))
      "badDesc" - assertFail("Desc")  (c1, CustomIssueTypeUpdate(1, nev(Desc(tooLongStr))))
      "dupKey"  - assertFail("unique")(c1, c2, CustomIssueTypeUpdate(2, nev(Key("TBD"))))
    }
  }
}
