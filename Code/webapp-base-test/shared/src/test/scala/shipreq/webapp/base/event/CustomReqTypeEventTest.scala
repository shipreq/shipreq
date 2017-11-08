package shipreq.webapp.base.event

import utest._
import japgolly.microlibs.nonempty.NonEmpty
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._
import ApplyEventTestFns._
import AutoNES._
import CustomReqTypeGD._
import DataImplicits._
import NoInitialEvents._

trait CustomReqTypeEvents {
  val mfName = "Major Feature"
  type CE = CustomReqTypeCreate
  val c1  = CustomReqTypeCreate(1, nev(Mnemonic("MF"), Name(mfName), Imp(ImplicationRequired)))
  val c2  = CustomReqTypeCreate(2, nev(Mnemonic("FR"), Name("Functional Req"), Imp(ImplicationRequired.Not)))
  val u1  = CustomReqTypeUpdate(1, nev(Mnemonic("M")))
  val sd1 = CustomReqTypeDelete(1)
  val r1  = CustomReqTypeRestore(1)
}

object CustomReqTypeEventSharedTests extends SharedTests with CustomReqTypeEvents {
  def setId(c: CE, i: Int) = c.copy(id = i)
  def copyId(to: CE, from: CE) = to.copy(id = from.id)
}

object CustomReqTypeEventTest extends TestSuite with CustomReqTypeEvents {

  implicit class CustomReqTypeCreateExt(private val a: CustomReqTypeCreate) extends AnyVal {
    def mod(f: Values => Values) =
      a.copy(vs = NonEmpty.force(f(a.vs.value)))
  }

  override def tests = TestSuite {
    'create {
      'needName - assertFail("Name")    (c1.mod(_ - Name))
      'needMne  - assertFail("Mnemonic")(c1.mod(_ - Mnemonic))
      'needImp  - assertFail("Imp")     (c1.mod(_ - Imp))
      'badName  - assertFail("blank")   (c1.mod(_ + Name("")))
      'badMne   - assertFail("Mnemonic")(c1.mod(_ + Mnemonic("?")))
      'dupName  - assertFail("unique")  (c1, c2.mod(_ + Name(mfName)))
      'dupMne   - assertFail("unique")  (c1, c2.mod(_ + Mnemonic("MF")))
    }

    'update {
      'ok - {
        var es = Vector(c1, u1)
        def r = _assertPass(es: _*).config.reqTypes.custom.get(1).get
        assertEq(r, CustomReqType(1, "M", Set("MF"), mfName, ImplicationRequired, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("X"), Name("xxx")))
        assertEq(r, CustomReqType(1, "X", Set("MF", "M"), "xxx", ImplicationRequired, Live))

        es :+= CustomReqTypeUpdate(1, nev(Mnemonic("MF"), Imp(ImplicationRequired.Not)))
        assertEq(r ,CustomReqType(1, "MF", Set("M", "X"), "xxx", ImplicationRequired.Not, Live))
      }
      'badName  - assertFail("blank")   (c1, CustomReqTypeUpdate(1, nev(Name(""))))
      'badMne   - assertFail("Mnemonic")(c1, CustomReqTypeUpdate(1, nev(Mnemonic("?"))))
      'dupName  - assertFail("unique")  (c1, c2, CustomReqTypeUpdate(2, nev(Name(mfName))))
      'dupMne   - assertFail("unique")  (c1, c2, CustomReqTypeUpdate(2, nev(Mnemonic("MF"))))
    }

    'delete {
      def testImpFieldLiveness(imp: Live, exp: Live)(es: Event*): Unit = {
        val p = _assertPass(es: _*)
        val f = p.config.customField(CustomImpFieldEventTest.c1.id)
        assertEq("live", imp, f live p.config)
        assertEq("liveExplicitly", exp, f.liveExplicitly)
      }
      'whenLiveImpField {
        testImpFieldLiveness(Dead, Live)(c1, CustomImpFieldEventTest.c1, sd1)
        testImpFieldLiveness(Live, Live)(c1, CustomImpFieldEventTest.c1, sd1, r1)
      }
      'whenDeadImpField {
        testImpFieldLiveness(Dead, Dead)(c1, CustomImpFieldEventTest.c1, CustomImpFieldEventTest.sd1, sd1)
        testImpFieldLiveness(Dead, Dead)(c1, CustomImpFieldEventTest.c1, CustomImpFieldEventTest.sd1, sd1, r1)
      }

      'deleteRestoreReqsAndReqCodes {
        val t     = new EventTester
        t.makeName = (i, e) => s"#$i: $e"
        val reqId = GenericReqId(8)
        val rc    = ReqCode.IdAndValue(9, "oh.good")
        def test(grLiveImp: Live)(e: Event): Unit =
          t(e) { name =>
            val r = t.p.content.reqs.genericReqs.need(reqId)
            assertEq(s"$name - req.live", r live t.p.config.reqTypes, grLiveImp)
            assertEq(s"$name - req.expLive", r.liveExplicitly, Live)
            assertEq(s"$name - RC.active?", t.p.content.reqCodes(rc.value).isActive, grLiveImp is Live)
          }
        t.justApply(c1)
        test(Live)(GenericReqCreate(reqId, c1.id, GenericReqGD.Codes(rc)))
        test(Dead)(sd1)
        test(Live)(r1)
      }
    }
  }
}
