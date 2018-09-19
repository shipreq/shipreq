package shipreq.webapp.base.event

import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.test.WebappTestUtil._
import AutoNES._

object HighLevelEventTest extends TestSuite {

  override def tests = Tests {

    'unusedReqTypeMnemonicsCanBeReclaimed {
      import NoInitialEvents._

      def createReqType(id: CustomReqTypeId, mnemonic: ReqType.Mnemonic, name: String) = {
        import CustomReqTypeGD._
        CustomReqTypeCreate(id, nev(Mnemonic(mnemonic), Name(name), Imp(ImplicationRequired.Not)))
      }

      def test(p: Project, expect: String): Unit = {
        val actual =
          p.config.reqTypes.custom.valuesIterator
            .map(t => s"${t.id.value}:${t.mnemonic.value}${t.oldMnemonics.toList.map("/" + _.value).sorted.mkString}")
            .toArray.sorted
            .mkString(" ")
        assertEq(actual, expect)
      }

      val p1 = applyEventsSuccessfully(Project.empty,
        createReqType(1, "MF", "a"),                            // → 1:MF
        createReqType(2, "FR", "b"),                            // → 1:MF 2:FR
        CustomReqTypeDelete(1),                                 // → 2:FR
        CustomReqTypeUpdate(2, CustomReqTypeGD.Mnemonic("MF")), // → 2:MF
        createReqType(3, "FR", "c"))                            // → 2:MF 3:FR

      test(p1, "2:MF 3:FR")

      val p2 = applyEventsSuccessfully(p1,
        GenericReqCreate(1.GR, 2, GenericReqGD.emptyValues), // → 2:MF 3:FR | MF-1
        ReqsDelete(1.GR, Set.empty, Vector.empty),           // → 2:MF 3:FR | MF-1!
        CustomReqTypeDelete(3),                              // → 2:MF      | MF-1!
        CustomReqTypeDelete(2))                              // → 2:MF!     | MF-1!

      test(p2, "2:MF")

      assertEventFails(p2, createReqType(4, "MF", "x"), "each mnemonic is unique")

      val p3 = applyEventsSuccessfully(p2,
        CustomReqTypeRestore(2),                                // → 2:MF   | MF-1!
        CustomReqTypeUpdate(2, CustomReqTypeGD.Mnemonic("X")),  // → 2:X/MF | MF-1!
        CustomReqTypeUpdate(2, CustomReqTypeGD.Mnemonic("MF"))) // → 2:MF/X | MF-1!

      test(p3, "2:MF/X")
    }

  }
}
