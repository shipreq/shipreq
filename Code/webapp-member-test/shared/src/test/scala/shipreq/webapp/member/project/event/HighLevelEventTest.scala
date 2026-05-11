package shipreq.webapp.member.project.event

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes.AutoNES._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

object HighLevelEventTest extends TestSuite {

  override def tests = Tests {

    "unusedReqTypeMnemonicsCanBeReclaimed" - {

      def createReqType(id: CustomReqTypeId, mnemonic: ReqType.Mnemonic, name: String) = {
        import CustomReqTypeGD._
        CustomReqTypeCreate(id, nev(Mnemonic(mnemonic), Description(None), Name(name), Implication(Optional)))
      }

      def test(p: Project, expect: String): Unit = {
        val actual =
          p.config.reqTypes.custom.valuesIterator
            .map(t => s"${t.id.value}:${t.mnemonic.value}${t.oldMnemonics.toList.map("/" + _.value).sorted.mkString}")
            .toArray.sorted
            .mkString(" ")
        assertEq(actual, expect)
      }

      val p1 = applyEventsSuccessfully(emptyProject1,
        createReqType(1, "MF", "a"),                            // → 1:MF
        createReqType(2, "FR", "b"),                            // → 1:MF 2:FR
        CustomReqTypeDelete(1),                                 // → 2:FR
        CustomReqTypeUpdate(2, CustomReqTypeGD.Mnemonic("MF")), // → 2:MF
        createReqType(3, "FR", "c"))                            // → 2:MF 3:FR

      test(p1, "2:MF 3:FR")

      val p2 = applyEventsSuccessfully(p1,
        GenericReqCreate(1.GR, 2, GenericReqGD.emptyValues), // → 2:MF 3:FR | MF-1
        ReqsDelete(1.GR, Set.empty, Text.empty),             // → 2:MF 3:FR | MF-1!
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
