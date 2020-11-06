package shipreq.webapp.member.project.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.test.WebappTestUtil._
import sourcecode.Line
import utest._

object DataLogicTest extends TestSuite {

  private def assertImpFields(project: Project,
                              field  : CustomField.Implication.Id,
                              expect : String)
                             (implicit l: Line): Unit = {

    val data = project.dataLogic.customFieldImps(HideDead)(field)

    val actual =
      MutableArray(
        project.liveReqIterator().map { req =>
          val values = MutableArray(data.getPubids(req.id).iterator.map(_.pos.value)).sort.mkString(",")
          val reqType = project.config.reqTypes.need(req.reqTypeId).mnemonic.value
          s"$reqType${req.pubid.pos.value} - $values".trim
        }
      ).sort.mkString("\n")

    assertMultiline(actual, expect)
  }

  private def testImpFieldLogicV2_SIG1(): Unit = {
    import shipreq.webapp.member.test.project.SampleImplicationGraph.Values.mfField
    import shipreq.webapp.member.test.project.SampleImplicationGraph._
    assertImpFields(project, mfField, mfFieldValues)
  }

  private def testImpFieldLogicV2_SIG2(): Unit = {
    import shipreq.webapp.member.test.project.SampleImplicationGraph2._
    assertImpFields(project, mfField, mfFieldValues)
  }

  private def testImpFieldLogicV2_SIG3(): Unit = {
    import shipreq.webapp.member.test.project.SampleImplicationGraph3._
    assertImpFields(project, mfField, mfFieldValues)
  }

  private def testImpFieldLogicV2_SIG3b(): Unit = {
    import shipreq.webapp.member.test.project.SampleImplicationGraph3._
    assertImpFields(VariantB.project, mfField, VariantB.mfFieldValues)
  }

  override def tests = Tests {
    // https://shipreq.com/project/d6My#/reqs/IV-26
    "impFieldLogicV2" - {
      "SIG1"  - testImpFieldLogicV2_SIG1()
      "SIG2"  - testImpFieldLogicV2_SIG2()
      "SIG3"  - testImpFieldLogicV2_SIG3()
      "SIG3b" - testImpFieldLogicV2_SIG3b()
    }
  }
}
