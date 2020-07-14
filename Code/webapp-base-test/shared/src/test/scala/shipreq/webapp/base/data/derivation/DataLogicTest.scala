package shipreq.webapp.base.data.derivation

import japgolly.microlibs.stdlib_ext.MutableArray
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
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
          val values = MutableArray(data(req.id).iterator.map(_.pos.value)).sort.mkString(",")
          val reqType = project.config.reqTypes.need(req.reqTypeId).mnemonic.value
          s"$reqType${req.pubid.pos.value} - $values".trim
        }
      ).sort.mkString("\n")

    assertMultiline(actual, expect)
  }

  private def testImpFieldLogicV2_SIG1(): Unit = {
    import SampleImplicationGraph.Values.mfField
    import SampleImplicationGraph._
    assertImpFields(project, mfField, mfFieldValues)
  }

  private def testImpFieldLogicV2_SIG2(): Unit = {
    import SampleImplicationGraph2._
    assertImpFields(project, mfField, mfFieldValues)
  }

  private def testImpFieldLogicV2_SIG3(): Unit = {
    import SampleImplicationGraph3._
    assertImpFields(project, mfField, mfFieldValues)
  }

  override def tests = Tests {
    // https://shipreq.com/project/d6My#/reqs/IV-26
    "impFieldLogicV2" - {
      "SIG1" - testImpFieldLogicV2_SIG1()
      "SIG2" - testImpFieldLogicV2_SIG2()
      "SIG3" - testImpFieldLogicV2_SIG3()
    }
  }
}
