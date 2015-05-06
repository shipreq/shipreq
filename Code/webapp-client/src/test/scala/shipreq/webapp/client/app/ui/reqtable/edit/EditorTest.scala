package shipreq.webapp.client.app.ui.reqtable
package edit

import scalaz.std.anyVal._
import scalaz.std.set._
import scalaz.std.string._
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.UnsafeTypes._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.test.TestUtil._
import shipreq.webapp.base.test.SampleImplicationGraph

object EditorTest extends TestSuite {
  shipreq.webapp.client.app.ui.Style // Ensure initialised

  val (impAllReqsAndKeys, impLookupAll) = {
    import SampleImplicationGraph._
    import ImplicationEditor._

    val reqsAndKeys = project.reqs.data.reqs
      .vstream(r => (r, AutoComplete.normaliseReqPubid(PlainText.pubid(project, r.pubid).get)))

    def lall = lookupAll(project, PlainText(project))

    (reqsAndKeys, lall)
  }

  val impAllKeys = impAllReqsAndKeys.map(_._2).toSet

  override def tests = TestSuite {

    'implicationCells {
      import SampleImplicationGraph._
      import ImplicationEditor._

      def testAll(declFwd: Boolean, subjImpliesInput: Boolean): Unit = {
        val x = project.reqFieldData.data.implications.srcToTgt
        for {
          (subj, _)  <- impAllReqsAndKeys
          (req, key) <- impAllReqsAndKeys
        }{
          val l = lookupForSubject(project, impLookupAll, subj.id, declFwd)
          val y =
            if (subjImpliesInput)
              x.add(subj.id, req.id).m
            else
              x.add(req.id, subj.id).m
          val c = ReqFieldData.implicationCycleDetector.hasCycle(y)
          assertEq(l.illegal contains key, c)
        }
      }


      'src {
        def test(subj: ReqId, illegal: String): Unit = {
          val l = lookupForSubject(project, impLookupAll, subj, declFwd(Column.ImplicationSrc))
          val keys = l.illegal.keySet ++ l.legalm.keySet
          assertEq(keys, impAllKeys)
          assertSet(l.illegal.keySet)(illegal.split(" +"): _*)
        }
        test(mf1, "MF1 FR1 FR2 FR3")
        test(fr2,         "FR2 FR3")
        test(fr3,             "FR3")
        test(br1, "BR1 MF2 FR2 FR3 BR2 MF3 FR4 FR5 MF5 MF4 FR6")
        test(br2,                 "BR2 MF3 FR4 FR5 MF5 MF4 FR6")
        test(fr2,         "FR2 FR3"                            )

        testAll(declFwd(Column.ImplicationSrc), false)
      }

      'tgt {
        testAll(declFwd(Column.ImplicationTgt), true)
      }

      'col {
        def test(subj: ReqId, legal: String): Unit = {
          val fid = CustomField.Implication.Id(6) // major feature
          val l1 = lookupForCol(project, impLookupAll, fid)
          val l = lookupForSubject(project, l1, subj, declFwd(fid))
          val keys = l.illegal.keySet ++ l.legalm.keySet
          assertEq(keys, impAllKeys)
          assertSet(l.legalm.keySet)(legal.split(" +"): _*)
        }
        test(mf1, "MF2 MF3 MF4 MF5")
        test(fr2, "MF1 MF2 MF3 MF4 MF5")
        test(br1, "MF1")
      }

    } // implicationCells
  }
}