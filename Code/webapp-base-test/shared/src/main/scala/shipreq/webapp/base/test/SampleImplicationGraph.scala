package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import shipreq.webapp.base.test.ProjectDsl._
import shipreq.webapp.base.test.UnsafeTypes._

/**
 * mf1 → fr1 ̉↘
 *             fr2 → fr3
 *     ↗ mf2 ↗
 * br1             ↗ mf4 → fr6
 *     ↘ br2 → mf3
 *                 ↘ fr4 → fr5 → mf5
 */
object SampleImplicationGraph {

  def mfFieldValues =
    """
      |BR1 - 2,3,4,5
      |BR2 - 3,4,5
      |FR1 - 1,2
      |FR2 - 1,2
      |FR3 - 1,2
      |FR4 - 3,5
      |FR5 - 3,5
      |FR6 - 3,4
      |FR7 -
      |MF1 - 1
      |MF2 - 2
      |MF3 - 3,4,5
      |MF4 - 3,4
      |MF5 - 3,5
      |""".stripMargin.trim

  val mf1: GenericReqId = 11
  val mf2: GenericReqId = 12
  val mf3: GenericReqId = 13
  val mf4: GenericReqId = 14
  val mf5: GenericReqId = 15

  val br1: GenericReqId = 21
  val br2: GenericReqId = 22

  val fr1: GenericReqId = 31
  val fr2: GenericReqId = 32
  val fr3: GenericReqId = 33
  val fr4: GenericReqId = 34
  val fr5: GenericReqId = 35
  val fr6: GenericReqId = 36
  val fr7: GenericReqId = 37

  type Values = SampleProject.Values
  val Values  = SampleProject.Values

  lazy val projectDsl = {
    def t(i: GenericReqId, rt: CustomReqTypeId, tgts: ReqId*) = GReq(id = i, reqType = rt, impTgts = tgts.toSet)
    import Values._

    ( t(mf1, mf, fr1)
    + t(mf2, mf, fr2)
    + t(mf3, mf, mf4, fr4)
    + t(mf4, mf, fr6)
    + t(mf5, mf)

    + t(br1, br, mf2, br2)
    + t(br2, br, mf3)

    + t(fr1, fr, fr2)
    + t(fr2, fr, fr3)
    + t(fr3, fr)
    + t(fr4, fr, fr5)
    + t(fr5, fr, mf5)
    + t(fr6, fr)
    + t(fr7, fr)
    )
  }

  lazy val project =
    projectDsl ! SampleProject.project
}
