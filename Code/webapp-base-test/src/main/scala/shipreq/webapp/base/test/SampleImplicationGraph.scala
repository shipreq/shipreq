package shipreq.webapp.base.test

import shipreq.webapp.base.data._
import ProjectDSL._
import UnsafeTypes._

/**
 * mf1 → fr1 ̉↘
 *             fr2 → fr3
 *     ↗ mf2 ↗
 * br1             ↗ mf4 → fr6
 *     ↘ br2 → mf3
 *                 ↘ fr4 → fr5 → mf5
 */
object SampleImplicationGraph {

  val mf1: GenericReq.Id = 11
  val mf2: GenericReq.Id = 12
  val mf3: GenericReq.Id = 13
  val mf4: GenericReq.Id = 14
  val mf5: GenericReq.Id = 15

  val br1: GenericReq.Id = 21
  val br2: GenericReq.Id = 22

  val fr1: GenericReq.Id = 31
  val fr2: GenericReq.Id = 32
  val fr3: GenericReq.Id = 33
  val fr4: GenericReq.Id = 34
  val fr5: GenericReq.Id = 35
  val fr6: GenericReq.Id = 36

  lazy val projectDsl = {
    def t(i: GenericReq.Id, rt: ReqType.Id, tgts: Req.Id*) = GReq(id = i, reqType = rt, impTgts = tgts.toSet)
    val (mf, br, fr) = (2: ReqType.Id, 4: ReqType.Id, 3: ReqType.Id)

    ( t(mf1, mf, 31)
    + t(mf2, mf, 32)
    + t(mf3, mf, 14, 34)
    + t(mf4, mf, 36)
    + t(mf5, mf)

    + t(br1, br, 12, 22)
    + t(br2, br, 13)

    + t(fr1, fr, 32)
    + t(fr2, fr, 33)
    + t(fr3, fr)
    + t(fr4, fr, 35)
    + t(fr5, fr, 15)
    + t(fr6, fr)
    )
  }

  lazy val project =
    projectDsl ! SampleProject.project
}
