package shipreq.webapp.member.test.project

import shipreq.base.util.Forwards
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._

//   MF4
//    ↓
//   FB3 ---------------------------------------------------+
//    ↑                                                     |
//   FB4                                                    |
//                                                          |
//   FB2 -------------------------------------------+       |
//                                                  ↓       ↓
//   FB1 --> IV1 --> UC1(DEAD) --> IV2 --> UC2 --> MF1 --> FR1
//            |                     ↑       |
//            +---------------------+       +----> MF2 --> MF3
//              (manually relinked)
//
// https://shipreq.com/project/d6My#reqs/IV-26
object SampleImplicationGraph2 {

  def mfFieldValues =
    """
      |FB1 - 1,2,3
      |FB2 - 1
      |FB3 - 1,4
      |FB4 - 1,4
      |FR1 - 1,4
      |IV1 - 1,2,3
      |IV2 - 1,2,3
      |MF1 - 1
      |MF2 - 2,3
      |MF3 - 2,3
      |MF4 - 4
      |UC2 - 1,2,3
      |""".stripMargin.trim

  val mf      = CustomReqTypeId(1)
  val fb      = CustomReqTypeId(2)
  val iv      = CustomReqTypeId(3)
  val fr      = CustomReqTypeId(4)
  val mf1     = GenericReqId(11)
  val mf2     = GenericReqId(12)
  val mf3     = GenericReqId(13)
  val mf4     = GenericReqId(14)
  val fb1     = GenericReqId(21)
  val fb2     = GenericReqId(22)
  val fb3     = GenericReqId(23)
  val fb4     = GenericReqId(24)
  val iv1     = GenericReqId(31)
  val iv2     = GenericReqId(32)
  val fr1     = GenericReqId(41)
  val uc1     = UseCaseId(51)
  val uc2     = UseCaseId(52)
  val mfField = CustomField.Implication.Id(1)

  val project = applyEventsSuccessfully(Project.empty,

    Event.CustomReqTypeCreate(mf, CustomReqTypeGD("MF", "Major Feature", Optional, None)),
    Event.CustomReqTypeCreate(fb, CustomReqTypeGD("FB", "Feedback", Optional, None)),
    Event.CustomReqTypeCreate(iv, CustomReqTypeGD("IV", "Interview", Optional, None)),
    Event.CustomReqTypeCreate(fr, CustomReqTypeGD("FR", "Functional Requirement", Optional, None)),
    Event.FieldCustomImpCreate(mfField, mf, CustomImpFieldGD(FieldReqTypeRules.optional)),

    Event.GenericReqCreate(mf1, mf, GenericReqGD.emptyValues),
    Event.GenericReqCreate(mf2, mf, GenericReqGD.emptyValues),
    Event.GenericReqCreate(mf3, mf, GenericReqGD.emptyValues),
    Event.GenericReqCreate(mf4, mf, GenericReqGD.emptyValues),
    Event.GenericReqCreate(fb1, fb, GenericReqGD.emptyValues),
    Event.GenericReqCreate(fb2, fb, GenericReqGD.emptyValues),
    Event.GenericReqCreate(fb3, fb, GenericReqGD.emptyValues),
    Event.GenericReqCreate(fb4, fb, GenericReqGD.emptyValues),
    Event.GenericReqCreate(iv1, iv, GenericReqGD.emptyValues),
    Event.GenericReqCreate(iv2, iv, GenericReqGD.emptyValues),
    Event.GenericReqCreate(fr1, fr, GenericReqGD.emptyValues),
    Event.UseCaseCreate(uc1, uc1.value, UseCaseGD.emptyValues),
    Event.UseCaseCreate(uc2, uc2.value, UseCaseGD.emptyValues),

    Event.ReqImplicationsPatch(mf1, Forwards, nesd()(fr1)),
    Event.ReqImplicationsPatch(mf2, Forwards, nesd()(mf3)),
    Event.ReqImplicationsPatch(mf4, Forwards, nesd()(fb3)),
    Event.ReqImplicationsPatch(fb1, Forwards, nesd()(iv1)),
    Event.ReqImplicationsPatch(fb2, Forwards, nesd()(mf1)),
    Event.ReqImplicationsPatch(fb3, Forwards, nesd()(fr1)),
    Event.ReqImplicationsPatch(fb4, Forwards, nesd()(fb3)),
    Event.ReqImplicationsPatch(iv1, Forwards, nesd()(uc1, iv2)),
    Event.ReqImplicationsPatch(iv2, Forwards, nesd()(uc2)),
    Event.ReqImplicationsPatch(uc1, Forwards, nesd()(iv2)),
    Event.ReqImplicationsPatch(uc2, Forwards, nesd()(mf1, mf2)),

    Event.ReqsDelete(NonEmptySet(uc1), ∅, ∅),
  )
}
