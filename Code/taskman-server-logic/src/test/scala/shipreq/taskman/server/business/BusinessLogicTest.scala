package shipreq.taskman.server.business

import org.specs2.mutable.Specification
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.taskman.server.TestHelpers._
import shipreq.taskman.api.Msg
import shipreq.taskman.server.{MsgDetail, MockBops}
import Bop._
import MailingList.API._
import shipreq.taskman.server.Worker.MsgProcessorIn

class BusinessLogicTest extends Specification {

  def testM(bop: MockBops, msg: Msg) = {
    val bl = new BusinessLogic(bop, MockEmails, null, null)
    val mo = bl(new MsgProcessorIn[Nothing](MsgDetail(mh_1, msg, 0), null))
    val io = mo getOrElse sys.error("Async not supported")
    (bop, io.unsafePerformIO())
  }

  s"${Msg.LandingPageHit} handler" should {

    def test(bop: MockBops) = testM(bop, sampleLP)

    "Add to ML & email support" in {
      val bop = new MockBops
      test(bop)._1 must haveRun[Bop].ops3[LookupShipReqUser, MailingListOp[Subscribe], SendEmail]
    }

    "Update ML & email support" in {
      val bop = new MockBops
      bop.mlSubscribe << MailingList.AlreadySubscribed
      test(bop)._1 must haveRun[Bop].ops4[LookupShipReqUser, MailingListOp[Subscribe], MailingListOp[UpdateMember], SendEmail]
    }

    "Skip the ML update when user already has account" in {
      val bop = new MockBops
      bop.lookupShipReqUserR << Some(null)
      test(bop)._1 must haveRun[Bop].ops2[LookupShipReqUser, SendEmail]
    }

    "Fail on ML error" in {
      val bop = new MockBops
      bop.mlSubscribe << ???
      test(bop) must match2(haveRun[Bop].anyBut1[SendEmail], beAnError)
    }
  }

  s"${Msg.RegistrationCompleted} handler" should {

    def test(bop: MockBops) = testM(bop, Msg.RegistrationCompleted(sampleUserId))

    "update ML" in {
      val bop = new MockBops
      bop.lookupShipReqUserR << Some(sampleShipReqUser)
      test(bop) must match2(haveRun[Bop].ops2[LookupShipReqUser, MailingListOp[BatchSubscribe]], notBeAnError)
    }
  }
}
