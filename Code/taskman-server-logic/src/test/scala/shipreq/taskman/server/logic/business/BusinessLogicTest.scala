package shipreq.taskman.server.logic.business

import org.specs2.mutable.Specification
import scalaz.syntax.catchable._
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.base.util.FxModule._
import shipreq.taskman.api.Msg
import shipreq.taskman.server.logic.TestHelpers._
import shipreq.taskman.server.logic.{MsgDetail, MockBops}
import BusinessOp._
import MailingList.API._

class BusinessLogicTest extends Specification {

  def testM(bop: MockBops, msg: Msg) = {
    val bl = new BusinessLogic(mockEmails(false), null, null)(bop)
    val fx = bl(MsgDetail(mh_1, msg, 0))
    (bop, fx.attempt.unsafeRun())
  }

  type raiseTicket = SupportOp[Support.API.NotifyLandingPage]

  s"${Msg.LandingPageHit} handler" should {

    def test(bop: MockBops) = testM(bop, sampleLP)

    "Add to ML & raise support ticket" in {
      val bop = new MockBops
      test(bop)._1 must haveRun[BusinessOp].ops3[FindShipReqUser, MailingListOp[Subscribe], raiseTicket]
    }

    "Update ML & raise support ticket" in {
      val bop = new MockBops
      bop.mlSubscribe << MailingList.AlreadySubscribed
      test(bop)._1 must haveRun[BusinessOp].ops4[FindShipReqUser, MailingListOp[Subscribe], MailingListOp[UpdateMember], raiseTicket]
    }

    "Skip the ML update when user already has account" in {
      val bop = new MockBops
      bop.findShipReqUser << Some(null)
      test(bop)._1 must haveRun[BusinessOp].ops2[FindShipReqUser, raiseTicket]
    }

    "Fail on ML error" in {
      val bop = new MockBops
      bop.mlSubscribe << ???
      test(bop) must match2(haveRun[BusinessOp].anyBut1[raiseTicket], beAnError)
    }
  }

  s"${Msg.RegistrationCompleted} handler" should {

    def test(bop: MockBops) = testM(bop, Msg.RegistrationCompleted(sampleUserId))

    "update ML" in {
      val bop = new MockBops
      bop.findShipReqUser << Some(sampleShipReqUser)
      test(bop) must match2(haveRun[BusinessOp].ops2[FindShipReqUser, MailingListOp[BatchSubscribe]], notBeAnError)
    }
  }
}
