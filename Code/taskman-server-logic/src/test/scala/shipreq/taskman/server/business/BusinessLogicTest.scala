package shipreq.taskman.server.business

import org.specs2.mutable.Specification
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.taskman.server.TestHelpers._
import shipreq.taskman.api.Msg
import shipreq.taskman.server.MockBops
import Bop._
import MailingList.API._

class BusinessLogicTest extends Specification {

  s"${Msg.LandingPageHit} handler" should {

    def test(bop: MockBops) = {
      val bl = new BusinessLogic(bop, MockEmails, null, null)
      val r = bl.LandingPage(sampleLP).unsafePerformIO()
      (bop, r)
    }

    "Add to ML & email support" in {
      val bop = new MockBops
      test(bop)._1 must haveRun[Bop].ops3[LookupShipReqUser, MailingListOp[Subscribe], SendEmail]
    }

    "Update ML & email support" in {
      val bop = new MockBops
      bop.mlSubscribe << Some(MailingList.AlreadySubscribed)
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
}
