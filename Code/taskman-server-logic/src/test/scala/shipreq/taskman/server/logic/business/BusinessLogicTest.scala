package shipreq.taskman.server.logic.business

import shipreq.base.util.FxModule._
import shipreq.taskman.api.Task
import shipreq.taskman.server.logic.TestHelpers._
import shipreq.taskman.server.logic.business.BusinessOp._
import shipreq.taskman.server.logic.business.MailingList.API._
import shipreq.taskman.server.logic.{MockBops, TaskDetail}
import utest._

object BusinessLogicTest extends TestSuite {

  private def testM(bop: MockBops, task: Task) = {
    val bl = new BusinessLogic(mockEmails(false), null)(bop)
    val fx = bl(TaskDetail(th_1, task, 0))
    (bop, fx.attempt.unsafeRun())
  }

  private type raiseTicket = SupportOp[Support.API.NotifyLandingPage]

  override def tests = Tests {

    "Task.LandingPageHit" - {

      def test(bop: MockBops) = testM(bop, sampleLP)

      "Add to ML & raise support ticket" - {
        val bop = new MockBops
        test(bop)._1.assertOpTypes3[FindShipReqUser, MailingListOp[Subscribe], raiseTicket]
      }

      "Update ML & raise support ticket" - {
        val bop = new MockBops
        bop.mlSubscribe << MailingList.AlreadySubscribed
        test(bop)._1.assertOpTypes4[FindShipReqUser, MailingListOp[Subscribe], MailingListOp[UpdateMember], raiseTicket]
      }

      "Skip the ML update when user already has account" - {
        val bop = new MockBops
        bop.findShipReqUser << Some(null)
        test(bop)._1.assertOpTypes2[FindShipReqUser, raiseTicket]
      }

      "Fail on ML error" - {
        val bop = new MockBops
        bop.mlSubscribe << ???
        val (bop2, result) = test(bop)
        bop2.assertAnyOpsBut1[raiseTicket]
        assert(result.isLeft)
      }
    }

    "Task.RegistrationCompleted" - {
      def test(bop: MockBops) = testM(bop, Task.RegistrationCompleted(sampleUserId))

      "update ML" - {
        val bop = new MockBops
        bop.findShipReqUser << Some(sampleShipReqUser)
        val (bop2, result) = test(bop)
        bop2.assertOpTypes2[FindShipReqUser, MailingListOp[BatchSubscribe]]
        assert(result.isRight)
      }
    }

  }
}
