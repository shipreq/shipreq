package shipreq.taskman.server

import org.specs2.mutable._
import Sop._
import Worker._
import TestHelpers._

class WorkerTest extends Specification {

  def test(opToIo: SopReifier, fp: FailurePolicy, mp: MsgProcessor): WorkResult =
    Worker.Reified()(NodeId(4), WorkerId(7), opToIo, clockReal, fp, mp).process(mh_1).unsafePerformIO()

  "Worker.Reified" >> {

    "Work completes" >> {
      val mockSop = assignWorkerAllow(new MockSops)
      val r = test(mockSop, fpRetry, mpNop)
      "Result" in {
        r must beLike{ case _: WorkResult.Completed => ok }
      }
      "Marks msg as complete" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MarkMsgComplete])
      }
    }

    "Worker crashes (retry)" >> {
      val mockSop = assignWorkerAllow(new MockSops)
      val r = test(mockSop, fpRetry, mpCrash)
      "Result" in {
        r must beLike{ case _: WorkResult.WorkerFailed => ok }
      }
      "Schedules retry" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedRetry])
      }
    }

    "Worker crashes (abort)" >> {
      val mockSop = assignWorkerAllow(new MockSops)
      val r = test(mockSop, fpAbort, mpCrash)
      "Result" in {
        r must beLike{ case _: WorkResult.WorkerFailed => ok }
      }
      "Aborts job" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedAbort])
      }
    }

    "Worker crashes (abort and notify support)" >> {
      val mockSop = assignWorkerAllow(new MockSops)
      val r = test(mockSop, fpAbortSupport, mpCrash)
      "Result" in {
        r must beLike{ case _: WorkResult.WorkerFailed => ok }
      }
      "Aborts job & notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedAbort], classOf[NotifySupportWorkerFailed])
      }
    }

    "Taskman crashes pre-work" >> {
      val mockSop = assignWorkerCrash(new MockSops)
      val r = test(mockSop, fpRetry, mpCrash)
      "Result" in {
        r must beLike{ case _: WorkResult.TaskmanFailed => ok }
      }
      "Notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[NotifySupportTaskmanError])
      }
    }

    "Taskman crashes post-work" >> {
      val mockSop = (msgCompleteCrash compose assignWorkerAllow)(new MockSops)
      val r = test(mockSop, fpRetry, mpNop)
      "Result" in {
        r must beLike{ case _: WorkResult.TaskmanFailed => ok }
      }
      "Notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MarkMsgComplete], classOf[NotifySupportTaskmanError])
      }
    }

  }

}
