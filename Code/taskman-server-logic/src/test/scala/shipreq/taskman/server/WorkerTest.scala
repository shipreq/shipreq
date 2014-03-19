package shipreq.taskman.server

import org.specs2.mutable._
import org.joda.time.{Period, DateTime}
import scalaz.{Endo, ~>, \/-}
import scalaz.effect.IO
import shipreq.base.test.MockOpTransformer
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import shipreq.taskman.api.Types._
import shipreq.taskman.api.Priority
import Sop._
import Worker._

class WorkerTest extends Specification {

  class MockSops extends MockOpTransformer[Sop, IO] {
    val assignNodeR = MockResponse(Seq.empty[MsgHeader])
    val assignWorkerR = MockResponse(Option[MsgDetail](null))
    val msgCompleteR = MockResponse(())
    val msgFailedRetryR = MockResponse(())

    override def call[A] = {
      case _: GetMsgsAssignNode => assignNodeR.pop()
      case _: GetMsgAssignWorker => assignWorkerR.pop()
      case _: MarkMsgComplete => msgCompleteR.pop()
      case _: MsgFailedAbort =>
      case _: MsgFailedRetry => msgFailedRetryR.pop()
      case _: NotifySupportWorkerFailed =>
      case _: NotifySupportTaskmanError =>
    }
  }

  final def endoMod[A](f: A => Unit) = Endo[A](a => {f(a); a})

  val tn = DateTime.now()
  val mh = MsgHeader(MsgId(1), Priority(6), tn)
  val md = MsgDetail(mh, ReRegistrationAttempted("@".tag), 1)

  val crashAssignWorker = endoMod[MockSops](_.assignWorkerR << ???)
  val allowAssignWorker = endoMod[MockSops](_.assignWorkerR << Some(md))
  val msgCompleteCrash = endoMod[MockSops](_.msgCompleteR << ???)

  val fpAbort: FailurePolicy =
    msg => err => FailurePolicyR(MsgFailedAbort(msg), Nil)

  val fpAbortSupport: FailurePolicy =
    msg => err => FailurePolicyR(MsgFailedAbort(msg), NotifySupportWorkerFailed(msg, err) :: Nil)

  val fpRetry: FailurePolicy =
    msg => err => FailurePolicyR(MsgFailedRetry(msg, Period days 1), Nil)

  val mpNop: MsgProcessor = msg => IO(\/-(()))
  val mpCrash: MsgProcessor = msg => ???

  def test(opToIo: Sop ~> IO, fp: FailurePolicy, mp: MsgProcessor): WorkResult =
    Worker.Reified(WorkerId(7))(NodeId(4), opToIo, fp, mp).process(mh).unsafePerformIO()

  "Worker.Reified" >> {

    "Work completes" >> {
      val mockSop = allowAssignWorker(new MockSops)
      val r = test(mockSop, fpRetry, mpNop)
      "Result" in {
        r ==== WorkResult.Completed
      }
      "Marks msg as complete" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MarkMsgComplete])
      }
    }

    "Worker crashes (retry)" >> {
      val mockSop = allowAssignWorker(new MockSops)
      val r = test(mockSop, fpRetry, mpCrash)
      "Result" in {
        r ==== WorkResult.WorkerFailed
      }
      "Schedules retry" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedRetry])
      }
    }

    "Worker crashes (abort)" >> {
      val mockSop = allowAssignWorker(new MockSops)
      val r = test(mockSop, fpAbort, mpCrash)
      "Result" in {
        r ==== WorkResult.WorkerFailed
      }
      "Aborts job" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedAbort])
      }
    }

    "Worker crashes (abort and notify support)" >> {
      val mockSop = allowAssignWorker(new MockSops)
      val r = test(mockSop, fpAbortSupport, mpCrash)
      "Result" in {
        r ==== WorkResult.WorkerFailed
      }
      "Aborts job & notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MsgFailedAbort], classOf[NotifySupportWorkerFailed])
      }
    }

    "Taskman crashes pre-work" >> {
      val mockSop = crashAssignWorker(new MockSops)
      val r = test(mockSop, fpRetry, mpCrash)
      "Result" in {
        r ==== WorkResult.TaskmanFailed
      }
      "Notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[NotifySupportTaskmanError])
      }
    }

    "Taskman crashes post-work" >> {
      val mockSop = (msgCompleteCrash compose allowAssignWorker)(new MockSops)
      val r = test(mockSop, fpRetry, mpNop)
      "Result" in {
        r ==== WorkResult.TaskmanFailed
      }
      "Notifies support" in {
        mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MarkMsgComplete], classOf[NotifySupportTaskmanError])
      }
    }

  }

}
