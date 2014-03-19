package shpireq.taskman.server

import org.specs2.mutable._
import org.joda.time.{Period, DateTime}
import org.scalacheck.Arbitrary
import shipreq.taskman.api.{Msg, Priority}
import Worker._
import scalaz.effect.IO
import shipreq.base.test.{MockOpTransformer, MockOpTransformer1}
import Sop._
import shipreq.base.util.ErrorOr
import scalaz.\/-
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import shipreq.taskman.api.Types._

class WorkerTest extends Specification {


  class MockSops extends MockOpTransformer[Sop, IO] {
    val assignNodeResponses = MockResponse(Seq.empty[MsgHeader])
    val assignWorkerResponses = MockResponse(Option[MsgDetail](null))

    override def call[A] = {
      case _: GetMsgsAssignNode => assignNodeResponses.pop()
      case _: GetMsgAssignWorker => assignWorkerResponses.pop()
      case _: MarkMsgComplete =>
      case _: MsgFailedAbort =>
      case _: MsgFailedRetry =>
      case _: NotifySupportWorkerFailed =>
      case _: NotifySupportTaskmanError =>
    }
  }


  /*
  case class Reified(worker: WorkerId)(
    implicit node: NodeId,
             opToIo: Sop ~> IO,
             jfToIo: FailedJobReaction => IO[Unit],
             failurePolicy: FailurePolicy,
             msgProcessor: Msg => IO[ErrorOr[Unit]]) {

   */

  val tn = DateTime.now()
  val mh = MsgHeader(MsgId(1), Priority(6), tn)


  "Worker.Reified" >> {
    implicit val node = NodeId(4)

    implicit val jfToIo: FailedJobReaction => IO[Unit] = jf => ???
    implicit val failurePolicy: FailurePolicy = msg => err => ???
    implicit val msgProcessor: Msg => IO[ErrorOr[Unit]] = msg => IO(\/-(()))

    // couldn't acquire

    // pass

    // worker throws

    // assign Throws

    val md = MsgDetail(mh, ReRegistrationAttempted("@".tag), 1)

    // mark complete throws

    "Work completes" >> {
      implicit val mockSop = new MockSops
      mockSop.assignWorkerResponses << Some(md)
      val r = Worker.Reified(WorkerId(1)).process(mh).unsafePerformIO()

      "Result" in { r ==== WorkResult.Completed }
      "Sops" in { mockSop.allOpClasses ==== List(classOf[GetMsgAssignWorker], classOf[MarkMsgComplete]) }
    }

  }

}
