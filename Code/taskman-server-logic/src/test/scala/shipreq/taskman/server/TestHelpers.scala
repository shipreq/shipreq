package shipreq.taskman.server

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.joda.time.{Period, DateTime}
import scalaz.Lens.lensg
import scalaz.{Heap, Order, Endo}
import scalaz.effect.IO
import shipreq.base.test.MockOpTransformer
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import Sop._
import Manager._
import Worker._

object TestHelpers {

  implicit class HeapExt[A: Order](val value: Heap[A]) {
    def -(a: A) = value.filter(_ != a)
    def +(a: A) = value insert a
    def ++(as: Seq[A]) = (value /: as)((q,a) => q + a)
  }

  final def endoMod[A](f: A => Unit) = Endo[A](a => {f(a); a})

  val timeNow = DateTime.now()
  val timePast = timeNow minusMinutes 10

  val msg_rereg = ReRegistrationAttempted("@".tag)
  val mh_1 = MsgHeader(MsgId(1), Priority(6), timeNow)
  val md_1 = MsgDetail(mh_1, msg_rereg, 0)
  val mh_2 = MsgHeader(MsgId(2), Priority(5), timePast)

  object lenses {
    object msgDetail {
      val failureCountL = lensg[MsgDetail, Int](m => f => m.copy(failureCount = f.toShort), _.failureCount)
      val headerL = lensg[MsgDetail, MsgHeader](m => h => m.copy(hdr = h), _.hdr)
      val priorityL = headerL >=> msgHeader.priorityL
      val createdL = headerL >=> msgHeader.createdL
    }
    object msgHeader {
      val priorityL = lensg[MsgHeader, Priority](m => p => m.copy(priority = p), _.priority)
      val createdL = lensg[MsgHeader, DateTime](m => c => m.copy(created = c), _.created)
    }
    object failureCtx {
      val msgL = lensg[FailureCtx, MsgDetail](c => md => c.copy(m = md), _.m)
      val failureCountL = msgL >=> msgDetail.failureCountL
      val priorityL = msgL >=> msgDetail.priorityL
      val createdL = msgL >=> msgDetail.createdL
    }
  }

  def assignWorkerTo(md: MsgDetail) = endoMod[MockSops](_.assignWorkerR << Some(md))
  val assignWorkerAllow = assignWorkerTo(md_1)
  val assignWorkerCrash = endoMod[MockSops](_.assignWorkerR << ???)
  val msgCompleteCrash = endoMod[MockSops](_.msgCompleteR << ???)

  val clockReal = IO(DateTime.now)

  val fpAbort: FailurePolicy =
    f => FailureResponse(MsgFailedAbort(f.m), Nil)

  val fpAbortSupport: FailurePolicy =
    f => FailureResponse(MsgFailedAbort(f.m), NotifySupportWorkerFailed(f.m, f.err) :: Nil)

  val fpRetry: FailurePolicy =
    f => FailureResponse(MsgFailedRetry(f.m, Period days 1), Nil)

  val mpNop: MsgProcessor = msg => nopTask
  val mpCrash: MsgProcessor = msg => ???

  def arbMap[B, A](f: A => B)(implicit a: Arbitrary[A]): Arbitrary[B] =
    Arbitrary { a.arbitrary.map(f) }

  implicit def arbitraryMsgId = arbMap[MsgId, Long](new MsgId(_))
  implicit def arbitraryPriority = arbMap[Priority, Short](new Priority(_))
  implicit def arbitraryDateTime = arbMap[DateTime, Long](new DateTime(_))

  implicit def arbitraryMsgHeader: Arbitrary[MsgHeader] =
    Arbitrary(for {
      i <- arbitrary[MsgId]
      p <- arbitrary[Priority]
      c <- arbitrary[DateTime]
    } yield
      MsgHeader(i,p,c))

  implicit def arbitraryJobQueue = arbMap[JobQueue, List[MsgHeader]](emptyQueue ++ _)

}

class MockSops extends MockOpTransformer[Sop, IO] {
  val cfgGetR = MockResponse(Option[String](null))
  val assignNodeR = MockResponse(Seq.empty[MsgHeader])
  val assignWorkerR = MockResponse(Option[MsgDetail](null))
  val msgCompleteR = MockResponse(())
  val msgFailedRetryR = MockResponse(())

  override def call[A] = {
    case _: CfgGet                    => cfgGetR.pop()
    case _: GetMsgsAssignNode         => assignNodeR.pop()
    case _: GetMsgAssignWorker        => assignWorkerR.pop()
    case _: MarkMsgComplete           => msgCompleteR.pop()
    case _: MsgFailedAbort            =>
    case _: MsgFailedRetry            => msgFailedRetryR.pop()
    case _: NotifySupportWorkerFailed =>
    case _: NotifySupportTaskmanError =>
    case    Nop                       =>
  }
}
