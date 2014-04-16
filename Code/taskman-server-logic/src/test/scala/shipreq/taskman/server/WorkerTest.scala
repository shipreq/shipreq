package shipreq.taskman.server

import org.joda.time.{DateTime, Period}
import org.specs2.mutable.Specification
import scala.reflect.ClassTag
import scalaz.{\/-, \/, -\/, ~>, Need, Endo}
import scalaz.effect.IO
import shipreq.base.util.ScalaExt.Tuple2Ext
import TestHelpers._
import Sop._
import Worker._
import WorkResult._

class WorkerTest extends Specification {

  type R = AsyncResult[Need] \/ WorkResult

  val nid = NodeId(4.toShort)
  val wid = WorkerId(7)
  val tp = AssignmentTrustPeriod(Period minutes 3)

  def everIncClock(p: Period, start: DateTime = new DateTime): IO[DateTime] = {
    var prev: Option[DateTime] = None
    IO {
      val r = prev map (_ plus p) getOrElse start
      prev = Some(r)
      r
    }
  }

  def haveResultS[W <: WorkResult : ClassTag] =
    beAnInstanceOf[W] ^^ {(_:AnyRef) match {
      case r: WorkResult => r
      case \/-(r: WorkResult) => r
      case Some(r: WorkResult) => r
      // case _ => ko("doesn't contain work result")
    }}

  def haveResultA = beAnInstanceOf[-\/[AsyncResult[Need]]]

  def allMockSopClasses = (_: MockSops).allOpClasses

  def haveRun1[A <: Sop[_] : ClassTag] =
    allMockSopClasses ^^ be_==(List(
      implicitly[ClassTag[A]].runtimeClass))

  def haveRun2[A <: Sop[_] : ClassTag, B <: Sop[_] : ClassTag] =
    allMockSopClasses ^^ be_==(List(
      implicitly[ClassTag[A]].runtimeClass
      , implicitly[ClassTag[B]].runtimeClass))

  def haveRun3[A <: Sop[_] : ClassTag, B <: Sop[_] : ClassTag, C <: Sop[_] : ClassTag] =
    allMockSopClasses ^^ be_==(List(
      implicitly[ClassTag[A]].runtimeClass
      , implicitly[ClassTag[B]].runtimeClass
      , implicitly[ClassTag[C]].runtimeClass))

  // -------------------------------------------------------------------------------------------------------------------

  "Worker processing msgs synchronously" >> {

    def test(sopEndo: Endo[MockSops], fp: FailurePolicy, mp: MsgProcessor[Need]) = {
      val mockSop = sopEndo(new MockSops)
      val w = new Worker(mp)(nid, wid, mockSop, tp, clockReal, fp)
      val r: R = w.process(mh_1).unsafePerformIO()
      (r, mockSop)
    }

    "Work completes" >> {
      val (r,s) = test(assignWorkerAllow, fpRetry, mpNop)
      "Result"                in (r must haveResultS[Completed])
      "Marks msg as complete" in (s must haveRun2[GetMsgAssignWorker, UpdateMsgSuccess])
    }

    "Worker crashes (retry)" >> {
      val (r, s) = test(assignWorkerAllow, fpRetry, mpCrash)
      "Result"          in (r must haveResultS[WorkerFailed])
      "Schedules retry" in (s must haveRun2[GetMsgAssignWorker, UpdateMsgRetry])
    }

    "Worker crashes (abort)" >> {
      val (r, s) = test(assignWorkerAllow, fpAbort, mpCrash)
      "Result"     in (r must haveResultS[WorkerFailed])
      "Aborts job" in (s must haveRun2[GetMsgAssignWorker, UpdateMsgAbort])
    }

    "Worker crashes (retry and notify support)" >> {
      val (r, s) = test(assignWorkerAllow, fpRetrySupport, mpCrash)
      "Result"                       in (r must haveResultS[WorkerFailed])
      "Fails job & notifies support" in (s must haveRun3[GetMsgAssignWorker, UpdateMsgRetry, NotifySupportWorkerFailed])
    }

    "Taskman crashes pre-work" >> {
      val (r, s) = test(assignWorkerCrash, fpRetry, mpCrash)
      "Result"           in (r must haveResultS[TaskmanFailed])
      "Notifies support" in (s must haveRun2[GetMsgAssignWorker, NotifySupportTaskmanError])
    }

    "Taskman crashes post-work" >> {
      val (r, s) = test(crashOnUpdateMsgSuccess compose assignWorkerAllow, fpRetry, mpNop)
      "Result"           in (r must haveResultS[TaskmanFailed])
      "Notifies support" in (s must haveRun3[GetMsgAssignWorker, UpdateMsgSuccess, NotifySupportTaskmanError])
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  "Worker processing msgs asynchronously" >> {

    def blah(io: IOE[Unit]
             , clock: IO[DateTime] = clockReal
             , sopEndo: Endo[MockSops] = assignWorkerAllow
              ) = {
      def run = {
        val need = new (IO ~> Need) { def apply[A](io: IO[A]) = Need(io.unsafePerformIO()) }
        val mp: MsgProcessor[Need] = i => i.asyncT(need)(io)
        val mockSop = sopEndo(new MockSops)
        val w = new Worker(mp)(nid, wid, mockSop, tp, clock, fpRetry)
        val r: R = w.process(mh_1).unsafePerformIO()
        (r, mockSop)
      }
      def runFuture(r1: R) = r1.swap.toOption.map(_.f).get.value
      (run, run map1 runFuture)
    }

    def longClock = everIncClock(tp.value plusSeconds 1)

    "Work completes" >> {
      val ((r1, s1), (r2, s2)) = blah(IOE.nop)
      "Immediate result"             in (r1 must haveResultA)
      "Assigns msg before future"    in (s1 must haveRun1[GetMsgAssignWorker])
      "Future result"                in (r2 must haveResultS[Completed])
      "Future marks msg as complete" in (s2 must haveRun2[GetMsgAssignWorker, UpdateMsgSuccess])
    }

    "Future crashes" >> {
      val ((r1, s1), (r2, s2)) = blah(IOE(???))
      "Immediate result"           in (r1 must haveResultA)
      "Assigns msg before future"  in (s1 must haveRun1[GetMsgAssignWorker])
      "Future result"              in (r2 must haveResultS[WorkerFailed])
      "Future marks msg as failed" in (s2 must haveRun2[GetMsgAssignWorker, UpdateMsgRetry])
    }

    "Reassigns and completes" >> {
      val ((r1, s1), (r2, s2)) = blah(IOE.nop, clock = longClock)
      "Immediate result"             in (r1 must haveResultA)
      "Assigns msg before future"    in (s1 must haveRun1[GetMsgAssignWorker])
      "Future result"                in (r2 must haveResultS[Completed])
      "Future marks msg as complete" in (s2 must haveRun3[GetMsgAssignWorker, ReAssignWorker, UpdateMsgSuccess])
    }

    "Future fails to reassign worker" >> {
      val ((r1, s1), (r2, s2)) = blah(IOE.nop, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerDeny)
      "Immediate result"                             in (r1 must haveResultA)
      "Assigns msg before future"                    in (s1 must haveRun1[GetMsgAssignWorker])
      "Future result"                                in (r2 must haveResultS[CouldntReAssign])
      "Future does nothing after reassignment fails" in (s2 must haveRun2[GetMsgAssignWorker, ReAssignWorker])
    }

    "Future encounters taskman error" >> {
      val ((r1, s1), (r2, s2)) = blah(IOE.nop, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerCrash)
      "Immediate result"          in (r1 must haveResultA)
      "Assigns msg before future" in (s1 must haveRun1[GetMsgAssignWorker])
      "Future result"             in (r2 must haveResultS[TaskmanFailed])
      "Future notifies support"   in (s2 must haveRun3[GetMsgAssignWorker, ReAssignWorker, NotifySupportTaskmanError])
    }

  }
}
