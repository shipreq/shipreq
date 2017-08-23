package shipreq.taskman.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Clock, Duration, Instant}
import org.specs2.mutable.Specification
import scala.reflect.ClassTag
import scalaz.{-\/, Endo, Need, \/, \/-}
import shipreq.base.util.FxModule._
import shipreq.base.test.specs2.BaseMatchers._
import shipreq.taskman.server.logic.business.BusinessOp
import shipreq.taskman.server.logic.business.BusinessOp.{SendEmail, SupportOp}
import shipreq.taskman.server.logic.business.Support.API.ReportFailure
import TestHelpers._
import ServerOp._
import Worker._
import WorkResult._

class WorkerTest extends Specification {

  type R = WorkResult[Need]

  val nid = NodeId(4.toShort)
  val wid = WorkerId(7)
  val tp = AssignmentTrustPeriod(Duration ofMinutes 3)

  def everIncClock(d: Duration, start: Instant = Clock.systemUTC().instant()): Fx[Instant] = {
    var prev: Option[Instant] = None
    Fx {
      val r = prev map (_ plus d) getOrElse start
      prev = Some(r)
      r
    }
  }

  def haveResultS[W <: WorkResult[Need] : ClassTag] =
    beAnInstanceOf[W] ^^ {(_:AnyRef) match {
      case r: WorkResult[_] => r
      case \/-(r: WorkResult[_]) => r
      case Some(r: WorkResult[_]) => r
      // case _ => ko("doesn't contain work result")
    }}

  def haveResultA = haveResultS[Scheduled[Need]]

  // -------------------------------------------------------------------------------------------------------------------

  "Worker processing msgs synchronously" >> {

    def test(sopEndo: Endo[MockSops], fp: FailurePolicy, mp: MsgProcessor[Need]) = {
      val mockSop = sopEndo(new MockSops)
      val w = new Worker(mp)(nid, wid, mockSop, tp, clockReal, fp)
      val r: R = w.process(mh_1).unsafeRun()
      (r, mockSop)
    }

    "Work completes" >> {
      val (r,s) = test(assignWorkerAllow, fpRetry, mpNop)
      "Result"                in (r must haveResultS[Completed])
      "Marks msg as complete" in (s must haveRun[ServerOp].ops2[GetMsgAssignWorker, UpdateMsgSuccess])
    }

    "Worker crashes (retry)" >> {
      val (r, s) = test(assignWorkerAllow, fpRetry, mpCrash)
      "Result"          in (r must haveResultS[WorkerFailed])
      "Schedules retry" in (s must haveRun[ServerOp].ops2[GetMsgAssignWorker, UpdateMsgAbort])
    }

    "Worker crashes (abort)" >> {
      val (r, s) = test(assignWorkerAllow, fpAbort, mpCrash)
      "Result"     in (r must haveResultS[WorkerFailed])
      "Aborts job" in (s must haveRun[ServerOp].ops2[GetMsgAssignWorker, UpdateMsgRetry])
    }

    "Worker crashes (retry and notify support)" >> {
      val (r, s) = test(assignWorkerAllow, fpRetrySupport, mpCrash)
      "Result"                       in (r must haveResultS[WorkerFailed])
      "Fails job & notifies support" in (s must haveRun[ServerOp].ops3[GetMsgAssignWorker, UpdateMsgAbort, NotifySupportWorkerFailed])
    }

    "Taskman crashes pre-work" >> {
      val (r, s) = test(assignWorkerCrash, fpRetry, mpCrash)
      "Result"           in (r must haveResultS[TaskmanFailed])
      "Notifies support" in (s must haveRun[ServerOp].ops2[GetMsgAssignWorker, NotifySupportTaskmanError])
    }

    "Taskman crashes post-work" >> {
      val (r, s) = test(crashOnUpdateMsgSuccess compose assignWorkerAllow, fpRetry, mpNop)
      "Result"           in (r must haveResultS[TaskmanFailed])
      "Notifies support" in (s must haveRun[ServerOp].ops3[GetMsgAssignWorker, UpdateMsgSuccess, NotifySupportTaskmanError])
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  "Worker processing msgs asynchronously" >> {

    def blah(fx     : Fx[Unit],
             clock  : Fx[Instant] = clockReal,
             sopEndo: Endo[MockSops] = assignWorkerAllow) = {
      val scheduler = new AsyncScheduler[Need] { def apply[A](io: Fx[A]) = Fx(Need(io.unsafeRun())) }
      val prComplete: ProcessorResult[Need] = ProcessorResult.Complete
      val schedule = ProcessorResult.Schedule(scheduler, fx.map(_ => prComplete))
      val mp: MsgProcessor[Need] = _ => Fx(schedule)
      def run = {
        val mockSop = sopEndo(new MockSops)
        val w = new Worker(mp)(nid, wid, mockSop, tp, clock, fpRetry)
        val r: R = w.process(mh_1).unsafeRun()
        (r, mockSop)
      }
      def runFuture(r1: R) = r1.asInstanceOf[Scheduled[Need]].f.value
      (run, run map1 runFuture)
    }

    def longClock = everIncClock(tp.value plusSeconds 1)

    "Work completes" >> {
      val ((r1, s1), (r2, s2)) = blah(Fx.unit)
      "Immediate result"             in (r1 must haveResultA)
      "Assigns msg before future"    in (s1 must haveRun[ServerOp].op[GetMsgAssignWorker])
      "Future result"                in (r2 must haveResultS[Completed])
      "Future marks msg as complete" in (s2 must haveRun[ServerOp].ops2[GetMsgAssignWorker, UpdateMsgSuccess])
    }

    "Future crashes" >> {
      val ((r1, s1), (r2, s2)) = blah(Fx(???))
      "Immediate result"           in (r1 must haveResultA)
      "Assigns msg before future"  in (s1 must haveRun[ServerOp].op[GetMsgAssignWorker])
      "Future result"              in (r2 must haveResultS[WorkerFailed])
      "Future marks msg as failed" in (s2 must haveRun[ServerOp].ops2[GetMsgAssignWorker, UpdateMsgAbort])
    }

    "Reassigns and completes" >> {
      val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock)
      "Immediate result"             in (r1 must haveResultA)
      "Assigns msg before future"    in (s1 must haveRun[ServerOp].op[GetMsgAssignWorker])
      "Future result"                in (r2 must haveResultS[Completed])
      "Future marks msg as complete" in (s2 must haveRun[ServerOp].ops3[GetMsgAssignWorker, ReassignWorker, UpdateMsgSuccess])
    }

    "Future fails to reassign worker" >> {
      val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerDeny)
      "Immediate result"                             in (r1 must haveResultA)
      "Assigns msg before future"                    in (s1 must haveRun[ServerOp].op[GetMsgAssignWorker])
      "Future result"                                in (r2 must haveResultS[CouldntReassign])
      "Future does nothing after reassignment fails" in (s2 must haveRun[ServerOp].ops2[GetMsgAssignWorker, ReassignWorker])
    }

    "Future encounters taskman error" >> {
      val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerCrash)
      "Immediate result"          in (r1 must haveResultA)
      "Assigns msg before future" in (s1 must haveRun[ServerOp].op[GetMsgAssignWorker])
      "Future result"             in (r2 must haveResultS[TaskmanFailed])
    "Future notifies support"   in (s2 must haveRun[ServerOp].ops3[GetMsgAssignWorker, ReassignWorker, NotifySupportTaskmanError])
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  "Worker.FailureHandler" >> {
    "handleFailedWorker" should {
      def test(archive: Boolean)(implicit bop: MockBops) = {
        new FailureHandler(mockEmails(archive)).handleFailedWorker(sampleNotifySupportWorkerFailed).unsafeRun()
        bop
      }

      "notify support" in {
        implicit val bop = new MockBops
        test(false) must haveRun[BusinessOp].op[SupportOp[ReportFailure]]
      }

      "send archive email" in {
        implicit val bop = new MockBops
        test(true) must haveRun[BusinessOp].ops2[SupportOp[ReportFailure], SendEmail]
      }

      "raise a taskman error if fails to notify support" in {
        implicit val bop = crashOnReportFailure(new MockBops)
        test(true) must haveRun[BusinessOp].ops4[SupportOp[ReportFailure], SendEmail, SupportOp[ReportFailure], SendEmail]
      }
    }

    "handleFailedTaskman" should {
      def test(archive: Boolean)(implicit bop: MockBops) = {
        new FailureHandler(mockEmails(archive)).handleFailedTaskman(sampleNotifySupportTaskmanError).unsafeRun()
        bop
      }

      "notify support" in {
        implicit val bop = new MockBops
        test(false) must haveRun[BusinessOp].op[SupportOp[ReportFailure]]
      }

      "send archive email" in {
        implicit val bop = new MockBops
        test(true) must haveRun[BusinessOp].ops2[SupportOp[ReportFailure], SendEmail]
      }

      "recover if unable to notify support" in {
        implicit val bop = crashOnReportFailure(new MockBops)
        test(true) must haveRun[BusinessOp].ops2[SupportOp[ReportFailure], SendEmail]
      }
    }
  }
}
