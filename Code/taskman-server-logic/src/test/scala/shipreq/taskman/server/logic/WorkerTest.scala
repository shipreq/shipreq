package shipreq.taskman.server.logic

import cats.{Endo, Eval}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Clock, Duration, Instant}
import scala.reflect.ClassTag
import shipreq.base.util.FxModule._
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.TestHelpers._
import shipreq.taskman.server.logic.Worker.WorkResult._
import shipreq.taskman.server.logic.Worker._
import shipreq.taskman.server.logic.business.BusinessOp.{SendEmail, SupportOp}
import shipreq.taskman.server.logic.business.Support.API.ReportFailure
import utest._

object WorkerTest extends TestSuite {

  type R = WorkResult[Eval]

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

  def assertResultS[W <: WorkResult[Eval]](r: R)(implicit W: ClassTag[W]): Unit =
    assert(W.runtimeClass.isAssignableFrom(r.getClass))

  def assertResultA(r: R) =
    assertResultS[Scheduled[Eval]](r)

  // -------------------------------------------------------------------------------------------------------------------

  override def tests = Tests {

    "Worker processing tasks synchronously" - {

      def test(sopEndo: Endo[MockSops], fp: FailurePolicy, mp: Processor[Eval]) = {
        val mockSop = sopEndo(new MockSops)
        val w = new Worker(mp)(nid, wid, mockSop, tp, clockReal, fp)
        val fx = w.process(th_1)
        val r: R = fx.unsafeRun()
        (r, mockSop)
      }

      "Work completes" - {
        val (r,s) = test(assignWorkerAllow, fpRetry, mpNop)
        "Result"                 - assertResultS[Completed](r)
        "Marks task as complete" - s.assertOpTypes2[GetTaskAssignWorker, UpdateTaskSuccess]
      }

      "Worker crashes (retry)" - {
        val (r, s) = test(assignWorkerAllow, fpRetry, mpCrash)
        "Result"          - assertResultS[WorkerFailed](r)
        "Schedules retry" - s.assertOpTypes2[GetTaskAssignWorker, UpdateTaskAbort]
      }

      "Worker crashes (abort)" - {
        val (r, s) = test(assignWorkerAllow, fpAbort, mpCrash)
        "Result"     - assertResultS[WorkerFailed](r)
        "Aborts job" - s.assertOpTypes2[GetTaskAssignWorker, UpdateTaskRetry]
      }

      "Worker crashes (retry and notify support)" - {
        val (r, s) = test(assignWorkerAllow, fpRetrySupport, mpCrash)
        "Result"                       - assertResultS[WorkerFailed](r)
        "Fails job & notifies support" - s.assertOpTypes3[GetTaskAssignWorker, NotifySupportWorkerFailed, UpdateTaskAbort]
      }

      "Taskman crashes pre-work" - {
        val (r, s) = test(assignWorkerCrash, fpRetry, mpCrash)
        "Result"           - assertResultS[TaskmanFailed](r)
        "Notifies support" - s.assertOpTypes2[GetTaskAssignWorker, NotifySupportTaskmanError]
      }

      "Taskman crashes post-work" - {
        val (r, s) = test(crashOnUpdateMsgSuccess compose assignWorkerAllow, fpRetry, mpNop)
        "Result"           - assertResultS[TaskmanFailed](r)
        "Notifies support" - s.assertOpTypes3[GetTaskAssignWorker, UpdateTaskSuccess, NotifySupportTaskmanError]
      }
    }

    // -------------------------------------------------------------------------------------------------------------------

    "Worker processing tasks asynchronously" - {

      def blah(fx     : Fx[Unit],
               clock  : Fx[Instant] = clockReal,
               sopEndo: Endo[MockSops] = assignWorkerAllow) = {
        val scheduler = new AsyncScheduler[Eval] { def apply[A](io: Fx[A]) = Fx(Eval.later(io.unsafeRun())) }
        val prComplete: ProcessorResult[Eval] = ProcessorResult.Complete
        val schedule = ProcessorResult.Schedule(scheduler, fx.map(_ => prComplete))
        val mp: Processor[Eval] = _ => Fx(schedule)
        def run = {
          val mockSop = sopEndo(new MockSops)
          val w = new Worker(mp)(nid, wid, mockSop, tp, clock, fpRetry)
          val r: R = w.process(th_1).unsafeRun()
          (r, mockSop)
        }
        def runFuture(r1: R) = r1.asInstanceOf[Scheduled[Eval]].fResult.value
        (run, run map1 runFuture)
      }

      def longClock = everIncClock(tp.value plusSeconds 1)

      "Work completes" - {
        val ((r1, s1), (r2, s2)) = blah(Fx.unit)
        "Immediate result"              - assertResultA(r1)
        "Assigns task before future"    - s1.assertOpTypes1[GetTaskAssignWorker]
        "Future result"                 - assertResultS[Completed](r2)
        "Future marks task as complete" - s2.assertOpTypes2[GetTaskAssignWorker, UpdateTaskSuccess]
      }

      "Future crashes" - {
        val ((r1, s1), (r2, s2)) = blah(Fx(???))
        "Immediate result"            - assertResultA(r1)
        "Assigns task before future"  - s1.assertOpTypes1[GetTaskAssignWorker]
        "Future result"               - assertResultS[WorkerFailed](r2)
        "Future marks task as failed" - s2.assertOpTypes2[GetTaskAssignWorker, UpdateTaskAbort]
      }

      "Reassigns and completes" - {
        val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock)
        "Immediate result"              - assertResultA(r1)
        "Assigns task before future"    - s1.assertOpTypes1[GetTaskAssignWorker]
        "Future result"                 - assertResultS[Completed](r2)
        "Future marks task as complete" - s2.assertOpTypes3[GetTaskAssignWorker, ReassignWorker, UpdateTaskSuccess]
      }

      "Future fails to reassign worker" - {
        val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerDeny)
        "Immediate result"                             - assertResultA(r1)
        "Assigns task before future"                    - s1.assertOpTypes1[GetTaskAssignWorker]
        "Future result"                                - assertResultS[CouldntReassign](r2)
        "Future does nothing after reassignment fails" - s2.assertOpTypes2[GetTaskAssignWorker, ReassignWorker]
      }

      "Future encounters taskman error" - {
        val ((r1, s1), (r2, s2)) = blah(Fx.unit, clock = longClock, sopEndo = assignWorkerAllow compose reassignWorkerCrash)
        "Immediate result"           - assertResultA(r1)
        "Assigns task before future" - s1.assertOpTypes1[GetTaskAssignWorker]
        "Future result"              - assertResultS[TaskmanFailed](r2)
        "Future notifies support"    - s2.assertOpTypes3[GetTaskAssignWorker, ReassignWorker, NotifySupportTaskmanError]
      }
    }

    // -------------------------------------------------------------------------------------------------------------------

    "Worker.FailureHandler" - {
      "handleFailedWorker" - {
        def test(archive: Boolean)(implicit bop: MockBops) = {
          new FailureHandler(mockEmails(archive)).handleFailedWorker(sampleNotifySupportWorkerFailed).unsafeRun()
          bop
        }

        "notify support" - {
          implicit val bop = new MockBops
          test(false).assertOpTypes1[SupportOp[ReportFailure]]
        }

        "send archive email" - {
          implicit val bop = new MockBops
          test(true).assertOpTypes2[SupportOp[ReportFailure], SendEmail]
        }

        "raise a taskman error if fails to notify support" - {
          implicit val bop = crashOnReportFailure(new MockBops)
          test(true).assertOpTypes4[SupportOp[ReportFailure], SendEmail, SupportOp[ReportFailure], SendEmail]
        }
      }

      "handleFailedTaskman" - {
        def test(archive: Boolean)(implicit bop: MockBops) = {
          new FailureHandler(mockEmails(archive)).handleFailedTaskman(sampleNotifySupportTaskmanError).unsafeRun()
          bop
        }

        "notify support" - {
          implicit val bop = new MockBops
          test(false).assertOpTypes1[SupportOp[ReportFailure]]
        }

        "send archive email" - {
          implicit val bop = new MockBops
          test(true).assertOpTypes2[SupportOp[ReportFailure], SendEmail]
        }

        "recover if unable to notify support" - {
          implicit val bop = crashOnReportFailure(new MockBops)
          test(true).assertOpTypes2[SupportOp[ReportFailure], SendEmail]
        }
      }
    }
  }

}
