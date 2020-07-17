package shipreq.taskman.server.logic

import java.time.{Duration, Instant}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scala.reflect.ClassTag
import scalaz.Lens.lensg
import scalaz.{Endo, Heap}
import shipreq.base.test.{MockOpTransformer, MockOpTransformerA, OpTypeProvider}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.taskman.api.Task.{LandingPageHit, ReRegistrationAttempted}
import shipreq.taskman.api.{EmailAddr, Priority, TaskId, UserId}
import shipreq.taskman.server.logic.Manager.{JobQueue, PrioritisationOrderZ}
import shipreq.taskman.server.logic.ServerOp._
import shipreq.taskman.server.logic.Worker._
import shipreq.taskman.server.logic.business.BusinessOp._
import shipreq.taskman.server.logic.business.Email.Addr
import shipreq.taskman.server.logic.business.MailingList.API._
import shipreq.taskman.server.logic.business.MailingList._
import shipreq.taskman.server.logic.business.Support.API._
import shipreq.taskman.server.logic.business.Support._
import shipreq.taskman.server.logic.business._

object TestHelpers {

  implicit class JobQueueExt(val j: JobQueue) {
    def mod(f: Heap[TaskHeader] => Heap[TaskHeader]) = JobQueue(f(j.q))
    def -(a: TaskHeader) = j.mod(_.filter(_ != a))
    def +(a: TaskHeader) = j.mod(_ insert a)
    def ++(as: Seq[TaskHeader]) = j.mod(i => as.foldLeft(i)(_ + _))
  }

  final def endoMod[A](f: A => Unit) = Endo[A](a => {f(a); a})

  val timeNow = Instant.now()
  val timePast = timeNow minusSeconds 600

  val sampleEmailAddr = EmailAddr("test@hehe.com")
  val task_rereg = ReRegistrationAttempted(sampleEmailAddr)
  val node1 = NodeId(1)
  val worker2 = WorkerId(2)
  val sampleUserId = UserId(30)
  val th_1 = TaskHeader(TaskId(1), Priority(6), timeNow)
  val td_1 = TaskDetail(th_1, task_rereg, 0)
  val th_2 = TaskHeader(TaskId(2), Priority(5), timePast)

  val sampleShipReqUser = ShipReqUser(sampleUserId, "usrnm", sampleEmailAddr, "Bob Bobb", true)

  val sampleNotifySupportWorkerFailed = NotifySupportWorkerFailed(timeNow, td_1, ArticulateError("WORKED FAILED"))
  val sampleNotifySupportTaskmanError = NotifySupportTaskmanError(timeNow, ArticulateError("WORKED FAILED"), Some(td_1))

  val sampleLP = LandingPageHit(sampleEmailAddr, "Bob", None, true, Some("1.1.1.1"))

  object lenses {
    object taskDetail {
      val failureCountL = lensg[TaskDetail, Int](m => f => m.copy(failureCount = f.toShort), _.failureCount)
      val headerL       = lensg[TaskDetail, TaskHeader](m => h => m.copy(hdr = h), _.hdr)
      val priorityL     = headerL >=> taskHeader.priorityL
      val createdL      = headerL >=> taskHeader.createdL
    }
    object taskHeader {
      val priorityL = lensg[TaskHeader, Priority](m => p => m.copy(priority = p), _.priority)
      val createdL  = lensg[TaskHeader, Instant](m => c => m.copy(created = c), _.created)
    }
    object failureCtx {
      val taskDetailL   = lensg[FailureCtx, TaskDetail](c => md => c.copy(taskDetail = md), _.taskDetail)
      val failureCountL = taskDetailL >=> taskDetail.failureCountL
      val priorityL     = taskDetailL >=> taskDetail.priorityL
      val createdL      = taskDetailL >=> taskDetail.createdL
    }
  }

  def assignWorkerTo(td: TaskDetail)   = endoMod[MockSops](_.assignWorkerR << Some(td))
  val assignWorkerAllow                = assignWorkerTo(td_1)
  val assignWorkerCrash                = endoMod[MockSops](_.assignWorkerR << ???)
  val crashOnUpdateMsgSuccess          = endoMod[MockSops](_.updateMsgSuccessR << ???)
  val crashOnNotifySupportWorkerFailed = endoMod[MockSops](_.notifySupportWorkerFailedR << ???)
  val crashOnNotifySupportTaskmanError = endoMod[MockSops](_.notifySupportTaskmanErrorR << ???)
  val reassignWorkerDeny               = endoMod[MockSops](_.reassignWorkerR << false)
  val reassignWorkerCrash              = endoMod[MockSops](_.reassignWorkerR << ???)

  val crashOnSendEmail     = endoMod[MockBops](_.sendEmail << -\/(ArticulateError("CRASH!")))
  val crashOnReportFailure = endoMod[MockBops](_.supReportFailure << -\/(ArticulateError("CRASH!")))

  val clockReal = Fx.now

  val fpRetry: FailurePolicy =
    f => FailureResponse(UpdateTaskAbort(f.node, f.worker, f.taskDetail), Nil)

  val fpRetrySupport: FailurePolicy =
    f => FailureResponse(UpdateTaskAbort(f.node, f.worker, f.taskDetail), NotifySupportWorkerFailed(timeNow, f.taskDetail, f.err) :: Nil)

  val fpAbort: FailurePolicy =
    f => FailureResponse(UpdateTaskRetry(f.node, f.worker, f.taskDetail, Duration ofDays 1), Nil)

  def mpNop[F[_]]: Processor[F] = _ => Fx(ProcessorResult.Complete)
  def mpCrash[F[_]]: Processor[F] = _ => ???

  def arbMap[B, A](f: A => B)(implicit a: Arbitrary[A]): Arbitrary[B] =
    Arbitrary { a.arbitrary.map(f) }

  implicit def arbitraryMsgId = arbMap[TaskId, Long](new TaskId(_))
  implicit def arbitraryPriority = arbMap[Priority, Short](new Priority(_))

  implicit def arbitraryInstant = Arbitrary[Instant](
    Gen.chooseNum(0L, 3000000000000L).map(Instant.ofEpochSecond))

  implicit def arbitraryMsgHeader: Arbitrary[TaskHeader] =
    Arbitrary(for {
      i <- arbitrary[TaskId]
      p <- arbitrary[Priority]
      c <- arbitrary[Instant]
    } yield
      TaskHeader(i,p,c))

  implicit def arbitraryJobQueue = arbMap[JobQueue, List[TaskHeader]](Manager.empty ++ _)

  def mockEmailEnvelopeProps(archive: Boolean): Email.EnvelopeProps = {
    implicit def autoParseEa(ea: String): Addr = Addr(EmailAddr(ea))
    Email.EnvelopeProps("publicFrom", if (archive) List[Addr]("archiveAddr") else Nil)
  }

  def mockEmailTokenValues =
    Email.TokenValues(shipreqName = "shipreq", loginUrl = "loginUrl")

  def mockEmails(archive: Boolean) =
    new Emails(mockEmailEnvelopeProps(archive), mockEmailTokenValues)

  def manifest[T](implicit m: ClassTag[T]) = m
}

import TestHelpers.manifest

// =====================================================================================================================

object SopTypeTags extends OpTypeProvider[ServerOp] {
  override def apply[A] = {
    case _: CfgGet                     => manifest[CfgGet]
    case _: GetTasksAssignNode         => manifest[GetTasksAssignNode]
    case _: GetTaskAssignWorker        => manifest[GetTaskAssignWorker]
    case _: UpdateTaskSuccess          => manifest[UpdateTaskSuccess]
    case _: UpdateTaskAbort            => manifest[UpdateTaskAbort]
    case _: UpdateTaskRetry            => manifest[UpdateTaskRetry]
    case _: NotifySupportWorkerFailed  => manifest[NotifySupportWorkerFailed]
    case _: NotifySupportTaskmanError  => manifest[NotifySupportTaskmanError]
    case _: ReassignWorker             => manifest[ReassignWorker]
    case    Nop                        => manifest[Nop.type]
  }
}

class MockSops extends MockOpTransformerA[ServerOp, Fx] {
  override def opTypeProvider = SopTypeTags

  val cfgGetR                    = MockResponse(Option[String](null))
  val assignNodeR                = MockResponse(List.empty[TaskHeader])
  val assignWorkerR              = MockResponse(Option[TaskDetail](null))
  val updateMsgSuccessR          = MockResponse(())
  val updateMsgAbortR            = MockResponse(())
  val updateMsgRetryR            = MockResponse(())
  val notifySupportWorkerFailedR = MockResponse(())
  val notifySupportTaskmanErrorR = MockResponse(())
  val reassignWorkerR            = MockResponse(true)

  override def cotrans[A]: ServerOp[A] => A = {
    case _: CfgGet                     => cfgGetR.pop()
    case _: GetTasksAssignNode         => assignNodeR.pop()
    case _: GetTaskAssignWorker        => assignWorkerR.pop()
    case _: UpdateTaskSuccess          => updateMsgSuccessR.pop()
    case _: UpdateTaskAbort            => updateMsgAbortR.pop()
    case _: UpdateTaskRetry            => updateMsgRetryR.pop()
    case _: NotifySupportWorkerFailed  => notifySupportWorkerFailedR.pop()
    case _: NotifySupportTaskmanError  => notifySupportTaskmanErrorR.pop()
    case _: ReassignWorker             => reassignWorkerR.pop()
    case    Nop                        => ()
  }
}

// =====================================================================================================================

object BopTypeTags extends OpTypeProvider[BusinessOp] {
  override def apply[A] = {
    case _: SendEmail                     => manifest[SendEmail]
    case _: FindShipReqUser               => manifest[FindShipReqUser]
    case _: FindShipReqUsers              => manifest[FindShipReqUsers]
    case MailingListOp(_: GetListId)      => manifest[MailingListOp[GetListId]]
    case MailingListOp(_: Subscribe)      => manifest[MailingListOp[Subscribe]]
    case MailingListOp(_: UpdateMember)   => manifest[MailingListOp[UpdateMember]]
    case MailingListOp(_: BatchSubscribe) => manifest[MailingListOp[BatchSubscribe]]
    case SupportOp(o) => o match {
      case _: NotifyLandingPage           => manifest[SupportOp[NotifyLandingPage]]
      case _: RecordUserFeedback          => manifest[SupportOp[RecordUserFeedback]]
      case _: ReportFailure               => manifest[SupportOp[ReportFailure]]
    }
//    case SupportOp(_: NotifyLandingPage)  => manifest[SupportOp[NotifyLandingPage]]
//    case SupportOp(_: ReportFailure)      => manifest[SupportOp[ReportFailure]]
  }
}

class MockBops extends MockOpTransformer[BusinessOp, Fx] {
  override def opTypeProvider = BopTypeTags

  val sendEmail             = MockResponse[Throwable \/ Unit](\/-(()))
  val findShipReqUser       = MockResponse[Option[ShipReqUser]](None)
  val findShipReqUsers      = MockResponse[List[ShipReqUser]](Nil)
  val mlGetListId           = MockResponse[Option[ListId]](None)
  val mlSubscribe           = MockResponse[SubscribeResult](Ok)
  val mlUpdateMember        = MockResponse[UpdateMemberResult](Ok)
  val mlBatchSubscribe      = MockResponse[Throwable \/ Unit](\/-(()))
  val supNotifyLandingPage  = MockResponse[TicketId](TicketId(666))
  val supRecordUserFeedback = MockResponse[TicketId](TicketId(667))
  val supReportFailure      = MockResponse[Throwable \/ TicketId](\/-(TicketId(200)))

  override def trans[A]: BusinessOp[A] => Fx[A] = {
    case _: SendEmail                     => Fx.lift(sendEmail.pop())
    case _: FindShipReqUser               => Fx(findShipReqUser.pop())
    case _: FindShipReqUsers              => Fx(findShipReqUsers.pop())
    case MailingListOp(_: GetListId)      => Fx(mlGetListId.pop())
    case MailingListOp(_: Subscribe)      => Fx(mlSubscribe.pop())
    case MailingListOp(_: UpdateMember)   => Fx(mlUpdateMember.pop())
    case MailingListOp(_: BatchSubscribe) => Fx.lift(mlBatchSubscribe.pop())
    case SupportOp(o) => o match {
      case _: NotifyLandingPage           => Fx(supNotifyLandingPage.pop())
      case _: RecordUserFeedback          => Fx(supRecordUserFeedback.pop())
      case _: ReportFailure               => Fx.lift(supReportFailure.pop())
    }
//    case SupportOp(_: NotifyLandingPage)  => FxE(supNotifyLandingPage.pop())
//    case SupportOp(_: ReportFailure)      => Fx(supReportFailure.pop())
  }
}
