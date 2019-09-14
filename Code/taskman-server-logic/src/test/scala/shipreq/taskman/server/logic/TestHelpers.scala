package shipreq.taskman.server.logic

import java.time.{Duration, Instant, ZoneId}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import scala.reflect.ClassTag
import scalaz.Lens.lensg
import scalaz.{-\/, Endo, Heap, NonEmptyList, Order, \/, \/-}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.test.{MockOpTransformer, MockOpTransformerA, OpTypeProvider}
import shipreq.taskman.api.{EmailAddr, MsgId, Priority, UserId}
import shipreq.taskman.api.Msg.{LandingPageHit, ReRegistrationAttempted}
import shipreq.taskman.server.logic.business._
import shipreq.taskman.server.logic.business.Email.Addr
import BusinessOp._
import ServerOp._
import Manager.{JobQueue, PrioritisationOrderZ}
import Worker._
import MailingList._
import MailingList.API._
import Support._
import Support.API._

object TestHelpers {

  implicit class JobQueueExt(val j: JobQueue) {
    def mod(f: Heap[MsgHeader] => Heap[MsgHeader]) = JobQueue(f(j.q))
    def -(a: MsgHeader) = j.mod(_.filter(_ != a))
    def +(a: MsgHeader) = j.mod(_ insert a)
    def ++(as: Seq[MsgHeader]) = j.mod(i => as.foldLeft(i)(_ + _))
  }

  final def endoMod[A](f: A => Unit) = Endo[A](a => {f(a); a})

  val timeNow = Instant.now()
  val timePast = timeNow minusSeconds 600

  val sampleEmailAddr = EmailAddr("test@hehe.com")
  val msg_rereg = ReRegistrationAttempted(sampleEmailAddr)
  val node1 = NodeId(1)
  val worker2 = WorkerId(2)
  val sampleUserId = UserId(30)
  val mh_1 = MsgHeader(MsgId(1), Priority(6), timeNow)
  val md_1 = MsgDetail(mh_1, msg_rereg, 0)
  val mh_2 = MsgHeader(MsgId(2), Priority(5), timePast)

  val sampleShipReqUser = ShipReqUser(sampleUserId, "usrnm", sampleEmailAddr, "Bob Bobb", true)

  val sampleNotifySupportWorkerFailed = NotifySupportWorkerFailed(timeNow, md_1, ArticulateError("WORKED FAILED"))
  val sampleNotifySupportTaskmanError = NotifySupportTaskmanError(timeNow, ArticulateError("WORKED FAILED"), Some(md_1))

  val sampleLP = LandingPageHit(sampleEmailAddr, "Bob", None, true)

  object lenses {
    object msgDetail {
      val failureCountL = lensg[MsgDetail, Int](m => f => m.copy(failureCount = f.toShort), _.failureCount)
      val headerL = lensg[MsgDetail, MsgHeader](m => h => m.copy(hdr = h), _.hdr)
      val priorityL = headerL >=> msgHeader.priorityL
      val createdL = headerL >=> msgHeader.createdL
    }
    object msgHeader {
      val priorityL = lensg[MsgHeader, Priority](m => p => m.copy(priority = p), _.priority)
      val createdL = lensg[MsgHeader, Instant](m => c => m.copy(created = c), _.created)
    }
    object failureCtx {
      val msgL = lensg[FailureCtx, MsgDetail](c => md => c.copy(msg = md), _.msg)
      val failureCountL = msgL >=> msgDetail.failureCountL
      val priorityL = msgL >=> msgDetail.priorityL
      val createdL = msgL >=> msgDetail.createdL
    }
  }

  def assignWorkerTo(md: MsgDetail)    = endoMod[MockSops](_.assignWorkerR << Some(md))
  val assignWorkerAllow                = assignWorkerTo(md_1)
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
    f => FailureResponse(UpdateMsgAbort(f.node, f.worker, f.msg), Nil)

  val fpRetrySupport: FailurePolicy =
    f => FailureResponse(UpdateMsgAbort(f.node, f.worker, f.msg), NotifySupportWorkerFailed(timeNow, f.msg, f.err) :: Nil)

  val fpAbort: FailurePolicy =
    f => FailureResponse(UpdateMsgRetry(f.node, f.worker, f.msg, Duration ofDays 1), Nil)

  def mpNop[F[_]]: MsgProcessor[F] = _ => Fx(ProcessorResult.Complete)
  def mpCrash[F[_]]: MsgProcessor[F] = _ => ???

  def arbMap[B, A](f: A => B)(implicit a: Arbitrary[A]): Arbitrary[B] =
    Arbitrary { a.arbitrary.map(f) }

  implicit def arbitraryMsgId = arbMap[MsgId, Long](new MsgId(_))
  implicit def arbitraryPriority = arbMap[Priority, Short](new Priority(_))

  implicit def arbitraryInstant = Arbitrary[Instant](
    Gen.chooseNum(0L, 3000000000000L).map(Instant.ofEpochSecond))

  implicit def arbitraryMsgHeader: Arbitrary[MsgHeader] =
    Arbitrary(for {
      i <- arbitrary[MsgId]
      p <- arbitrary[Priority]
      c <- arbitrary[Instant]
    } yield
      MsgHeader(i,p,c))

  implicit def arbitraryJobQueue = arbMap[JobQueue, List[MsgHeader]](Manager.empty ++ _)

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
    case _: CfgGet                    => manifest[CfgGet]
    case _: GetMsgsAssignNode         => manifest[GetMsgsAssignNode]
    case _: GetMsgAssignWorker        => manifest[GetMsgAssignWorker]
    case _: UpdateMsgSuccess          => manifest[UpdateMsgSuccess]
    case _: UpdateMsgAbort            => manifest[UpdateMsgAbort]
    case _: UpdateMsgRetry            => manifest[UpdateMsgRetry]
    case _: NotifySupportWorkerFailed => manifest[NotifySupportWorkerFailed]
    case _: NotifySupportTaskmanError => manifest[NotifySupportTaskmanError]
    case _: ReassignWorker            => manifest[ReassignWorker]
    case    Nop                       => manifest[Nop.type]
  }
}

class MockSops extends MockOpTransformerA[ServerOp, Fx] {
  override def opTypeProvider = SopTypeTags

  val cfgGetR                    = MockResponse(Option[String](null))
  val assignNodeR                = MockResponse(List.empty[MsgHeader])
  val assignWorkerR              = MockResponse(Option[MsgDetail](null))
  val updateMsgSuccessR          = MockResponse(())
  val updateMsgAbortR            = MockResponse(())
  val updateMsgRetryR            = MockResponse(())
  val notifySupportWorkerFailedR = MockResponse(())
  val notifySupportTaskmanErrorR = MockResponse(())
  val reassignWorkerR            = MockResponse(true)

  override def cotrans[A]: ServerOp[A] => A = {
    case _: CfgGet                    => cfgGetR.pop()
    case _: GetMsgsAssignNode         => assignNodeR.pop()
    case _: GetMsgAssignWorker        => assignWorkerR.pop()
    case _: UpdateMsgSuccess          => updateMsgSuccessR.pop()
    case _: UpdateMsgAbort            => updateMsgAbortR.pop()
    case _: UpdateMsgRetry            => updateMsgRetryR.pop()
    case _: NotifySupportWorkerFailed => notifySupportWorkerFailedR.pop()
    case _: NotifySupportTaskmanError => notifySupportTaskmanErrorR.pop()
    case _: ReassignWorker            => reassignWorkerR.pop()
    case    Nop                       => ()
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
      case _: ReportFailure               => manifest[SupportOp[ReportFailure]]
    }
//    case SupportOp(_: NotifyLandingPage)  => manifest[SupportOp[NotifyLandingPage]]
//    case SupportOp(_: ReportFailure)      => manifest[SupportOp[ReportFailure]]
  }
}

class MockBops extends MockOpTransformer[BusinessOp, Fx] {
  override def opTypeProvider = BopTypeTags

  val sendEmail            = MockResponse[Throwable \/ Unit](\/-(()))
  val findShipReqUser      = MockResponse[Option[ShipReqUser]](None)
  val findShipReqUsers     = MockResponse[List[ShipReqUser]](Nil)
  val mlGetListId          = MockResponse[Option[ListId]](None)
  val mlSubscribe          = MockResponse[SubscribeResult](Ok)
  val mlUpdateMember       = MockResponse[UpdateMemberResult](Ok)
  val mlBatchSubscribe     = MockResponse[Throwable \/ Unit](\/-(()))
  val supNotifyLandingPage = MockResponse[TicketId](TicketId(666))
  val supReportFailure     = MockResponse[Throwable \/ TicketId](\/-(TicketId(200)))

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
      case _: ReportFailure               => Fx.lift(supReportFailure.pop())
    }
//    case SupportOp(_: NotifyLandingPage)  => FxE(supNotifyLandingPage.pop())
//    case SupportOp(_: ReportFailure)      => Fx(supReportFailure.pop())
  }
}
