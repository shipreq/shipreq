package shipreq.taskman.server

import java.time.{Duration, Instant, ZoneId}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import scala.reflect.ClassTag
import scalaz.Lens.lensg
import scalaz.{Endo, Heap, NonEmptyList, Order}
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.FxModule._
import shipreq.base.util.effect._
import shipreq.base.test.{MockOpTransformer, MockOpTransformerA, OpTypeProvider}
import shipreq.taskman.api.{EmailAddr, MsgId, Priority, UserId}
import shipreq.taskman.api.Msg.{LandingPageHit, ReRegistrationAttempted}
import shipreq.taskman.server.business.{Bop, Email, Emails, MailingList, ShipReqUser, Support}
import shipreq.taskman.server.business.Email.Addr
import Bop._
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
    def ++(as: Seq[MsgHeader]) = j.mod(i => (i /: as)((q,a) => q + a))
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

  val sampleNotifySupportWorkerFailed = NotifySupportWorkerFailed(timeNow, md_1, Error("WORKED FAILED"))
  val sampleNotifySupportTaskmanError = NotifySupportTaskmanError(timeNow, Error("WORKED FAILED"), Some(md_1))

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
      val msgL = lensg[FailureCtx, MsgDetail](c => md => c.copy(m = md), _.m)
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

  val crashOnSendEmail     = endoMod[MockBops](_.sendEmail << ErrorOr.error("CRASH!"))
  val crashOnReportFailure = endoMod[MockBops](_.supReportFailure << ErrorOr.error("CRASH!"))

  val clockReal = Fx.now

  val fpRetry: FailurePolicy =
    f => FailureResponse(UpdateMsgAbort(f.n, f.w, f.m), Nil)

  val fpRetrySupport: FailurePolicy =
    f => FailureResponse(UpdateMsgAbort(f.n, f.w, f.m), NotifySupportWorkerFailed(timeNow, f.m, f.err) :: Nil)

  val fpAbort: FailurePolicy =
    f => FailureResponse(UpdateMsgRetry(f.n, f.w, f.m, Duration ofDays 1), Nil)

  def mpNop[F[_]]: MsgProcessor[F] = _ => FxE(ProcessorResult.Complete)
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

  override def cotrans[A] = {
    case _: CfgGet                    => cfgGetR.pop()
    case _: GetMsgsAssignNode         => assignNodeR.pop()
    case _: GetMsgAssignWorker        => assignWorkerR.pop()
    case _: UpdateMsgSuccess          => updateMsgSuccessR.pop()
    case _: UpdateMsgAbort            => updateMsgAbortR.pop()
    case _: UpdateMsgRetry            => updateMsgRetryR.pop()
    case _: NotifySupportWorkerFailed => notifySupportWorkerFailedR.pop()
    case _: NotifySupportTaskmanError => notifySupportTaskmanErrorR.pop()
    case _: ReassignWorker            => reassignWorkerR.pop()
    case    Nop                       =>
  }
}

// =====================================================================================================================

object BopTypeTags extends OpTypeProvider[Bop] {
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

class MockBops extends MockOpTransformer[Bop, FxE] {
  override def opTypeProvider = BopTypeTags

  val sendEmail            = MockResponse(ErrorOr.unit)
  val findShipReqUser      = MockResponse[Option[ShipReqUser]](None)
  val findShipReqUsers     = MockResponse[List[ShipReqUser]](Nil)
  val mlGetListId          = MockResponse[Option[ListId]](None)
  val mlSubscribe          = MockResponse[SubscribeResult](Ok)
  val mlUpdateMember       = MockResponse[UpdateMemberResult](Ok)
  val mlBatchSubscribe     = MockResponse(ErrorOr.unit)
  val supNotifyLandingPage = MockResponse[TicketId](TicketId(666))
  val supReportFailure     = MockResponse(ErrorOr(TicketId(200)))

  override def trans[A] = {
    case _: SendEmail                     => Fx(sendEmail.pop())
    case _: FindShipReqUser               => FxE(findShipReqUser.pop())
    case _: FindShipReqUsers              => FxE(findShipReqUsers.pop())
    case MailingListOp(_: GetListId)      => FxE(mlGetListId.pop())
    case MailingListOp(_: Subscribe)      => FxE(mlSubscribe.pop())
    case MailingListOp(_: UpdateMember)   => FxE(mlUpdateMember.pop())
    case MailingListOp(_: BatchSubscribe) => Fx(mlBatchSubscribe.pop())
    case SupportOp(o) => o match {
      case _: NotifyLandingPage           => FxE(supNotifyLandingPage.pop())
      case _: ReportFailure               => Fx(supReportFailure.pop())
    }
//    case SupportOp(_: NotifyLandingPage)  => FxE(supNotifyLandingPage.pop())
//    case SupportOp(_: ReportFailure)      => Fx(supReportFailure.pop())
  }
}
