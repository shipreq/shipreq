package shipreq.taskman.server

import org.joda.time.{Period, DateTime}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import scalaz.Lens.lensg
import scalaz.{NonEmptyList, Heap, Order, Endo}
import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.util.effect.IOE
import shipreq.base.test.{OpTypeProvider, MockOpTransformerA, MockOpTransformer}
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.Msg.{LandingPageHit, ReRegistrationAttempted}
import shipreq.taskman.server.business.{MailingList, ShipReqUser, Emails, Bop, Email}
import shipreq.taskman.server.business.Email.Addr
import Bop._
import Sop._
import Manager._
import Worker._
import MailingList._
import MailingList.API._

object TestHelpers {

  implicit class HeapExt[A: Order](val value: Heap[A]) {
    def -(a: A) = value.filter(_ != a)
    def +(a: A) = value insert a
    def ++(as: Seq[A]) = (value /: as)((q,a) => q + a)
  }

  final def endoMod[A](f: A => Unit) = Endo[A](a => {f(a); a})

  val timeNow = DateTime.now()
  val timePast = timeNow minusMinutes 10

  val sampleEmailAddr = "test@hehe.com".tag[IsEmailAddr]
  val msg_rereg = ReRegistrationAttempted(sampleEmailAddr)
  val node1 = NodeId(1)
  val worker2 = WorkerId(2)
  val mh_1 = MsgHeader(MsgId(1), Priority(6), timeNow)
  val md_1 = MsgDetail(mh_1, msg_rereg, 0)
  val mh_2 = MsgHeader(MsgId(2), Priority(5), timePast)

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
      val createdL = lensg[MsgHeader, DateTime](m => c => m.copy(created = c), _.created)
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

  val crashOnSendEmail = endoMod[MockBops](_.sendEmailR << ErrorOr.error("CRASH!"))

  val clockReal = IO(DateTime.now)

  val fpRetry: FailurePolicy =
    f => FailureResponse(UpdateMsgRetry(f.n, f.w, f.m), Nil)

  val fpRetrySupport: FailurePolicy =
    f => FailureResponse(UpdateMsgRetry(f.n, f.w, f.m), NotifySupportWorkerFailed(timeNow, f.m, f.err) :: Nil)

  val fpAbort: FailurePolicy =
    f => FailureResponse(UpdateMsgAbort(f.n, f.w, f.m, Period days 1), Nil)

  def mpNop[F[_]]: MsgProcessor[F] = _ sync IOE.nop
  def mpCrash[F[_]]: MsgProcessor[F] = _ => ???

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

  object MockEmailEnvelopeProps extends Email.EnvelopeProps {
    private[this] implicit def autoParseEa(ea: String): Addr = Addr(ea.tag)
    override val publicFrom: Addr = "publicFrom"
    override val landingPageEnv = Email.EnvelopeFront(NonEmptyList("LP.To"))
    override val supportEnv = Email.Envelope("Support.From", NonEmptyList("Support.To"))
  }

  object MockEmailTokenValues extends Email.TokenValues {
    override val shipreqName = "shipreq"
    override val loginUrl = "loginUrl"
  }

  val MockEmails = new Emails(MockEmailEnvelopeProps, MockEmailTokenValues)

  def manifest[T](implicit m: Manifest[T]) = m
}

import TestHelpers.manifest

// =====================================================================================================================

object SopTypeTags extends OpTypeProvider[Sop] {
  override def apply[A] = {
    case _: CfgGet                    => manifest[CfgGet]
    case _: GetMsgsAssignNode         => manifest[GetMsgsAssignNode]
    case _: GetMsgAssignWorker        => manifest[GetMsgAssignWorker]
    case _: UpdateMsgSuccess          => manifest[UpdateMsgSuccess]
    case _: UpdateMsgRetry            => manifest[UpdateMsgRetry]
    case _: UpdateMsgAbort            => manifest[UpdateMsgAbort]
    case _: NotifySupportWorkerFailed => manifest[NotifySupportWorkerFailed]
    case _: NotifySupportTaskmanError => manifest[NotifySupportTaskmanError]
    case _: ReAssignWorker            => manifest[ReAssignWorker]
    case    Nop                       => manifest[Nop.type]
  }
}

class MockSops extends MockOpTransformerA[Sop, IO] {
  override def opTypeProvider = SopTypeTags

  val cfgGetR                    = MockResponse(Option[String](null))
  val assignNodeR                = MockResponse(Seq.empty[MsgHeader])
  val assignWorkerR              = MockResponse(Option[MsgDetail](null))
  val updateMsgSuccessR          = MockResponse(())
  val updateMsgRetryR            = MockResponse(())
  val updateMsgAbortR            = MockResponse(())
  val notifySupportWorkerFailedR = MockResponse(())
  val notifySupportTaskmanErrorR = MockResponse(())
  val reassignWorkerR            = MockResponse(true)

  override def cotrans[A] = {
    case _: CfgGet                    => cfgGetR.pop()
    case _: GetMsgsAssignNode         => assignNodeR.pop()
    case _: GetMsgAssignWorker        => assignWorkerR.pop()
    case _: UpdateMsgSuccess          => updateMsgSuccessR.pop()
    case _: UpdateMsgRetry            => updateMsgRetryR.pop()
    case _: UpdateMsgAbort            => updateMsgAbortR.pop()
    case _: NotifySupportWorkerFailed => notifySupportWorkerFailedR.pop()
    case _: NotifySupportTaskmanError => notifySupportTaskmanErrorR.pop()
    case _: ReAssignWorker            => reassignWorkerR.pop()
    case    Nop                       =>
  }
}

// =====================================================================================================================

object BopTypeTags extends OpTypeProvider[Bop] {
  override def apply[A] = {
    case _: SendEmail                     => manifest[SendEmail]
    case _: LookupShipReqUser             => manifest[LookupShipReqUser]
    case MailingListOp(_: GetListId)      => manifest[MailingListOp[GetListId]]
    case MailingListOp(_: Subscribe)      => manifest[MailingListOp[Subscribe]]
    case MailingListOp(_: UpdateMember)   => manifest[MailingListOp[UpdateMember]]
    case MailingListOp(_: BatchSubscribe) => manifest[MailingListOp[BatchSubscribe]]
  }
}

class MockBops extends MockOpTransformer[Bop, IOE] {
  override def opTypeProvider = BopTypeTags

  val sendEmailR         = MockResponse(ErrorOr.unit)
  val lookupShipReqUserR = MockResponse[Option[ShipReqUser]](None)
  val mlGetListId        = MockResponse[Option[ListId]](None)
  val mlSubscribe        = MockResponse[Option[SubscribeFail]](None)
  val mlUpdateMember     = MockResponse[Option[UpdateMemberFail]](None)
  val mlBatchSubscribe   = MockResponse(ErrorOr.unit)

  override def trans[A] = {
    case _: SendEmail                     => IO(sendEmailR.pop())
    case _: LookupShipReqUser             => IOE(lookupShipReqUserR.pop())
    case MailingListOp(_: GetListId)      => IOE(mlGetListId.pop())
    case MailingListOp(_: Subscribe)      => IOE(mlSubscribe.pop())
    case MailingListOp(_: UpdateMember)   => IOE(mlUpdateMember.pop())
    case MailingListOp(_: BatchSubscribe) => IO(mlBatchSubscribe.pop())
  }
}
