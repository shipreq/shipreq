package shipreq.taskman.server

import org.joda.time.{Period, DateTime}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.specs2.matcher.Matcher
import org.specs2.matcher.AnyMatchers._
import scalaz.Lens.lensg
import scalaz.{NonEmptyList, Heap, Order, Endo}
import scalaz.effect.IO
import shipreq.base.util.{ErrorOr, Error}
import shipreq.base.test.{MockOpTransformerA, MockOpTransformer}
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import shipreq.taskman.server.business.{Emails, Bop, Email}
import Bop._
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

  val sampleNotifySupportWorkerFailed = NotifySupportWorkerFailed(timeNow, md_1, Error("WORKED FAILED"))
  val sampleNotifySupportTaskmanError = NotifySupportTaskmanError(timeNow, Error("WORKED FAILED"), Some(md_1))

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
    f => FailureResponse(UpdateMsgRetry(f.m), Nil)

  val fpRetrySupport: FailurePolicy =
    f => FailureResponse(UpdateMsgRetry(f.m), NotifySupportWorkerFailed(timeNow, f.m, f.err) :: Nil)

  val fpAbort: FailurePolicy =
    f => FailureResponse(UpdateMsgAbort(f.m, Period days 1), Nil)

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

  object MockEmailCtx extends Email.Ctx[String] {
    override def addrParser = identity
    override val publicFrom = "publicFrom"
    override val supportEnv = Email.Envelope("Support.From", NonEmptyList("Support.To"))
    override val shipreq = "shipreq"
    override val loginUrl = "loginUrl"
  }
  val MockEmails = new Emails(MockEmailCtx)

  def haveRunBops(expBops: Class[_ <: Bop[_]]*): Matcher[MockBops] =
    beEqualTo(expBops.toList) ^^ {(b: MockBops) => b.allOpClasses}

  def haveRunOps(expSops: Class[_ <: Sop[_]]*)(expBops: Class[_ <: Bop[_]]*): Matcher[(MockSops, MockBops)] =
    beEqualTo((expSops.toList, expBops.toList)) ^^ {(t: (MockSops, MockBops)) => (t._1.allOpClasses, t._2.allOpClasses)}
}

class MockSops extends MockOpTransformerA[Sop, IO] {
  val cfgGetR = MockResponse(Option[String](null))
  val assignNodeR = MockResponse(Seq.empty[MsgHeader])
  val assignWorkerR = MockResponse(Option[MsgDetail](null))
  val updateMsgSuccessR = MockResponse(())
  val updateMsgRetryR = MockResponse(())
  val updateMsgAbortR = MockResponse(())
  val notifySupportWorkerFailedR = MockResponse(())
  val notifySupportTaskmanErrorR = MockResponse(())
  val reassignWorkerR = MockResponse(true)

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

class MockBops extends MockOpTransformer[Bop, IOE] {
  val sendEmailR = MockResponse(ErrorOr.unit)

  override def trans[A] = {
    case _: SendEmail[_] => IO(sendEmailR.pop())
  }
}
