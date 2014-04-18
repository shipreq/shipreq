package shipreq.taskman.server

import org.joda.time.Period
import scala.slick.session.{Database, Session}
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.traverse._
import shipreq.base.util.{ErrorOr, StringBasedValueReader}
import shipreq.base.util.ExternalValueReader.Retriever
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.api.impl.Serialisation
import shipreq.taskman.server.business.{Failure, BopReifier, Emails}
import Sql.{FailAndAbort, Succeeded}

object SopImpl {

  class Dao(session: Session) {
    import Sql._
    private[this] implicit def _s = session

    def getMsgsAssignNode(node: NodeId, limit: Int, assignmentTrustPeriod: Period, queued: Option[(Priority, Int)]): List[MsgHeader] =
      queued match {
        case None =>
          // Empty mem-queue
          getMsgsAssignNodeZ.list(node, assignmentTrustPeriod, limit)

        case Some((memPri, memSize)) =>
          val freeSlots = limit - memSize
          if (freeSlots > 0)
            // Partial mem-queue
            getMsgsAssignNodeP.list(assignmentTrustPeriod, limit, freeSlots, memPri, node)
          else
            // Full mem-queue
            getMsgsAssignNodeF.list(node, assignmentTrustPeriod, memPri, limit)
      }

    def getMsgAssignWorker(node: NodeId, worker: WorkerId, hdr: MsgHeader): Option[MsgDetail] =
      getMsgAssignWorkerQ.firstOption(worker, hdr.id, node) map {
        case (msgType, msgData, failureCount) =>
          ErrorOr.require_!(
            Serialisation.deserialise(msgType, msgData).map(msg =>
              MsgDetail(hdr, msg, failureCount)))
      }

    def reassignWorker(n: NodeId, w: WorkerId, m: MsgId): Boolean =
      reassignWorkerQ.firstOption(n, w, m) getOrElse false

    def failAndRetry(msg: MsgId, delay: Period): Unit =
      failAndRetryQ.execute(delay, msg)
    
    def archiveMsg(msg: MsgId, status: ArchiveIntent): Unit =
      archiveMsgQ.execute(msg, status)

    def getNextNodeId: NodeId =
      getNextNodeIdQ.first()

    def cfgGet(k: String): Option[String] =
      cfgGetQ.firstOption(k)
  }

  def cfgValueReader(db: Database) =
    new StringBasedValueReader(
      new Retriever[String](k =>
        ErrorOr.safe(db.withSession((s: Session) => new Dao(s).cfgGet(k)))
          .sequence))
}

// =====================================================================================================================

class SopImpl[EA](db: Database, emails: Emails, bopReifier: BopReifier) extends SopReifier {
  import Sop._
  import SopImpl._

  private[this] val failedWorkerHandler = Failure.handleFailedWorker(emails, bopReifier, this)
  private[this] val failedTaskmanHandler = Failure.handleFailedTaskman(emails, bopReifier)
  private[this] def ioD[A](f: Dao => A): IO[A] = IO(db.withSession(s => f(new Dao(s))))

  def getNextNodeId = ioD(_.getNextNodeId)

  override def apply[A](op: Sop[A]): IO[A] = op match {

    case GetMsgsAssignNode(node, limit, trustPeriod, queued) =>
      ioD(_.getMsgsAssignNode(node, limit, trustPeriod, queued))

    case GetMsgAssignWorker(node, worker, hdr) =>
      ioD(_.getMsgAssignWorker(node, worker, hdr))

    case ReAssignWorker(n, w, m) =>
      ioD(_.reassignWorker(n, w, m))

    case UpdateMsgAbort(m, delay) =>
      ioD(_.failAndRetry(m, delay))

    case UpdateMsgSuccess(m) =>
      ioD(_.archiveMsg(m, Succeeded))

    case UpdateMsgRetry(m) =>
      ioD(_.archiveMsg(m, FailAndAbort))

    case CfgGet(k) =>
      ioD(_ cfgGet k)

    case op: NotifySupportWorkerFailed =>
      failedWorkerHandler(op)

    case op: NotifySupportTaskmanError =>
      failedTaskmanHandler(op)

    case Nop =>
      nopIo
  }
}
