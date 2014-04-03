package shipreq.taskman.server

import org.joda.time.Period
import scala.slick.session.{Database, Session}
import scalaz.effect.IO
import shipreq.base.util.ErrorOr
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.api.impl.Serialisation
import Sql.{FailAndAbort, Succeeded}

object SopImpl {

  class Dao(implicit session: Session) {
    import Sql._

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

    def failAndRetry(msg: MsgId, delay: Period): Unit =
      failAndRetryQ.execute(delay, msg)
    
    def archiveMsg(msg: MsgId, status: ArchiveIntent): Unit =
      archiveMsgQ.execute(msg, status)

    def getNextNodeId: NodeId =
      getNextNodeIdQ.first()

    def cfgGet(k: String): Option[String] =
      cfgGetQ.firstOption(k)
  }
}

// =====================================================================================================================

class SopImpl(db: Database) extends SopReifier {
  import Sop._
  import SopImpl._

  val nop = IO(())

  private[this] def ioD[A](f: Dao => A): IO[A] =
    IO(db.withSession(implicit s => f(new Dao())))

  def getNextNodeId = ioD(_.getNextNodeId)

  override def apply[A](op: Sop[A]): IO[A] = op match {

    case GetMsgsAssignNode(node, limit, trustPeriod, queued) =>
      ioD(_.getMsgsAssignNode(node, limit, trustPeriod, queued))

    case GetMsgAssignWorker(node, worker, hdr) =>
      ioD(_.getMsgAssignWorker(node, worker, hdr))

    case MsgFailedRetry(m, delay) =>
      ioD(_.failAndRetry(m, delay))

    case MarkMsgComplete(m) =>
      ioD(_.archiveMsg(m, Succeeded))

    case MsgFailedAbort(m) =>
      ioD(_.archiveMsg(m, FailAndAbort))

    case CfgGet(k) =>
      ioD(_ cfgGet k)

    case Nop =>
      nop

    /*
    case class NotifySupportWorkerFailed(m: MsgDetail, e: Error) extends Sop[Unit]
    case class NotifySupportTaskmanError(e: Error, m: Option[MsgDetail]) extends Sop[Unit]
     */
  }
}
