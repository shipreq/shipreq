package shipreq.taskman.server

import java.time.Duration
import scala.slick.jdbc.JdbcBackend.{Database, Session}
import scalaz.effect.IO
import scalaz.std.option.optionInstance
import scalaz.syntax.traverse._
import shipreq.base.util.{ErrorOr, StringBasedValueReader}
import shipreq.base.util.ExternalValueReader.Retriever
import shipreq.base.util.effect.IoUtils
import shipreq.taskman.api.{MsgId, Msg, Priority}
import shipreq.taskman.api.impl.Serialisation
import shipreq.base.util.TaggedTypes.JsonStr

object SopImpl {

  sealed trait ArchiveIntent {
    def resultFlag: Char
    val resultFlagS = resultFlag.toString
    def incFailureCount_? : Boolean
    def failureCountInc = if (incFailureCount_?) 1 else 0
  }

  case object Succeeded extends ArchiveIntent {
    override def resultFlag = 's'
    override def incFailureCount_? = false
  }

  case object FailAndAbort extends ArchiveIntent {
    override def resultFlag = 'f'
    override def incFailureCount_? = true
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Sql {
    import scala.slick.jdbc.{GetResult, SetParameter, PositionedParameters}
    import scala.slick.jdbc.StaticQuery.{query, queryNA}
    import shipreq.base.db.SqlHelpers._
    import shipreq.base.db.JavaTimeSqlHelpers._

    implicit val dbCodecWorkerId = DbCodec.WithOption.caseClass[WorkerId].writeOnly
    implicit val dbCodecNodeId   = DbCodec.WithOption.caseClass[NodeId]
    implicit val dbCodecMsg      = DbCodec.WithOption.json[Msg]
    implicit val dbCodecMsgId    = DbCodec.caseClass[MsgId]
    implicit val dbCodecPriority = DbCodec.caseClass[Priority]

    implicit object SP_ArchiveIntent extends SetParameter[ArchiveIntent] {
      def apply(v: ArchiveIntent, pp: PositionedParameters): Unit = {
        pp setString v.resultFlagS
        pp setInt v.failureCountInc
      }
    }

    implicit val GR_MsgHeader: GetResult[MsgHeader] =
      GetResult(r => MsgHeader(r.<<, r.<<, r.<<))

    // -----------------------------------------------------------------------------------------------------------------

    val getNextNodeIdQ = queryNA[NodeId]("select NEXTVAL('node_seq')")

    val cfgGetQ = query[String, String]("select v from cfg where k=?")

    private[this] def getMsgsAssignNode_q(extraSel: Option[String], extraCond: Option[String]) = s"""
         select ctid ${extraSel.fold("")(s => s",$s")}
         from msgq
         where
           effective_from <= clock_timestamp()
           and (
             node is null                           -- Unassigned
             or updated_at <= clock_timestamp()-?   -- Assignment lapsed
           )
           ${extraCond.fold("")(s => s"and($s)")}
         order by priority desc
         limit ? -- for update
    """.sql

    private[this] def nwi = "(node = ? and worker = ? and id = ?)"

    private[this] def getMsgsAssignNode_upd(ctids: String) = s"""
       update msgq
       set node = ?, worker = NULL, updated_at = clock_timestamp()
       where ctid in ($ctids)
       returning id, priority, created_at
    """.sql

    val getMsgsAssignNodeZ = query[(NodeId, Duration, Int), MsgHeader](
      getMsgsAssignNode_upd(getMsgsAssignNode_q(None, None)))

    val getMsgsAssignNodeF = query[(NodeId, Duration, Priority, Int), MsgHeader](
      getMsgsAssignNode_upd(getMsgsAssignNode_q(None, Some("priority > ?"))))

    val getMsgsAssignNodeP = query[(Duration, Int, Int, Priority, NodeId), MsgHeader](s"""
      with a as (${getMsgsAssignNode_q(Some("priority p"), None)})
      , b as (
          select ctid from a
          order by p desc
          limit greatest(?,(select count(1) from a where p>?))
      )
      ${getMsgsAssignNode_upd("select ctid from b")}
    """.sql)

    val getMsgAssignWorkerQ = query[(WorkerId, MsgId, NodeId), (Short, JsonStr[Msg], Short)]("""
      update msgq
      set worker = ?, updated_at = clock_timestamp()
      where id = ? and node = ? and worker is null
      returning type, data, failure_count
    """.sql)

    val reassignWorkerQ = query[(NodeId, WorkerId, MsgId), Boolean](s"""
      update msgq
      set updated_at = clock_timestamp()
      where $nwi
      returning true
    """.sql)

    val failAndRetryQ = query[(Duration, NodeId, WorkerId, MsgId), Boolean](s"""
      update msgq
      set
        node = null,
        worker = null,
        failure_count = failure_count + 1,
        updated_at = clock_timestamp(),
        effective_from = clock_timestamp() + ?
      where $nwi
      returning true
    """.sql)

    val archiveMsgQ = query[(NodeId, WorkerId, MsgId, ArchiveIntent), Boolean](s"""
      with tmp as (
        delete from msgq where $nwi
        returning id, type, data, ?, failure_count+?, created_at, clock_timestamp()
      )
      insert into msg_history select * from tmp
      returning true
    """.sql)
  }

  // ===================================================================================================================

  class Dao(session: Session) {
    import Sql._
    private[this] implicit def _s = session

    def getMsgsAssignNode(node: NodeId, limit: Int, assignmentTrustPeriod: Duration, queued: Option[(Priority, Int)]): List[MsgHeader] =
      queued match {
        case None =>
          // Empty mem-queue
          getMsgsAssignNodeZ(node, assignmentTrustPeriod, limit).list

        case Some((memPri, memSize)) =>
          val freeSlots = limit - memSize
          if (freeSlots > 0)
            // Partial mem-queue
            getMsgsAssignNodeP(assignmentTrustPeriod, limit, freeSlots, memPri, node).list
          else
            // Full mem-queue
            getMsgsAssignNodeF(node, assignmentTrustPeriod, memPri, limit).list
      }

    def getMsgAssignWorker(node: NodeId, worker: WorkerId, hdr: MsgHeader): Option[MsgDetail] =
      getMsgAssignWorkerQ(worker, hdr.id, node).firstOption map {
        case (msgType, msgData, failureCount) =>
          ErrorOr.require_!(
            Serialisation.deserialise(msgType, msgData).map(msg =>
              MsgDetail(hdr, msg, failureCount)))
      }

    def reassignWorker(n: NodeId, w: WorkerId, m: MsgId): Boolean =
      reassignWorkerQ(n, w, m).firstOption getOrElse false

    def failAndRetry(n: NodeId, w: WorkerId, m: MsgId, delay: Duration): Unit =
      failAndRetryQ(delay, n, w, m).first

    def archiveMsg(n: NodeId, w: WorkerId, m: MsgId, status: ArchiveIntent): Unit =
      archiveMsgQ(n, w, m, status).first

    def getNextNodeId: NodeId =
      getNextNodeIdQ.first

    def cfgGet(k: String): Option[String] =
      cfgGetQ(k).firstOption
  }

  def cfgValueReader(db: Database) =
    new StringBasedValueReader(
      new Retriever[String](k =>
        ErrorOr.safe(db.withSession((s: Session) => new Dao(s).cfgGet(k)))
          .sequence))
}

// =====================================================================================================================

class SopImpl[EA](db: Database, fh: Worker.FailureHandler) extends SopReifier {
  import Sop._
  import SopImpl._

  private[this] def ioD[A](f: Dao => A): IO[A] = IO(db.withSession(s => f(new Dao(s))))

  def getNextNodeId = ioD(_.getNextNodeId)

  override def apply[A](op: Sop[A]): IO[A] = op match {

    case GetMsgsAssignNode(node, limit, trustPeriod, queued) =>
      ioD(_.getMsgsAssignNode(node, limit, trustPeriod, queued))

    case GetMsgAssignWorker(node, worker, hdr) =>
      ioD(_.getMsgAssignWorker(node, worker, hdr))

    case ReAssignWorker(n, w, m) =>
      ioD(_.reassignWorker(n, w, m))

    case UpdateMsgSuccess(n, w, m) =>
      ioD(_.archiveMsg(n, w, m, Succeeded))

    case UpdateMsgRetry(n, w, m, delay) =>
      ioD(_.failAndRetry(n, w, m, delay))

    case UpdateMsgAbort(n, w, m) =>
      ioD(_.archiveMsg(n, w, m, FailAndAbort))

    case CfgGet(k) =>
      ioD(_ cfgGet k)

    case op: NotifySupportWorkerFailed =>
      fh handleFailedWorker op

    case op: NotifySupportTaskmanError =>
      fh handleFailedTaskman op

    case Nop =>
      IoUtils.nop
  }
}
