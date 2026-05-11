package shipreq.taskman.server

import cats.~>
import doobie._
import japgolly.clearconfig._
import java.time.Duration
import shipreq.base.db.{DbAccessor, XA}
import shipreq.base.util.FxModule._
import shipreq.taskman.api._
import shipreq.taskman.server.logic._

object ServerOpFx {

  sealed trait ArchiveIntent {
    def resultFlag: Char
    val resultFlagS = resultFlag.toString
    def incFailureCount_? : Boolean
    def failureCountInc: Short = if (incFailureCount_?) 1.toShort else 0.toShort
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
    import shipreq.base.db.DoobieHelpers._
    import shipreq.base.db.BaseDoobieCodecs._
    import shipreq.base.db.SqlHelpers._
    import shipreq.taskman.api.impl.TaskmanDoobieCodecs._

    implicit val doobieMetaWorkerId: Meta[WorkerId] =
      Meta[Short].timap(WorkerId.apply)(_.value)

    implicit val doobieMetaNodeId: Meta[NodeId] =
      Meta[Int].timap(NodeId.apply)(_.value)

    implicit val doobieReadTaskHeader: Read[TaskHeader] =
      Read.apply3(TaskHeader.apply)

    implicit val doobieWriteTaskHeader: Write[TaskHeader] =
      Write.apply3(h => (h.id, h.priority, h.created))

    implicit val doobieWriteArchiveIntent: Write[ArchiveIntent] =
      Write.apply2(x => (x.resultFlagS, x.failureCountInc))

    // -----------------------------------------------------------------------------------------------------------------

    val getNextNodeIdQ = Query0[NodeId]("select NEXTVAL('node_seq')")

    val cfgGetQ = Query[String, String]("select v from cfg where k=?")

    val cfgGetAllQ = Query0[(String, String)]("select k,v from cfg")

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

    val getMsgsAssignNodeZ = Query[(NodeId, Duration, Int), TaskHeader](
      getMsgsAssignNode_upd(getMsgsAssignNode_q(None, None)))

    val getMsgsAssignNodeF = Query[(NodeId, Duration, Priority, Int), TaskHeader](
      getMsgsAssignNode_upd(getMsgsAssignNode_q(None, Some("priority > ?"))))

    val getMsgsAssignNodeP = Query[(Duration, Int, Int, Priority, NodeId), TaskHeader](s"""
      with a as (${getMsgsAssignNode_q(Some("priority p"), None)})
      , b as (
          select ctid from a
          order by p desc
          limit greatest(?,(select count(1) from a where p>?))
      )
      ${getMsgsAssignNode_upd("select ctid from b")}
    """.sql)

    val getMsgAssignWorkerQ = Query[(WorkerId, TaskId, NodeId), (Task, Short)]("""
      update msgq
      set worker = ?, updated_at = clock_timestamp()
      where id = ? and node = ? and worker is null
      returning type, data, failure_count
    """.sql)

    val reassignWorkerQ = Query[(NodeId, WorkerId, TaskId), Boolean](s"""
      update msgq
      set updated_at = clock_timestamp()
      where $nwi
      returning true
    """.sql)

    val failAndRetryQ = Query[(Duration, NodeId, WorkerId, TaskId), Boolean](s"""
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

    val archiveMsgQ = Query[(NodeId, WorkerId, TaskId, ArchiveIntent), Boolean](s"""
      with tmp as (
        delete from msgq where $nwi
        returning id, type, data, ?, failure_count+?, created_at, clock_timestamp()
      )
      insert into msg_history select * from tmp
      returning true
    """.sql)
  }

  // ===================================================================================================================

  object Dao {
    import Sql._

    def getMsgsAssignNode(node                 : NodeId,
                          limit                : Int,
                          assignmentTrustPeriod: Duration,
                          queued               : Option[(Priority, Int)]): ConnectionIO[List[TaskHeader]] =
      queued match {
        case None =>
          // Empty mem-queue
          getMsgsAssignNodeZ.toQuery0((node, assignmentTrustPeriod, limit)).to[List]

        case Some((memPri, memSize)) =>
          val freeSlots = limit - memSize
          if (freeSlots > 0)
            // Partial mem-queue
            getMsgsAssignNodeP.toQuery0((assignmentTrustPeriod, limit, freeSlots, memPri, node)).to[List]
          else
            // Full mem-queue
            getMsgsAssignNodeF.toQuery0((node, assignmentTrustPeriod, memPri, limit)).to[List]
      }

    def getMsgAssignWorker(node: NodeId, worker: WorkerId, hdr: TaskHeader): ConnectionIO[Option[TaskDetail]] =
      getMsgAssignWorkerQ.toQuery0((worker, hdr.id, node)).option.map(_ map {
        case (msg, failureCount) => TaskDetail(hdr, msg, failureCount)
      })

    def reassignWorker(n: NodeId, w: WorkerId, m: TaskId): ConnectionIO[Boolean] =
      reassignWorkerQ.toQuery0((n, w, m)).option.map(_ getOrElse false)

    def failAndRetry(n: NodeId, w: WorkerId, m: TaskId, delay: Duration): ConnectionIO[Unit] =
      failAndRetryQ.toQuery0((delay, n, w, m)).unique.map(_ => ())

    def archiveMsg(n: NodeId, w: WorkerId, m: TaskId, status: ArchiveIntent): ConnectionIO[Unit] =
      archiveMsgQ.toQuery0((n, w, m, status)).unique.map(_ => ())

    def getNextNodeId: ConnectionIO[NodeId] =
      getNextNodeIdQ.unique

    def cfgGet(k: String): ConnectionIO[Option[String]] =
      cfgGetQ.toQuery0(k).option

    def cfgGetAll: ConnectionIO[List[(String, String)]] =
      cfgGetAllQ.to[List]
  }

  def configSource(db: DbAccessor, xa: XA): ConfigSource[Fx] =
    ConfigSource[Fx](
      ConfigSourceName(db.desc),
      xa(Dao.cfgGetAll).attemptFx.map {
        case \/-(kvs) => \/-(ConfigStore.ofMap[Fx](kvs.toMap))
        case -\/(e)   => -\/(e.toString)
      }
    )
}

// =====================================================================================================================

final class ServerOpFx(xa: XA, fh: Worker.FailureHandler) extends (ServerOp ~> Fx) {
  import ServerOp._
  import ServerOpFx._

  def getNextNodeId = xa(Dao.getNextNodeId)

  override def apply[A](op: ServerOp[A]): Fx[A] = op match {

    case GetTasksAssignNode(node, limit, trustPeriod, queued) =>
      xa(Dao.getMsgsAssignNode(node, limit, trustPeriod, queued))

    case GetTaskAssignWorker(node, worker, hdr) =>
      xa(Dao.getMsgAssignWorker(node, worker, hdr))

    case ReassignWorker(n, w, m) =>
      xa(Dao.reassignWorker(n, w, m.id))

    case UpdateTaskSuccess(n, w, m) =>
      xa(Dao.archiveMsg(n, w, m.id, Succeeded))

    case UpdateTaskRetry(n, w, m, delay) =>
      xa(Dao.failAndRetry(n, w, m.id, delay))

    case UpdateTaskAbort(n, w, m) =>
      xa(Dao.archiveMsg(n, w, m.id, FailAndAbort))

    case CfgGet(k) =>
      xa(Dao.cfgGet(k))

    case op: NotifySupportWorkerFailed =>
      fh handleFailedWorker op

    case op: NotifySupportTaskmanError =>
      fh handleFailedTaskman op

    case Nop =>
      Fx.unit
  }
}
