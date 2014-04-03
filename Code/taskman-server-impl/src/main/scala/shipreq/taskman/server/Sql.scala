package shipreq.taskman.server

import java.sql.Timestamp
import org.joda.time.{Period, DateTime}
import org.postgresql.util.PGInterval
import scala.slick.jdbc.StaticQuery.{query, queryNA, update}
import scala.slick.jdbc.{GetResult, SetParameter}
import scala.slick.session.PositionedParameters
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{MsgId, Msg, Priority}

object Sql {

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

  // ===================================================================================================================

  implicit val GR_JsonMsg = GR_Json[Msg]
  implicit val SP_JsonMsg = SP_Json[Msg]

  // TODO joda time + slick should be shared in base-db
  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)
  implicit val GR_DateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))

  implicit object SP_ArchiveIntent extends SetParameter[ArchiveIntent] {
    def apply(v: ArchiveIntent, pp: PositionedParameters): Unit = {
      pp setString v.resultFlagS
      pp setInt v.failureCountInc
    }
  }

  implicit val GR_MsgId = GetResult(r => MsgId(r.<<))
  implicit object SP_MsgId extends SetParameter[MsgId] {
    def apply(v: MsgId, pp: PositionedParameters): Unit = pp setLong v.value
  }

  implicit val GR_NodeId = GetResult(r => NodeId(r.<<))
  implicit object SP_NodeId extends SetParameter[NodeId] {
    def apply(v: NodeId, pp: PositionedParameters): Unit = pp setInt v.value
  }
  implicit object SP_NodeIdO extends SetParameter[Option[NodeId]] {
    def apply(v: Option[NodeId], pp: PositionedParameters): Unit = pp setIntOption v.map(_.value)
  }

  implicit object SP_WorkerId extends SetParameter[WorkerId] {
    def apply(v: WorkerId, pp: PositionedParameters): Unit = pp setShort v.value
  }
  implicit object SP_WorkerIdO extends SetParameter[Option[WorkerId]] {
    def apply(v: Option[WorkerId], pp: PositionedParameters): Unit = pp setShortOption v.map(_.value)
  }

  implicit val GR_Priority = GetResult(r => Priority(r.<<))
  implicit object SP_Priority extends SetParameter[Priority] {
    def apply(v: Priority, pp: PositionedParameters): Unit = pp setShort v.value
  }

  implicit object SP_Period extends SetParameter[Period] {
    def apply(v: Period, pp: PositionedParameters): Unit = {
      val i = new PGInterval(
        v.getYears, v.getMonths, v.getDays, v.getHours, v.getMinutes,
        v.getSeconds.toDouble + v.getMillis/1000.0
      )
      pp.setObject(i, java.sql.Types.OTHER)
    }
  }

  implicit val GR_MsgHeader = GetResult(r => MsgHeader(r.<<, r.<<, r.<<))

  // ===================================================================================================================

  val getNextNodeIdQ = queryNA[NodeId]("select NEXTVAL('node_seq')")

  val cfgGetQ = query[String, String]("select v from cfg where k=?")

  private[this] def getMsgsAssignNode_q(extraSel: Option[String], extraCond: Option[String]) = s"""
         select ctid ${extraSel.map(s => s",$s") getOrElse ""}
         from msgq
         where
           effective_from <= clock_timestamp()
           and (
             node is null                           -- Unassigned
             or updated_at <= clock_timestamp()-?   -- Assignment lapsed
           )
           ${extraCond.map(s => s"and($s)") getOrElse ""}
         order by priority desc
         limit ? -- for update
    """.sql

  private[this] def getMsgsAssignNode_upd(ctids: String) = s"""
       update msgq
       set node = ?, worker = NULL, updated_at = clock_timestamp()
       where ctid in ($ctids)
       returning id, priority, created_at
    """.sql

  val getMsgsAssignNodeZ = query[(NodeId, Period, Int), MsgHeader](
    getMsgsAssignNode_upd(getMsgsAssignNode_q(None, None)))

  val getMsgsAssignNodeF = query[(NodeId, Period, Priority, Int), MsgHeader](
    getMsgsAssignNode_upd(getMsgsAssignNode_q(None, Some("priority > ?"))))

  val getMsgsAssignNodeP = query[(Period, Int, Int, Priority, NodeId), MsgHeader](s"""
      with a as (${getMsgsAssignNode_q(Some("priority p"), None)})
      , b as (
          select ctid from a
          order by p desc
          limit greatest(?,(select count(1) from a where p>?))
      )
      ${getMsgsAssignNode_upd("select ctid from b")}
    """.sql)

  val getMsgAssignWorkerQ = query[(WorkerId, MsgId, NodeId), (Short, Json[Msg], Short)]("""
      update msgq
      set worker = ?, updated_at = clock_timestamp()
      where id = ? and node = ? and worker is null
      returning type, data, failure_count
    """.sql)

  // TODO Doesn't confirm worker. and node = ? and worker = ?  , NodeId, WorkerId
  val failAndRetryQ = update[(Period, MsgId)]("""
      update msgq
      set
        node = null,
        worker = null,
        failure_count = failure_count + 1,
        updated_at = clock_timestamp(),
        effective_from = clock_timestamp() + ?
      where id = ?
    """.sql)

  // TODO Doesn't confirm worker. and node = ? and worker = ?  , NodeId, WorkerId
  val archiveMsgQ = update[(MsgId, ArchiveIntent)]("""
      with tmp as (
        delete from msgq where id=?
        returning id, type, data, ?, failure_count+?, created_at, clock_timestamp()
      )
      insert into msg_history select * from tmp
    """.sql)
}
