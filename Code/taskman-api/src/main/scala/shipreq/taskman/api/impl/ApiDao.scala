package shipreq.taskman.api.impl

import doobie.imports._
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api._
import Serialisation.Ser

private[api] class ApiDao(prefix: String) {

  private implicit val doobieMetaMsg   = jsonStr[Msg]
  private implicit val doobieMetaMsgId = doobieMetaCaseClass[MsgId]

  private implicit val doobieMetaMsgStatus: Meta[MsgStatus] =
    Meta[String].readOnly {
      case "unassigned"    => MsgStatus.Unassigned
      case "node_assigned" => MsgStatus.NodeAssigned
      case "working"       => MsgStatus.Working
      case "complete"      => MsgStatus.Complete
      case "aborted"       => MsgStatus.Aborted
    }

  private[impl] val CreateMsg = Query[(Short, Option[Ser], Short), MsgId](
    s"select ${prefix}create_msg_v01(?::int2, ?::json, ?::int2)")

  def createMsg(m: Msg): ConnectionIO[MsgId] =
    createMsg(MsgType lookup m, Serialisation serialise m, Priority of m)

  def createMsg(m: MsgType, taskData: Ser, p: Priority): ConnectionIO[MsgId] =
    CreateMsg.toQuery0(m.id.toShort, Some(taskData), p.value).unique

  private[impl] val CfgPut = Query[(String, String), Unit](
    s"select ${prefix}cfg_update(?::VARCHAR, ?::TEXT)")

  def cfgPut(k: String, v: String): ConnectionIO[Unit] =
    CfgPut.toQuery0((k, v)).unique

  private[impl] val QueryMsgStatus = Query[MsgId, Option[MsgStatus]](
    s"select ${prefix}query_msg_status_v01(?)")

  def queryMsgStatus(id: MsgId): ConnectionIO[Option[MsgStatus]] =
    QueryMsgStatus.toQuery0(id).unique
}
