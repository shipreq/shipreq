package shipreq.taskman.api.impl

import doobie.imports._
import shipreq.taskman.api._

private[api] class ApiDao(prefix: String) {
  import DoobieMeta._

  private[impl] val createMsgQuery: Query[(Msg, Priority), MsgId] =
    Query(s"select ${prefix}create_msg_v01(?::INT2, ?::JSONB, ?::INT2)")

  def createMsg(m: Msg): ConnectionIO[MsgId] =
    createMsgQuery.toQuery0((m, Priority.of(m))).unique

  private[impl] val cfgPutQuery: Query[(String, String), Unit] =
    Query(s"select ${prefix}cfg_update(?::VARCHAR, ?::TEXT)")

  def cfgPut(k: String, v: String): ConnectionIO[Unit] =
    cfgPutQuery.toQuery0((k, v)).unique

  private[impl] val queryMsgStatusQuery: Query[MsgId, Option[MsgStatus]] =
    Query(s"select ${prefix}query_msg_status_v01(?)")

  def queryMsgStatus(id: MsgId): ConnectionIO[Option[MsgStatus]] =
    queryMsgStatusQuery.toQuery0(id).unique
}
