package shipreq.taskman.api.impl

import doobie._
import shipreq.taskman.api._

private[api] class ApiDao(prefix: String) {
  import TaskmanDoobieCodecs._

  private[impl] val createMsgQuery: Query[(Task, Priority), TaskId] =
    Query(s"select ${prefix}create_msg_v01(?::INT2, ?::JSONB, ?::INT2)")

  def createMsg(m: Task): ConnectionIO[TaskId] =
    createMsgQuery.toQuery0((m, Priority.of(m))).unique

  private[impl] val cfgPutQuery: Query[(String, String), Unit] =
    Query(s"select ${prefix}cfg_update(?::VARCHAR, ?::TEXT)")

  def cfgPut(k: String, v: String): ConnectionIO[Unit] =
    cfgPutQuery.toQuery0((k, v)).unique

  private[impl] val queryMsgStatusQuery: Query[TaskId, Option[TaskStatus]] =
    Query(s"select ${prefix}query_msg_status_v01(?)")

  def queryMsgStatus(id: TaskId): ConnectionIO[Option[TaskStatus]] =
    queryMsgStatusQuery.toQuery0(id).unique
}
