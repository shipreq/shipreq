package shipreq.taskman.api.impl

import scala.slick.session.Session
import shipreq.taskman.api._
import Serialisation.Ser

private[api] class ApiSql(prefix: String) {
  import shipreq.base.db.SqlHelpers._
  import scala.slick.jdbc.StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_JsonTaskDef = GR_Json[TaskDef]
  implicit val SP_JsonTaskDef = SP_Json[TaskDef]

  val CreateTask = update[(Short, Option[Ser], Short)](
    s"select ${prefix}create_task_v01(?::int2, ?::json, ?::int2)")
}

private[api] class ApiDao(ctx: TaskmanApiImpl.GlobalContext, session: Session) {
  import ctx.sql._

  implicit def _session = session

  def createTask(t: TaskDef): Unit =
    createTask(TaskTypes lookupType t, Serialisation serialise t, Priority forTask t)

  def createTask(t: TaskType, taskData: Ser, p: Priority): Unit =
    CreateTask.execute(t.id.toShort, Some(taskData), p.value)
}
