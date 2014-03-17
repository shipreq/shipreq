package shipreq.taskman.api.impl

import scala.slick.session.Session
import shipreq.taskman.api._
import Serialisation.Ser

private[api] class ApiSql(prefix: String) {
  import shipreq.base.db.SqlHelpers._
  import scala.slick.jdbc.StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_JsonMsg = GR_Json[Msg]
  implicit val SP_JsonMsg = SP_Json[Msg]

  val CreateMsg = update[(Short, Option[Ser], Short)](
    s"select ${prefix}create_msg_v01(?::int2, ?::json, ?::int2)")
}

private[api] class ApiDao(ctx: TaskmanApiImpl.GlobalContext, session: Session) {
  import ctx.sql._

  implicit def _session = session

  def createMsg(m: Msg): Unit =
    createMsg(MsgType lookup m, Serialisation serialise m, Priority of m)

  def createMsg(m: MsgType, taskData: Ser, p: Priority): Unit =
    CreateMsg.execute(m.id.toShort, Some(taskData), p.value)
}
