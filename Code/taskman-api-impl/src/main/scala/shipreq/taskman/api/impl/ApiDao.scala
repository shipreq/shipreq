package shipreq.taskman.api.impl

import scala.slick.session.Session
import shipreq.taskman.api._
import Serialisation.Ser

private[api] class ApiSql(prefix: String) {
  import shipreq.base.db.SqlHelpers._
  import scala.slick.jdbc.GetResult
  import scala.slick.jdbc.StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_JsonMsg = GR_Json[Msg]
  implicit val SP_JsonMsg = SP_Json[Msg]
  implicit val GR_MsgId: GetResult[MsgId] = implicitly[GetResult[Long]] andThen MsgId

  val CreateMsg = query[(Short, Option[Ser], Short), MsgId](
    s"select ${prefix}create_msg_v01(?::int2, ?::json, ?::int2)")

  val CfgPut = update[(String, String)](
    s"select ${prefix}cfg_update(?::VARCHAR, ?::TEXT)")
}

private[api] class ApiDao(ctx: TaskmanApiImpl.GlobalContext, session: Session) {
  import ctx.sql._

  implicit def _session = session

  def createMsg(m: Msg): MsgId =
    createMsg(MsgType lookup m, Serialisation serialise m, Priority of m)

  def createMsg(m: MsgType, taskData: Ser, p: Priority): MsgId =
    CreateMsg.first(m.id.toShort, Some(taskData), p.value)

  def cfgPut(k: String, v: String): Unit =
    CfgPut.execute(k, v)
}
