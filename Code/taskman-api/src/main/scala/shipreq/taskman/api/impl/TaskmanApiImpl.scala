package shipreq.taskman.api.impl

import doobie.imports._
import scalaz.~>
import shipreq.taskman.api._

object TaskmanApiImpl {
  case class Context(schema: Option[String]) {
    private[impl] val dao = new ApiDao(schema.map(_ + ".") getOrElse "")
  }

  def apply(ctx: Context): TaskmanApi[ConnectionIO] =
    new TaskmanApi[ConnectionIO] {
      override def cfgPut(k: String, v: String) = ctx.dao.cfgPut(k, v)
      override def submitMsg(m: Msg)            = ctx.dao.createMsg(m)
      override def queryMsgStatus(id: MsgId)    = ctx.dao.queryMsgStatus(id)
    }

  def apply[F[_]](ctx: Context, t: ConnectionIO ~> F): TaskmanApi[F] =
    TaskmanApi.trans(apply(ctx))(t)
}
