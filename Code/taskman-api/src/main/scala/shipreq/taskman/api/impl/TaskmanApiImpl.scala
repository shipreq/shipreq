package shipreq.taskman.api.impl

import doobie._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api._

object TaskmanApiImpl extends HasLogger {

  def apply(schema: Option[String]): TaskmanApi[ConnectionIO] =
    new TaskmanApi[ConnectionIO] {
      private[this] val dao = new ApiDao(schema.map(_ + ".") getOrElse "")
      override def cfgPut(k: String, v: String) = dao.cfgPut(k, v)
      override def submit(m: Task)              = dao.createMsg(m)
      override def getStatus(id: TaskId)        = dao.queryMsgStatus(id)
    }
}
