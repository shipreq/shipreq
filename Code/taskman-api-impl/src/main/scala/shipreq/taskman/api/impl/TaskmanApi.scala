package shipreq.taskman.api.impl

import doobie.imports._
import shipreq.taskman.api.{ApiOpReifier, ApiOp}
import scalaz.effect.IO
import scalaz.std.list.listInstance
import scalaz.syntax.traverse._

object TaskmanApi {
  case class Context(schema: Option[String]) {
    private[impl] val dao = new ApiDao(schema.map(_ + ".") getOrElse "")
  }
}

class TaskmanApi(ctx: TaskmanApi.Context, db: Transactor[IO]) extends ApiOpReifier {
  import ApiOp._

  private[this] def dbio[A](q: ConnectionIO[A]): IO[A] =
    db.trans(q)

  override def apply[A](op: ApiOp[A]): IO[A] = op match {
    case SubmitMsg(m)       => dbio(ctx.dao.createMsg(m))
    case CfgPut(k, v)       => dbio(ctx.dao.cfgPut(k, v))
    case QueryMsgStatus(id) => dbio(ctx.dao.queryMsgStatus(id))
    case SubmitMsgs(ms)     => dbio(ms.traverseU(m => ctx.dao.createMsg(m).map((m, _))))
  }
}
