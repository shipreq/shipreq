package shipreq.taskman.api.impl

import scala.slick.session.Database
import shipreq.taskman.api.{ApiOpReifier, ApiOp}
import scalaz.effect.IO

object TaskmanApi {
  case class Context(schema: Option[String]) {
    private[impl] val sql = new ApiSql(schema.map(_ + ".") getOrElse "")
  }
}

class TaskmanApi(ctx: TaskmanApi.Context, db: Database) extends ApiOpReifier {
  import ApiOp._

  private[this] def io[A](f: ApiDao => A): IO[A] =
    IO(db.withSession(s => f(new ApiDao(ctx, s))))
  
  override def apply[A](op: ApiOp[A]): IO[A] = op match {
    case SubmitMsg(t)   => io(_ createMsg t)
    case SubmitMsgs(ts) => io(dao => ts.foreach(t => dao createMsg t))
    case CfgPut(k, v)   => io(_.cfgPut(k, v))
  }
}
