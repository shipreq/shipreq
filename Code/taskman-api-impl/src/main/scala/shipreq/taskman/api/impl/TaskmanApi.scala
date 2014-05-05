package shipreq.taskman.api.impl

import scala.slick.jdbc.JdbcBackend.Database
import shipreq.taskman.api.{Msg, MsgId, ApiOpReifier, ApiOp}
import scalaz.effect.IO

object TaskmanApi {
  case class Context(schema: Option[String]) {
    private[impl] val sql = new ApiSql(schema.map(_ + ".") getOrElse "")
  }
}

class TaskmanApi(ctx: TaskmanApi.Context, db: Database) extends ApiOpReifier {
  import ApiOp._

  private[this] def io [A](f: ApiDao => A): IO[A] = IO(db.withSession(s => f(new ApiDao(ctx, s))))
  private[this] def ioT[A](f: ApiDao => A): IO[A] = IO(db.withTransaction(s => f(new ApiDao(ctx, s))))

  override def apply[A](op: ApiOp[A]): IO[A] = op match {
    case SubmitMsg(m)       => io(_ createMsg m)
    case CfgPut(k, v)       => io(_.cfgPut(k, v))
    case QueryMsgStatus(id) => io(_ queryMsgStatus id)

    case SubmitMsgs(ms)     => ioT(dao =>
      (List.empty[(Msg, MsgId)] /: ms)((r, m) =>
        (m, dao createMsg m) :: r
      ))
  }
}
