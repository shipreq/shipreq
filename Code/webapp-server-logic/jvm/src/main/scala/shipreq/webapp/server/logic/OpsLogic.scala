package shipreq.webapp.server.logic

import java.time.Duration
import upickle.Js
import scalaz.{Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util.ErrorMsg
import shipreq.taskman.api.{Msg, MsgId, TaskmanApi}
import shipreq.webapp.base.user.{EmailAddr, UserValidators}

trait OpsLogic[F[_]] {
  import OpsLogic._

  def taskmanMsgStatus(id: MsgId): F[Option[MsgStatusResult]]

  def sendMail(emailAddr: String): F[ErrorMsg \/ SendMailResult]

}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object OpsLogic {

  def apply[F[_]](randomToken: F[String])
                 (implicit F: Monad[F],
                  svr: Server.Time[F],
                  taskman: TaskmanApi[F]): OpsLogic[F] =
    new OpsLogic[F] {
      import Implicits._
      import WebappTaskmanConverters._

      private def measureDuration[A](f: F[A]): F[(A, Duration)] =
        for {
          t1 <- svr.now
          a  <- f
          t2 <- svr.now
        } yield (a, Duration.between(t1, t2))

      override def taskmanMsgStatus(id: MsgId) =
        taskman.queryMsgStatus(id).map(_.map(status =>
          MsgStatusResult(id, status.toString, status.isArchived)))

      override def sendMail(emailAddrStr: String) =
        UserValidators.emailAddr.named(emailAddrStr).onValid(emailAddr =>
          for {
            token     ← randomToken
            now       ← svr.now
            subj      = "ShipReq send-mail test"
            body      = s"Token: $token\nIssued: ${now.toStringIso8601}"
            msg       = Msg.SendDiagEmail(emailAddr.toTaskman, subj, body)
            r         ← measureDuration(taskman.submitMsg(msg))
          } yield \/-(SendMailResult(r._1, r._2, token))
        )

    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def jsBool(b: Boolean): Js.Value =
    if (b) Js.True else  Js.False

  private def jsDuration(d: Duration): Js.Value =
    Js.Num(d.toMillis.toDouble / 1000 + d.getNano.toDouble / 1000000000)

  final case class MsgStatusResult(id: MsgId, status: String, archived: Boolean) {
    def toJsValue: Js.Value =
      Js.Obj(
        "id" -> Js.Num(id.value),
        "status" -> Js.Str(status),
        "archived" -> jsBool(archived))
  }

  final case class SendMailResult(id: MsgId, time: Duration, token: String) {
    def toJsValue: Js.Value =
      Js.Obj(
        "id" -> Js.Num(id.value),
        "token" -> Js.Str(token),
        "time" -> jsDuration(time))
  }
}