package shipreq.webapp.server.logic

import java.time.{Duration, Instant}
import upickle.Js
import scalaz.{Monad, \/, \/-}
import scalaz.syntax.monad._
import shipreq.base.util.ErrorMsg
import shipreq.taskman.api.{Msg, MsgId, TaskmanApi}
import shipreq.webapp.base.user.{User, UserValidators}

trait OpsLogic[F[_]] {
  import OpsLogic._

  def dbStats: F[DbStats]

  def userStats: F[UserStats]

  def taskmanMsgStatus(id: MsgId): F[Option[MsgStatusResult]]

  def sendMail(emailAddr: String): F[ErrorMsg \/ SendMailResult]

  def trackLogin(sessionId: SessionId, user: User): F[Unit]

  def trackLogout(sessionId: SessionId): F[Unit]
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object OpsLogic {
  import Implicits._

  abstract class Base[F[_]](implicit F: Monad[F],
                            db: DB.ForOps[F],
                            svr: Server.Time[F],
                            taskman: TaskmanApi[F]) extends OpsLogic[F] {

    import WebappTaskmanConverters._

    protected def randomToken: F[String]

    protected def measureDuration[A](f: F[A]): F[(A, Duration)] =
      for {
        t1 <- svr.now
        a  <- f
        t2 <- svr.now
      } yield (a, Duration.between(t1, t2))

    override def dbStats =
      for {
        first   <- measureDuration(db.now)
        tables  <- db.tableStats
        dbSize  <- db.dbSize
        last    <- measureDuration(db.now)
      } yield DbStats(
        now        = first._1,
        latency1   = first._2,
        latency2   = last._2,
        tableStats = tables,
        dbSize     = dbSize)

    override def userStats =
      db.userStats.map(UserStats)

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

  private def jsInstant(i: Instant): Js.Value =
    Js.Str(i.toStringIso8601)

  trait HasJsValue {
    def toJsValue: Js.Value
  }

  final case class MsgStatusResult(id: MsgId, status: String, archived: Boolean) extends HasJsValue {
    def toJsValue: Js.Value =
      Js.Obj(
        "id" -> Js.Num(id.value),
        "status" -> Js.Str(status),
        "archived" -> jsBool(archived))
  }

  final case class SendMailResult(id: MsgId, time: Duration, token: String) extends HasJsValue {
    def toJsValue: Js.Value =
      Js.Obj(
        "id" -> Js.Num(id.value),
        "token" -> Js.Str(token),
        "time" -> jsDuration(time))
  }

  final case class UserStats(stats: DB.ForOps.UserStats) extends HasJsValue {
    def toJsValue: Js.Value =
      Js.Obj(
        "registered" -> Js.Num(stats.registered),
        "pending"    -> Js.Num(stats.pendingRegistration),
        "total"      -> Js.Num(stats.total))
  }

  final case class DbStats(now       : Instant,
                           latency1  : Duration,
                           latency2  : Duration,
                           tableStats: List[DB.ForOps.TableStat],
                           dbSize    : Long) extends HasJsValue {
    import DB.ForOps.TableStat
    def toJsValue: Js.Value = {
      val tables = {
        val fields = List.newBuilder[(String, Js.Value)]
        def add(t: TableStat): Unit =
          fields +=
            t.name -> Js.Obj(
              "size" -> Js.Obj(
                "table"   -> Js.Num(t.tableSize),
                "indexes" -> Js.Num(t.indexesSize),
                "total"   -> Js.Num(t.totalSize)))
        var tableSize = 0L
        var indexesSize = 0L
        for (t <- tableStats) {
          tableSize += t.tableSize
          indexesSize += t.indexesSize
          add(t)
        }
        add(TableStat("TOTAL", tableSize = tableSize, indexesSize = indexesSize))
        Js.Obj(fields.result(): _*)
      }

      Js.Obj(
        "now"     -> jsInstant(now),
        "latency" -> Js.Arr(jsDuration(latency1), jsDuration(latency2)),
        "tables"  -> tables,
        "dbSize"  -> Js.Num(dbSize))
    }
  }
}