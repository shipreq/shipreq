package shipreq.webapp.server.logic.impl

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scalaz.Monad
import scalaz.syntax.monad._
import shipreq.base.util.ErrorMsg
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.{Task, TaskId, TaskmanApi}
import shipreq.webapp.base.data._
import shipreq.webapp.base.validation.UserValidators
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.{ApplyEvent, VerifiedEvent}
import shipreq.webapp.server.logic.algebra.{Crypto, DB, Server}
import shipreq.webapp.server.logic.data.ProjectEncryptionKey
import shipreq.webapp.server.logic.dispatch.{ResponseCmd, StatusCode}

trait OpsEndpointLogic[F[_]] {
  import OpsEndpointLogic._

  def dbStats: F[DbStats]

  def userStats: F[UserStats]

  def taskmanMsgStatus(id: TaskId): F[Option[MsgStatusResult]]

  def sendMail(emailAddr: String): F[ErrorMsg \/ SendMailResult]

  def getProjectEvents(pid: ProjectId): F[ResponseCmd]

  def createProject(user: Username \/ EmailAddr, eventsJson: String): F[ResponseCmd]
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object OpsEndpointLogic extends HasLogger {
  import shipreq.webapp.server.logic.util.LogicHelpers._

  abstract class Base[F[_]](implicit F: Monad[F],
                            crypto: Crypto[F],
                            db: DB.ForOps[F],
                            svr: Server.Time[F],
                            taskman: TaskmanApi[F]) extends OpsEndpointLogic[F] {

    import shipreq.webapp.member.project.protocol.json.v1.Latest.{decoderVerifiedEvent, encoderVerifiedEvent}
    import shipreq.webapp.server.logic.util.WebappTaskmanConverters._

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

    override def taskmanMsgStatus(id: TaskId) =
      taskman.getStatus(id).map(_.map(status =>
        MsgStatusResult(id, status.toString, status.isArchived)))

    override def sendMail(emailAddrStr: String) =
      UserValidators.emailAddr.named(emailAddrStr).onValid(emailAddr =>
        for {
          token     <- randomToken
          now       <- svr.now
          subj      = "ShipReq send-mail test"
          body      = s"Token: $token\nIssued: ${now.toStringIso8601}"
          msg       = Task.SendDiagEmail(emailAddr.toTaskman, subj, body)
          r         <- measureDuration(taskman.submit(msg))
        } yield \/-(SendMailResult(r._1, r._2, token))
      )

    override def getProjectEvents(pid: ProjectId): F[ResponseCmd] =
      db.getAllProjectEvents(pid).map {
        case \/-(ves) =>
          if (ves.isEmpty)
            ResponseCmd.StatusOnly.NotFound
          else {
            val json = ves.iterator.map(_.asJson.noSpacesSortKeys).mkString("[", "\n,", "\n]")
            ResponseCmd.Json(StatusCode.OK, json)
          }
        case -\/(e: DB.ReadProjectEventError.DecodeFailure) =>
          logger.warn(s"Failed to decode project ${pid.value} event ${e.ord.value}: ${e.logMsg}")
          val json = Json.obj(
            "error"  -> "Failed to decode event".asJson,
            "ord"    -> e.ord.value.asJson,
            "reason" -> e.logMsg.asJson)
          ResponseCmd.Json(StatusCode.NotImplemented, json)
      }

    override def createProject(user: Username \/ EmailAddr, eventsJson: String): F[ResponseCmd] =
      decodeEvents(eventsJson) match {
        case \/-(ves) =>
          db.getUserId(user).flatMap {
            case Some(uid) =>
              ApplyEvent.untrusted.applyVerified(ves)(Project.empty) match {
                case \/-(p) =>
                  for {
                    key <- crypto.generateKey256
                    pid <- db.createProject(uid, ves, p, ProjectEncryptionKey(key))
                  } yield {
                    val response = CreateProjectResult(uid, pid)
                    ResponseCmd.Json(StatusCode.OK, response.toJson)
                  }
                case -\/(err) =>
                  F pure ResponseCmd.Text(StatusCode.Forbidden, err.value)
              }
            case None =>
              F pure ResponseCmd.Text(StatusCode.BadRequest, "User not found")
          }
        case -\/(res) =>
          F pure res
      }

    private def decodeEvents(jsonStr: String): ResponseCmd \/ VerifiedEvent.Seq =
      decode[VerifiedEvent.Seq](jsonStr) match {
        case Right(ves) => \/-(ves)
        case Left(e) => -\/(ResponseCmd.Text(StatusCode.BadRequest, "Failed to parse event JSON: " + e))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private implicit val encoderDuration: Encoder[Duration] =
    Encoder.encodeString.contramap(_.conciseDesc)

  private implicit val encoderInstant: Encoder[Instant] =
    Encoder.encodeString.contramap(_.toStringIso8601)

  trait HasJson {
    def toJson: Json
  }

  final case class MsgStatusResult(id: TaskId, status: String, archived: Boolean) extends HasJson {
    override def toJson: Json =
      Json.obj(
        "id"       -> id.value.asJson,
        "status"   -> status.asJson,
        "archived" -> archived.asJson)
  }

  final case class SendMailResult(id: TaskId, time: Duration, token: String) extends HasJson {
    override def toJson: Json =
      Json.obj(
        "id"    -> id.value.asJson,
        "token" -> token.asJson,
        "time"  -> time.asJson)
  }

  final case class UserStats(stats: DB.ForOps.UserStats) extends HasJson {
    override def toJson: Json =
      Json.obj(
        "registered" -> stats.registered.asJson,
        "pending"    -> stats.pendingRegistration.asJson,
        "total"      -> stats.total.asJson)
  }

  final case class DbStats(now       : Instant,
                           latency1  : Duration,
                           latency2  : Duration,
                           tableStats: List[DB.ForOps.TableStat],
                           dbSize    : Long) extends HasJson {
    import shipreq.webapp.server.logic.algebra.DB.ForOps.TableStat
    override def toJson: Json = {
      val tables = {
        val fields = List.newBuilder[(String, Json)]
        def add(t: TableStat): Unit =
          fields +=
            t.name -> Json.obj(
              "size" -> Json.obj(
                "table"   -> t.tableSize.asJson,
                "indexes" -> t.indexesSize.asJson,
                "total"   -> t.totalSize.asJson))
        var tableSize = 0L
        var indexesSize = 0L
        for (t <- tableStats) {
          tableSize += t.tableSize
          indexesSize += t.indexesSize
          add(t)
        }
        add(TableStat("TOTAL", tableSize = tableSize, indexesSize = indexesSize))
        Json.obj(fields.result(): _*)
      }

      Json.obj(
        "now"     -> now.asJson,
        "latency" -> Json.arr(latency1.asJson, latency2.asJson),
        "tables"  -> tables,
        "dbSize"  -> dbSize.asJson)
    }
  }

  final case class CreateProjectResult(userId: UserId, projectId: ProjectId) extends HasJson {
    override def toJson: Json =
      Json.obj(
        "userId"    -> userId.value.asJson,
        "projectId" -> projectId.value.asJson)
  }
}