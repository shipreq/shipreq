package shipreq.webapp.server.db.migration

import com.typesafe.scalalogging.StrictLogging
import doobie._
import doobie.implicits._
import java.time.Instant
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.db.DoobieHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.global.GlobalEvent._
import shipreq.webapp.server.db.GlobalEventSerialisation
import shipreq.webapp.server.db.GlobalEventSerialisation.Row
import shipreq.webapp.server.db.WebappDoobieCodecs._

class V5_1__RetroGlobalEvents extends DbMigration with StrictLogging {
  import V5_1__RetroGlobalEvents._

  override protected def migration: ConnectionIO[_] =
    for {
      e1 <- entriesForUserRegister1
      e2 <- entriesForUserRegister2
      e3 <- entriesForUserPasswordResetRequest
      e4 <- entriesForUserPasswordReset
      es  = (e1 ++ e2 ++ e3 ++ e4).sorted
      _  <- logPlan(es)
      _  <- saveAll(es)
    } yield ()

  private def entriesForUserRegister1: ConnectionIO[Vector[Entry]] =
    sql"select id, confirmation_sent_at from usr"
      .query[(UserId, Instant)]
      .map { case (id, t) => Entry(UserRegister1(None, id), t) }
      .to[Vector]

  private def entriesForUserRegister2: ConnectionIO[Vector[Entry]] =
    sql"select id, confirmed_at from usr where confirmed_at is not null"
      .query[(UserId, Instant)]
      .map { case (id, t) => Entry(UserRegister2(None, id), t) }
      .to[Vector]

  private def entriesForUserPasswordResetRequest: ConnectionIO[Vector[Entry]] =
    sql"select id, email, reset_password_sent_at from usr where reset_password_sent_at is not null"
      .query[(UserId, String, Instant)]
      .map { case (id, e, t) => Entry(UserPasswordResetRequest(None, e, Some(id)), t) }
      .to[Vector]

  private def entriesForUserPasswordReset: ConnectionIO[Vector[Entry]] =
    sql"select id, password_changed_at from usr where reset_password_sent_at is not null and password_changed_at is not null"
      .query[(UserId, Instant)]
      .map { case (id, t) => Entry(UserPasswordReset(None, id), t) }
      .to[Vector]

  private def logPlan(entries: Vector[Entry]): ConnectionIO[Unit] =
    point {
      val len = entries.length
      logger.info(s"Creating $len global events")
      for (i <- entries.indices) {
        val e = entries(i)
        logger.info(s"  - [${i + 1}/$len] $e")
      }
    }

  private def saveAll(entries: Vector[Entry]): ConnectionIO[Unit] =
    Update[(Row, Instant)]("INSERT INTO global_event(type,data,ip,usr_id,created_at) VALUES(?,?,?,?,?)")
      .updateMany(entries.map(e => (e.row, e.createdAt)))
      .void
}

object V5_1__RetroGlobalEvents {

  final case class Entry(event: GlobalEvent, createdAt: Instant) {
    def row = GlobalEventSerialisation.encode(event)
  }

  object Entry {
    implicit def ordering: Ordering[Entry] =
      Ordering.by(_.createdAt)
  }
}