package shipreq.webapp.server.test

import doobie._
import doobie.implicits._
import scala.util.Random
import shipreq.base.db.DoobieHelpers._
import shipreq.base.test.db._
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.{ProjectId, UserId, Username}
import shipreq.webapp.member.data.Project
import shipreq.webapp.member.event.ActiveEvent
import shipreq.webapp.server.db.DbInterpreter
import shipreq.webapp.server.logic.effect.DB
import shipreq.webapp.server.security.SecurityInterpreter
import shipreq.webapp.server.test.WebappServerTestUtil._

object DbUtil {

  private[DbUtil] val Random = new Random()
}

final case class DbUtil(xa: ImperativeXA) {

  lazy val security = {
    val g = PrepareEnv.global()
    implicit val t = g.config.server.traceAlgebraFx
    implicit val c = g.config.server.security
    implicit val d = DB.ForSecurity.trans(DbInterpreter.ForSecurity)(xa.transZ)
    new SecurityInterpreter[Fx]()
  }

  val dbAlgebra = {
    val g = PrepareEnv.global()
    implicit val c = g.config.server.security
    new DbInterpreter()
  }

  private def randomStr: String =
    DbUtil.Random.nextString(32)

  def newProjectId(userId    : UserId              = getOrCreateUserId(),
                   initEvents: Vector[ActiveEvent] = Vector.empty,
                  ): ProjectId = {
    val p = applyEventsSuccessfully(Project.empty, initEvents: _*)
    xa ! dbAlgebra.createProject(userId, initEvents, p)
  }

  def getOrCreateUserId(): UserId =
    (xa ! Query0[UserId]("select id from usr where username is not null limit 1").option) getOrElse newUserId()

  def newUserId(): UserId =
    xa ! Query[(String, String), UserId](
      "INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,0,0,NOW(),NOW(),NOW()) RETURNING id"
    ).toQuery0((randomStr, randomStr)).unique

  def getUsername(id: UserId): Username =
    xa ! Query[UserId, Username]("SELECT username FROM usr WHERE id=?").toQuery0(id).unique

  def deleteUser(id: Long): Unit = {
    xa ! sql"DELETE FROM usrh_name WHERE usr_id = $id".update.run
    xa ! sql"DELETE FROM usrd WHERE usr_id = $id".update.run
    xa ! sql"DELETE FROM usr WHERE id = $id".update.execute
  }

  def deleteUserByEmail(email: String): Unit = {
    val id = xa ! sql"SELECT id FROM usr WHERE email = $email".query[Long].option
    id.foreach(deleteUser)
  }

  def lookupConfirmationToken(email: String): Option[String] =
    xa ! sql"select confirmation_token from usr where email=$email".query[String].option
}
