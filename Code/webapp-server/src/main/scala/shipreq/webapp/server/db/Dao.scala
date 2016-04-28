package shipreq.webapp.server.db

import net.liftweb.util.Helpers.nextFuncName
import org.postgresql.util.PSQLException
import scala.slick.jdbc.JdbcBackend.Session
import shipreq.taskman.api.{UserId, EmailAddr}
import shipreq.webapp.server.data._
import shipreq.webapp.server.lib.Misc.retry
import shipreq.webapp.server.security.PasswordAndSalt

/**
 * Database interface.
 *
 * Methods follow this pattern:
 *
 * - `find`: Searches for a single row. Returns Option[T].
 * - `findAll`: Searches for a multiple rows. Returns List[T], possibly empty.
 * - `findOrCreate`: Searches for an item and creates it if not found. Returns T.
 * - `summarise`: Retrieves a list of objects that summarise one or more data. Returns List[T].
 *
 * - `create`: Creates a new row. Returns T.
 * - `link`: Creates a link/mapping between existing DB records. Returns unit.
 * - `log`: Records a loggable event. Returns unit.
 * - `sync`: Ensures the DB state of a record matches a provided model, changing the DB if necessary.
 * - `update`: Modifies an existing DB record. Return specialised result.
 * - `delete`: Delete data and usually its dependents. Returns unit.
 *
 * - `perform`: Perform specialised business-logic (as opposed to CRUD-like operations).
 * - `diag`: Diagnostic functions.
 */
private[db] class Dao(_session: Session) extends DaoT {
  implicit final val session = _session
}

/**
 * Database interfacing methods that do not require a transaction.
 */
sealed trait DaoS {
  import Sql._

  implicit val session: Session

  /** "Safe" in the sense that an error rolls back the inner transaction without aborting the outer one. */
  private[this] def inSafeTransaction[T](f: => T): T = {
    val conn = session.conn
    val oldAutoCommit = conn.getAutoCommit
    if (oldAutoCommit)
      conn.setAutoCommit(false)
    val sp = conn.setSavepoint()

    try {
      val result = f
      if (oldAutoCommit)
        conn.commit
      result
    } catch {
      case e: Throwable =>
        conn.rollback(sp)
        throw e
    } finally {
      if (oldAutoCommit)
        conn.setAutoCommit(true)
    }
  }

  // TODO: Use EmailAddr instead of String

  // ===================================================================================================================
  // User

  private def tokenAttempt(tokenFn: () => String)(f: String => Unit): String =
    retry(10) {
      inSafeTransaction {
        val token = tokenFn()
        f(token)
        token
      }
    }

  /** Creates an unconfirmed user account. No username, no password until email confirmed. */
  def createUserPlaceholder(email: EmailAddr, tokenFn: () => String): String =
    tokenAttempt(tokenFn)(token => InsertUserPlaceholder(email, token).execute)

  def updateUserConfirmationToken(id: UserId, tokenFn: () => String): String =
    tokenAttempt(tokenFn)(token => UpdateConfirmationToken(token, id).execute)

  def findUserDescAndCredentials(usernameOrEmail: String): Option[(UserDescriptor, PasswordAndSalt)] =
    if (usernameOrEmail.indexOf('@') == -1)
      findUserDescAndCredentialsU(Username(usernameOrEmail))
    else
      findUserDescAndCredentialsE(EmailAddr(usernameOrEmail))

  def findUserDescAndCredentialsU(username: Username) = GetUserDescCredByUsername(username).firstOption

  def findUserDescAndCredentialsE(email: EmailAddr) = GetUserDescCredByEmail(email).firstOption

  def findUserRegistrationInfo(email: EmailAddr) = GetUserRegInfo(email).firstOption

  def findUserRegAndResetPwInfo(email: EmailAddr) = GetUserRegAndResetPwInfo(email).firstOption

  def findUserConfirmationTokenIssuedDate(token: String) = GetConfirmationTokenIssuedDate(token).firstOption

  def findUserSuppAndDetail(id: UserId) = GetUserSuppAndDetail(id).firstOption

  def logUserLogin(id: UserId, ip: Option[String]): Unit = LogUserLogin(id, ip).execute

  def updateUserDetails(id: UserId, d: UserDetail): Unit =
    UpdateUserDetails(d.name, d.newsletter, id).execute

  def updateUserPassword(id: UserId, ps: PasswordAndSalt): Unit = UpdateUserPassword(ps, id).execute

  def performInstallNewResetPasswordToken(u: UserId, tokenFn: () => String): String =
    tokenAttempt(tokenFn)(token => InstallNewResetPasswordToken(token, u).execute)

  def performReuseResetPasswordToken(u: UserId): Unit = ReuseResetPasswordToken(u).execute

  def findResetPasswordTokenIssuedDate(token: String) = GetResetPasswordTokenIssuedDate(token).firstOption

  def performPasswordReset(ps: PasswordAndSalt, token: String) = ResetPassword(ps, token).execute

  // ===================================================================================================================
  // Project

  private[this] def saveProject[R](name: String, saveFn: String => R)(nameAlreadyInUse: R): R =
    try saveFn(name)
    catch {
      case e: PSQLException if e.getMessage.contains("_usr_id_name_") => nameAlreadyInUse
    }

  def createProject(usrId: UserId, name: String): CreateProjectResult = {
    import CreateProjectResult._
    saveProject[CreateProjectResult](name, name => {
      val id = CreateProject(usrId, name).first
      DbSuccess(id)
    })(NameAlreadyInUse)
  }

  def updateProject(id: ProjectId, usrId: UserId, name: String): UpdateProjectResult = {
    import UpdateProjectResult._
    saveProject[UpdateProjectResult](name, name => {
      if (RenameProject(name, id, usrId).first == 0) ProjectNotFound
      else DbSuccess
    })(NameAlreadyInUse)
  }

  def findProject(id: ProjectId): Option[Project] = FindProject(id).firstOption

  def deleteProjectSoft(id: ProjectId): Unit = DeleteProjectSoft(nextFuncName, id).execute
}

// #####################################################################################################################

/**
 * All database interfacing methods, including those that require a transaction.
 */
sealed trait DaoT extends DaoS with EventDao {
  import Sql._

  def performUserRegistration(token: String)(
    username: Username, ps: PasswordAndSalt, ipAddr: String)(
    name: String, newsletter: Boolean): UserRegistrationResult = {

    import UserRegistrationResult._
    try {
      RegisterUser(username, ps, ipAddr, token).firstOption match {
        case Some(id) =>
          InsertUsrd(id, name, newsletter).execute
          DbSuccess(id)
        case None =>
          NoMatchingConfToken
      }
    } catch {
      case e: PSQLException if e.getMessage.contains("usr_username_key") => UsernameTaken
    }
  }
}


// #####################################################################################################################

/**
 * DAO for administrative and diagnostic purposes.
 */
sealed class AdminDao(_session: Session) extends DaoS {
  import AdminSql._
  implicit val session = _session

  def diagSelectNow() = DiagSelectNow.first

  def statsCountUsers: UsrCount = {
    val (a, b) = StatsCountUsers.first
    UsrCount(a, b)
  }

  def statsTableSizes: List[(String, Long)] = StatsSizesByTypes("r").list
  def statsIndexSizes: List[(String, Long)] = StatsSizesByTypes("i").list
  def statsDatabaseSize: Long = StatsDatabaseSize(DB.DatabaseName.replaceFirst("^.*/", "")).first
}

// #####################################################################################################################

sealed trait UserRegistrationResult
object UserRegistrationResult {
  case class DbSuccess(userId: UserId) extends UserRegistrationResult
  case object NoMatchingConfToken extends UserRegistrationResult
  case object UsernameTaken extends UserRegistrationResult
}

sealed trait CreateProjectResult
object CreateProjectResult {
  case class DbSuccess(id: ProjectId) extends CreateProjectResult
  case object NameAlreadyInUse extends CreateProjectResult
}

sealed trait UpdateProjectResult
object UpdateProjectResult {
  case object DbSuccess extends UpdateProjectResult
  case object NameAlreadyInUse extends UpdateProjectResult
  case object ProjectNotFound extends UpdateProjectResult
}
