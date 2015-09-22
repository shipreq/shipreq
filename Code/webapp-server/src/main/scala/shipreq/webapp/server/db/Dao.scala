package shipreq.webapp.server.db

import net.liftweb.util.Helpers.nextFuncName
import org.postgresql.util.PSQLException
import scala.slick.jdbc.JdbcBackend.Session
import shipreq.base.util.TaggedTypes.JsonStr
import shipreq.taskman.api.{UserId, EmailAddr}
import shipreq.webapp.server.feature.uc.field.FieldDefinition
import shipreq.webapp.server.feature.UcFilter
import shipreq.webapp.server.lib.Locks.{UseCaseNumbers, SingleUseCase}
import shipreq.webapp.server.lib.Misc.retry
import shipreq.webapp.server.lib.ShareUrlTokenGen
import shipreq.webapp.server.lib.Types._
import shipreq.webapp.server.security.PasswordAndSalt
import shipreq.webapp.server.util.Lock

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
      findUserDescAndCredentials(Username(usernameOrEmail))
    else
      findUserDescAndCredentials(EmailAddr(usernameOrEmail))

  def findUserDescAndCredentials(username: Username) = GetUserDescCredByUsername(username).firstOption

  def findUserDescAndCredentials(email: EmailAddr) = GetUserDescCredByEmail(email).firstOption

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

  def findProjectByUc(ucId: UseCaseIdentId): Option[Project] = FindProjectByUc(ucId).firstOption

  def summariseProjects(userId: UserId): List[ProjectSummary] = SummariseProjects(userId).list

  def deleteProjectSoft(id: ProjectId): Unit = DeleteProjectSoft(nextFuncName, id).execute

  // ===================================================================================================================
  // Use Case

  def createUseCaseRev(ucIdent: UseCaseIdent, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val (id, createdAt) = InsertUseCaseRev(ucIdent, rev, header.title).first
    UseCaseRev(ucIdent, rev, id, header, createdAt)
  }

  def findUseCaseRev(revId: UseCaseRevId): Option[UseCaseRev] = SelectUseCaseRev(revId).firstOption

  def findUseCaseLatestRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestUseCaseRevId(ucId).firstOption

  def findUseCaseLatestRev(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestUseCaseRev(ucId).firstOption

  def findAllLatestUseCaseRevsByProject(pid: ProjectId): List[UseCaseRev] = SelectLatestUseCaseRevsByProject(pid).list

  def findAllLatestUseCaseRevs(pid: ProjectId, ids: List[UseCaseIdentId]): List[UseCaseRev] =
    if (ids.isEmpty)
      List.empty
    else
      SelectLatestUseCaseRevsByIds(pid, ids).list

  def summariseUseCases(projectId: ProjectId): List[UseCaseSummary] = SummariseUseCases(projectId).list

  // ===================================================================================================================
  // uc_field

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldTextWithFK] =
    SelectUcFields(ucRevId).list

  def findAllUcFieldData(ids: List[UseCaseRevId]): List[(UseCaseRevId, UcFieldTextWithFK)] =
    if (ids.isEmpty)
      List.empty
    else
      SelectUcFieldsInBulk(ids).list

  def linkUcToText(uc: UseCaseRevId, txt: TextRevId): Unit =
    LinkUcToText(uc, txt).execute

  def linkUcToStep(uc: UseCaseRevId, label: StepLabel, index: Short, parentId: Option[TextRevId], text: TextRev): UcFieldText = {
    LinkUcToStep(uc, label, parentId, index, text.id).execute
    UcFieldText(Some(label), parentId, index, text)
  }

  def linkUcToSameFieldsAsOtherUc(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs(to, from).execute

  // ===================================================================================================================
  // Text

  def createTextIdent(ucId: UseCaseIdentId, fkId: FieldKeyId): TextIdentId =
    InsertTextIdent(ucId, fkId).first

  def createTextRev(identId: TextIdentId, rev: Short, text: NormalisedText): TextRev = {
    val id = InsertTextRev(identId, rev, text).first
    TextRev(identId, rev, id, text)
  }

  // ===================================================================================================================
  // Fields

  def createFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = {
    val id = InsertFieldKey(fkType, data).first
    FieldKeyRec(id, fkType, data)
  }

  // ===================================================================================================================
  // Shares

  def createShare(
    projectId: ProjectId, ps: PasswordAndSalt,
    name: String, preface: Option[String], ucFilterJson: JsonStr[UcFilter],
    urlTokenFn: () => ShareUrlToken = ShareUrlTokenGen.fn)
  : Share =
    retry(12) { // Chance of error when 200,000 tokens in use = 0.0000002% ^ 12 (6E-105%)
      inSafeTransaction {
        val urlToken = urlTokenFn()
        val id = InsertShare(projectId, urlToken, ps, name, preface, ucFilterJson).first
        Share(id, projectId, urlToken, name, preface, ucFilterJson)
      }
    }

  def updateShare(id: ShareId, name: String, preface: Option[String], ucFilterJson: JsonStr[UcFilter]): Unit =
    UpdateShare(name, preface, ucFilterJson, id).execute

  def updateSharePassword(id: ShareId, ps: PasswordAndSalt): Unit =
    UpdateSharePassword(ps, id).execute

  def deleteShare(id: ShareId): Unit =
    DeleteShare(id).execute

  def findShare(id: ShareId): Option[Share] =
    SelectShare(id).firstOption

  def findShareAndPassword(url: ShareUrlToken): Option[(Share, PasswordAndSalt)] =
    SelectShareAndPasswordByUrl(url).firstOption

  def findShareAndProject(url: ShareUrlToken): Option[(Share, Project)] =
    SelectShareAndProjectByUrl(url).firstOption

  def summariseShares(projectId: ProjectId): List[ShareSummary] =
    SummariseShares(projectId).list

  def logShareView(shareId: ShareId, ip: Option[String]): Unit =
    LogShareView(shareId, ip).execute
}

// #####################################################################################################################

/**
 * All database interfacing methods, including those that require a transaction.
 */
sealed trait DaoT extends DaoS with EventDao {
  import shipreq.webapp.server.lib.Misc.ShortExt
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

  /**
   * Creates a new `usecase` row. If a `usecase_rev` row is not inserted before the end of the transaction, then the
   * transaction will fail because `usecase.latest_rev_id` will be `NULL`.
   */
  def createUseCaseIdent(projectId: ProjectId): UseCaseIdent = InsertUseCaseIdent(projectId, projectId).first

  /**
   * Same as `#createUseCaseIdent` except uses a manually-provided UC number.
   * This should only be used in tests.
   */
  def createUseCaseIdentWithForcedNumber(projectId: ProjectId, ucn: UseCaseNumber): UseCaseIdent = {
    val id = InsertUseCaseIdentForceNum(projectId, ucn).first
    UseCaseIdent(id, ucn, projectId)
  }

  def createUseCaseIdentAndRev1(projectId: ProjectId, header: UseCaseHeader, lock: Lock.Write[UseCaseNumbers]): UseCaseRev = {
    val ident = createUseCaseIdent(projectId)
    val rev = 1: Short
    createUseCaseRev(ident, rev, header)
  }

  /**
   * Updates the header of an existing use case (ie. just the contents of the `usecase` table ignoring its relations).
   *
   * When updating just the title of an Untitled rev #1 UC, the update is direct. In all other cases requiring an
   * update, a new revision is created.
   */
  def updateUseCaseHeader(ucId: UseCaseIdentId, modFn: UseCaseHeader => UseCaseHeader, lock: Lock.Write[SingleUseCase]): UseCaseHeaderUpdateResult = {
    import UseCaseHeaderUpdateResult._

    findUseCaseLatestRev(ucId) match {
      case None => UseCaseNotFound
      case Some(latest) =>
        val h = modFn(latest.header)
        if (h == latest.header)
          AlreadyUpToDate(latest)
        else {
          val newRev = createUseCaseRev(latest.ident, latest.rev +! 1, h)
          linkUcToSameFieldsAsOtherUc(latest, newRev)
          DbSuccess(newRev)
        }
    }
  }

  def findOrCreateFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec =
    SelectReusableFieldKeyId(fkType, data).firstOption
      .fold(createFieldKey(fkType, data))(FieldKeyRec(_, fkType, data))

  def syncFieldList(fields: List[FieldDefinition]): FieldListRec = {
    val fkRecs = fields.map(f => findOrCreateFieldKey(f.fieldKeyType, f.fieldKeyData))
    FieldListRec(fkRecs)
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

sealed trait UseCaseHeaderUpdateResult
object UseCaseHeaderUpdateResult {
  case class DbSuccess(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case class AlreadyUpToDate(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case object UseCaseNotFound extends UseCaseHeaderUpdateResult
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
