package shipreq.webapp.db

import net.liftweb.util.Helpers.nextFuncName
import org.postgresql.util.PSQLException
import scala.slick.jdbc.JdbcBackend.Session
import shipreq.webapp.feature.uc.field.FieldDefinition
import shipreq.webapp.feature.UcFilter
import shipreq.webapp.lib.Locks.{UseCaseNumbers, SingleUseCase}
import shipreq.webapp.lib.Misc.retry
import shipreq.webapp.lib.ShareUrlTokenGen
import shipreq.webapp.lib.Types._
import shipreq.webapp.security.PasswordAndSalt
import shipreq.webapp.util.Lock

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
    tokenAttempt(tokenFn)(token => InsertUserPlaceholder.execute(email, token))

  def updateUserConfirmationToken(id: UserId, tokenFn: () => String): String =
    tokenAttempt(tokenFn)(token => UpdateConfirmationToken.execute(token, id))

  def findUserDescAndCredentials(usernameOrEmail: String): Option[(UserDescriptor, PasswordAndSalt)] =
    if (usernameOrEmail.indexOf('@') == -1)
      findUserDescAndCredentialsByUsername(usernameOrEmail)
    else
      findUserDescAndCredentialsByEmail(usernameOrEmail)

  def findUserDescAndCredentialsByUsername(username: String) = GetUserDescCredByUsername.firstOption(username)

  def findUserDescAndCredentialsByEmail(email: String) = GetUserDescCredByEmail.firstOption(email)

  def findUserRegistrationInfo(email: String) = GetUserRegInfo.firstOption(email)

  def findUserRegAndResetPwInfo(email: String) = GetUserRegAndResetPwInfo.firstOption(email)

  def findUserConfirmationTokenIssuedDate(token: String) = GetConfirmationTokenIssuedDate.firstOption(token)

  def findUserSuppAndDetail(id: UserId) = GetUserSuppAndDetail.firstOption(id)

  def logUserLogin(id: UserId, ip: Option[String]): Unit = LogUserLogin.execute(id, ip)

  def updateUserDetails(id: UserId, d: UserDetail): Unit =
    UpdateUserDetails.execute(d.name, d.newsletter, id)

  def updateUserPassword(id: UserId, ps: PasswordAndSalt): Unit = UpdateUserPassword.execute(ps, id)

  def performInstallNewResetPasswordToken(u: UserId, tokenFn: () => String): String =
    tokenAttempt(tokenFn)(token => InstallNewResetPasswordToken.execute(token, u))

  def performReuseResetPasswordToken(u: UserId): Unit = ReuseResetPasswordToken.execute(u)

  def findResetPasswordTokenIssuedDate(token: String) = GetResetPasswordTokenIssuedDate.firstOption(token)

  def performPasswordReset(ps: PasswordAndSalt, token: String) = ResetPassword.execute(ps, token)

  // ===================================================================================================================
  // Project

  private[this] def saveProject[R](name: String @@ Validated, saveFn: String => R)(nameAlreadyInUse: R): R =
    try saveFn(name)
    catch {
      case e: PSQLException if e.getMessage.contains("_usr_id_name_") => nameAlreadyInUse
    }

  def createProject(usrId: UserId, name: String @@ Validated): CreateProjectResult = {
    import CreateProjectResult._
    saveProject[CreateProjectResult](name, name => {
      val id = CreateProject.first(usrId, name)
      DbSuccess(id)
    })(NameAlreadyInUse)
  }

  def updateProject(id: ProjectId, usrId: UserId, name: String @@ Validated): UpdateProjectResult = {
    import UpdateProjectResult._
    saveProject[UpdateProjectResult](name, name => {
      if (RenameProject.first(name, id, usrId) == 0) ProjectNotFound
      else DbSuccess
    })(NameAlreadyInUse)
  }

  def findProject(id: ProjectId): Option[Project] = FindProject.firstOption(id)

  def findProjectByUc(ucId: UseCaseIdentId): Option[Project] = FindProjectByUc.firstOption(ucId)

  def summariseProjects(userId: UserId): List[ProjectSummary] = SummariseProjects.list(userId)

  def deleteProjectSoft(id: ProjectId): Unit = DeleteProjectSoft.execute(nextFuncName, id)

  // ===================================================================================================================
  // Use Case

  def createUseCaseRev(ucIdent: UseCaseIdent, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val (id, createdAt) = InsertUseCaseRev.first(ucIdent, rev, header.title)
    UseCaseRev(ucIdent, rev, id, header, createdAt)
  }

  def findUseCaseRev(revId: UseCaseRevId): Option[UseCaseRev] = SelectUseCaseRev.firstOption(revId)

  def findUseCaseLatestRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestUseCaseRevId.firstOption(ucId)

  def findUseCaseLatestRev(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestUseCaseRev.firstOption(ucId)

  def findAllLatestUseCaseRevsByProject(pid: ProjectId): List[UseCaseRev] = SelectLatestUseCaseRevsByProject.list(pid)

  def findAllLatestUseCaseRevs(pid: ProjectId, ids: List[UseCaseIdentId]): List[UseCaseRev] =
    if (ids.isEmpty)
      List.empty
    else
      SelectLatestUseCaseRevsByIds.list(pid, ids)

  def summariseUseCases(projectId: ProjectId): List[UseCaseSummary] = SummariseUseCases.list(projectId)

  // ===================================================================================================================
  // uc_field

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldTextWithFK] =
    SelectUcFields.list(ucRevId)

  def findAllUcFieldData(ids: List[UseCaseRevId]): List[(UseCaseRevId, UcFieldTextWithFK)] =
    if (ids.isEmpty)
      List.empty
    else
      SelectUcFieldsInBulk.list(ids)

  def linkUcToText(uc: UseCaseRevId, txt: TextRevId): Unit =
    LinkUcToText.execute(uc, txt)

  def linkUcToStep(uc: UseCaseRevId, label: StepLabel, index: Short, parentId: Option[TextRevId], text: TextRev): UcFieldText = {
    LinkUcToStep.execute(uc, label, parentId, index, text.id)
    UcFieldText(Some(label), parentId, index, text)
  }

  def linkUcToSameFieldsAsOtherUc(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs.execute(to, from)

  // ===================================================================================================================
  // Text

  def createTextIdent(ucId: UseCaseIdentId, fkId: FieldKeyId): TextIdentId =
    InsertTextIdent.first(ucId, fkId)

  def createTextRev(identId: TextIdentId, rev: Short, text: NormalisedText): TextRev = {
    val id = InsertTextRev.first(identId, rev, text)
    TextRev(identId, rev, id, text)
  }

  // ===================================================================================================================
  // Fields

  def createFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = {
    val id = InsertFieldKey.first(fkType, data)
    FieldKeyRec(id, fkType, data)
  }

  // ===================================================================================================================
  // Shares

  def createShare(
    projectId: ProjectId, ps: PasswordAndSalt,
    name: String, preface: Option[String], ucFilterJson: Json[UcFilter],
    urlTokenFn: () => ShareUrlToken = ShareUrlTokenGen.fn)
  : Share =
    retry(12) { // Chance of error when 200,000 tokens in use = 0.0000002% ^ 12 (6E-105%)
      inSafeTransaction {
        val urlToken = urlTokenFn()
        val id = InsertShare.first(projectId, urlToken, ps, name, preface, ucFilterJson)
        Share(id, projectId, urlToken, name, preface, ucFilterJson)
      }
    }

  def updateShare(id: ShareId, name: String, preface: Option[String], ucFilterJson: Json[UcFilter]): Unit =
    UpdateShare.execute(name, preface, ucFilterJson, id)

  def updateSharePassword(id: ShareId, ps: PasswordAndSalt): Unit =
    UpdateSharePassword.execute(ps, id)

  def deleteShare(id: ShareId): Unit =
    DeleteShare.execute(id)

  def findShare(id: ShareId): Option[Share] =
    SelectShare.firstOption(id)

  def findShareAndPassword(url: ShareUrlToken): Option[(Share, PasswordAndSalt)] =
    SelectShareAndPasswordByUrl.firstOption(url)

  def findShareAndProject(url: ShareUrlToken): Option[(Share, Project)] =
    SelectShareAndProjectByUrl.firstOption(url)

  def summariseShares(projectId: ProjectId): List[ShareSummary] =
    SummariseShares.list(projectId)

  def logShareView(shareId: ShareId, ip: Option[String]): Unit =
    LogShareView.execute(shareId, ip)
}

// #####################################################################################################################

/**
 * All database interfacing methods, including those that require a transaction.
 */
sealed trait DaoT extends DaoS {
  import shipreq.webapp.lib.Misc.ShortExt
  import Sql._

  def performUserRegistration(token: String)(
    username: String @@ Validated, ps: PasswordAndSalt, ipAddr: String)(
    name: String @@ Validated, newsletter: Boolean): UserRegistrationResult = {

    import UserRegistrationResult._
    try {
      RegisterUser.firstOption(username, ps, ipAddr, token) match {
        case Some(id) =>
          InsertUsrd.execute(id, name, newsletter)
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
  def createUseCaseIdent(projectId: ProjectId): UseCaseIdent = InsertUseCaseIdent.first(projectId, projectId)

  /**
   * Same as `#createUseCaseIdent` except uses a manually-provided UC number.
   * This should only be used in tests.
   */
  def createUseCaseIdentWithForcedNumber(projectId: ProjectId, ucn: UseCaseNumber): UseCaseIdent = {
    val id = InsertUseCaseIdentForceNum.first(projectId, ucn)
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

  def findOrCreateFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = {
    SelectReusableFieldKeyId.firstOption(fkType, data)
    .map(FieldKeyRec(_, fkType, data))
    .getOrElse(createFieldKey(fkType, data))
  }

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
    val (a, b) = StatsCountUsers.first()
    UsrCount(a, b)
  }

  def statsTableSizes: List[(String, Long)] = StatsSizesByTypes.list("r")
  def statsIndexSizes: List[(String, Long)] = StatsSizesByTypes.list("i")
  def statsDatabaseSize: Long = StatsDatabaseSize.first(DB.DatabaseName.replaceFirst("^.*/", ""))
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
