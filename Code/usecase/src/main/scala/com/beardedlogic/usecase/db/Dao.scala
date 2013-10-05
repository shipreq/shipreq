package com.beardedlogic.usecase
package db

import org.postgresql.util.PSQLException
import scala.slick.driver.PostgresDriver.simple._
import lib.field.FieldDefinition
import lib.{Defaults, InputCorrection}
import lib.Types._
import security.PasswordAndSalt

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
 *
 * - `perform`: Perform specialised business-logic (as opposed to CRUD-like operations).
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

  // ===================================================================================================================
  // User

  /** Creates an unconfirmed user account. No username, no password until email confirmed. */
  def createUserPlaceholder(email: String, token: String): Unit = InsertUserPlaceholder.execute(email, token)

  def performUserRegistration(token: String)(username: String, ps: PasswordAndSalt, ipAddr: String): UserRegistrationResult = {
    import UserRegistrationResult._
    try {
      RegisterUser.firstOption(username, ps, ipAddr, token) match {
        case Some(id) => Success(id)
        case None => NoMatchingConfToken
      }
    } catch {
      case e: PSQLException if e.getMessage.contains("usr_username_key") => UsernameTaken
    }
  }

  def updateUserConfirmationToken(id: UserId, token: String): Unit = UpdateConfirmationToken.execute(token, id)

  def findUserDescAndCredentials(usernameOrEmail: String): Option[(UserDescriptor, PasswordAndSalt)] =
    if (usernameOrEmail.indexOf('@') == -1)
      findUserDescAndCredentialsByUsername(usernameOrEmail)
    else
      findUserDescAndCredentialsByEmail(usernameOrEmail)

  def findUserDescAndCredentialsByUsername(username: String) = GetUserDescCredByUsername.firstOption(username)

  def findUserDescAndCredentialsByEmail(email: String) = GetUserDescCredByEmail.firstOption(email)

  def findUserRegistrationInfo(email: String) = GetUserRegInfo.firstOption(email)

  def findUserConfirmationTokenIssuedDate(token: String) = GetConfirmationTokenIssuedDate.firstOption(token)

  def logUserLogin(id: UserId, ipAddr: String): Unit = LogUserLogin.execute(ipAddr, id)

  // ===================================================================================================================
  // Project

  private[this] def saveProject[R](rawName: String, f: String => R)(invalidName: R, nameAlreadyInUse: R): R = {
    val name = InputCorrection.projectName(rawName)
    if (name.isEmpty)
      invalidName
    else try
      f(name)
    catch {
      case e: PSQLException if e.getMessage.contains("_usr_id_name_") => nameAlreadyInUse
    }
  }

  def createProject(usrId: UserId, rawName: String): CreateProjectResult = {
    import CreateProjectResult._
    saveProject[CreateProjectResult](rawName, name => {
      val id = CreateProject.first(usrId, name)
      Success(id)
    })(InvalidName, NameAlreadyInUse)
  }

  def updateProject(id: ProjectId, usrId: UserId, rawName: String): UpdateProjectResult = {
    import UpdateProjectResult._
    saveProject[UpdateProjectResult](rawName, name => {
      if (RenameProject.first(name, id, usrId) == 0)
        ProjectNotFound
      else
        Success(name)
    })(InvalidName, NameAlreadyInUse)
  }

  def findProject(id: ProjectId): Option[Project] = FindProject.firstOption(id)

  def findProjectByUc(ucId: UseCaseIdentId): Option[Project] = FindProjectByUc.firstOption(ucId)

  def summariseProjects(userId: UserId): List[ProjectSummary] = SummariseProjects.list(userId)

  // ===================================================================================================================
  // Use Case

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
    UseCaseIdent(id, ucn)
  }

  protected def createUseCaseRevWithoutCorrection(ident: UseCaseIdent, rev: Short, h: UseCaseHeader): UseCaseRev = {
    val id = InsertUseCaseRev.first(ident, rev, h.title)
    UseCaseRev(ident, rev, id, h)
  }

  def createUseCaseRev(ucIdent: UseCaseIdent, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val uch = InputCorrection.correct(header)
    createUseCaseRevWithoutCorrection(ucIdent, rev, uch)
  }

  // TODO New-UC: Use table locking for mutex?

  def findUseCaseRev(revId: UseCaseRevId): Option[UseCaseRev] = SelectUseCaseRev.firstOption(revId)

  def findUseCaseLatestRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestUseCaseRevId.firstOption(ucId)

  def findUseCaseLatestRev(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestUseCaseRev.firstOption(ucId)

  def summariseUseCases(projectId: ProjectId): List[UseCaseSummary] = SummariseUseCases.list(projectId)

  // ===================================================================================================================
  // uc_field

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldTextWithFK] =
    SelectUcFields.list(ucRevId)

  def linkUcToText(uc: UseCaseRevId, txt: TextRevId): Unit =
    LinkUcToText.execute(uc, txt)

  def linkUcToStep(uc: UseCaseRevId, label: LabelStr, index: Short, parentId: Option[TextRevId], text: TextRev): UcFieldText = {
    LinkUcToStep.execute(uc, label, parentId, index, text.id)
    UcFieldText(Some(label), parentId, index, text)
  }

  def linkUcToSameFieldsAsOtherUc(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs.execute(to, from)

  // ===================================================================================================================
  // Text

  def createTextIdent(ucId: UseCaseIdentId, fkId: FieldKeyId): TextIdentId =
    InsertTextIdent.first(ucId, fkId)

  def createTextRev(identId: TextIdentId, rev: Short, text: TextWithNormalisedRefs): TextRev = {
    val id = InsertTextRev.first(identId, rev, text)
    TextRev(identId, rev, id, text)
  }

  // ===================================================================================================================
  // Fields

  def createFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = {
    val id = InsertFieldKey.first(fkType, data)
    FieldKeyRec(id, fkType, data)
  }
}

// #####################################################################################################################

/**
 * All database interfacing methods, including those that require a transaction.
 */
sealed trait DaoT extends DaoS {
  import lib.Misc.ShortExt
  import Sql._

  def createUseCaseIdentAndRev1(projectId: ProjectId, header: UseCaseHeader): UseCaseRev = {
    val ident = createUseCaseIdent(projectId)
    val h = InputCorrection.correct(header)
    val rev = 1: Short
    createUseCaseRevWithoutCorrection(ident, rev, h)
  }

  /**
   * Updates the header of an existing use case (ie. just the contents of the `usecase` table ignoring its relations).
   *
   * When updating just the title of an Untitled rev #1 UC, the update is direct. In all other cases requiring an
   * update, a new revision is created.
   */
  def updateUseCaseHeader(ucId: UseCaseIdentId, modFn: UseCaseHeader => UseCaseHeader): UseCaseHeaderUpdateResult = {
    // TODO locking? race conditions here? ensure DB mutex
    import UseCaseHeaderUpdateResult._

    findUseCaseLatestRev(ucId) match {
      case None => UseCaseNotFound

      case Some(latest) =>
        val newHeader = InputCorrection.correct(modFn(latest.header))

        // NOP
        if (latest.header == newHeader)
          AlreadyUpToDate(latest)

        // Rev #1 title update
        else if (latest.rev == 1 && latest.header == newHeader.copy(title = Defaults.title)) {
          UpdateUseCaseTitleDirect.execute(newHeader.title, latest.id)
          DirectUpdate(latest.copy(header = newHeader))
        }

        // Audited update
        else {
          val newRev = createUseCaseRevWithoutCorrection(latest.ident, latest.rev +! 1, newHeader)
          linkUcToSameFieldsAsOtherUc(latest, newRev)
          NewRevision(newRev)
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

sealed trait UserRegistrationResult
object UserRegistrationResult {
  case class Success(userId: UserId) extends UserRegistrationResult
  case object NoMatchingConfToken extends UserRegistrationResult
  case object UsernameTaken extends UserRegistrationResult
}

sealed trait UseCaseHeaderUpdateResult
object UseCaseHeaderUpdateResult {
  case class NewRevision(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case class DirectUpdate(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case class AlreadyUpToDate(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case object UseCaseNotFound extends UseCaseHeaderUpdateResult
}

sealed trait CreateProjectResult
object CreateProjectResult {
  case class Success(id: ProjectId) extends CreateProjectResult
  case object InvalidName extends CreateProjectResult
  case object NameAlreadyInUse extends CreateProjectResult
}

sealed trait UpdateProjectResult
object UpdateProjectResult {
  case class Success(name: String) extends UpdateProjectResult
  case object InvalidName extends UpdateProjectResult
  case object NameAlreadyInUse extends UpdateProjectResult
  case object ProjectNotFound extends UpdateProjectResult
}
