package com.beardedlogic.usecase
package db

import org.postgresql.util.PSQLException
import scala.slick.driver.PostgresDriver.simple._
import scalaz.NonEmptyList.nel
import feature.uc.field.FieldDefinition
import lib.Locks.{UseCaseNumbers, SingleUseCase}
import lib.Types._
import security.PasswordAndSalt
import util.Lock

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
  def createUserPlaceholder(email: String @@ Validated, token: String): Unit = InsertUserPlaceholder.execute(email, token)

  def performUserRegistration(token: String)(username: String @@ Validated, ps: PasswordAndSalt, ipAddr: String): UserRegistrationResult = {
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

  private[this] def saveProject[R](name: String @@ Validated, saveFn: String => R)(nameAlreadyInUse: R): R =
    try saveFn(name)
    catch {
      case e: PSQLException if e.getMessage.contains("_usr_id_name_") => nameAlreadyInUse
    }

  def createProject(usrId: UserId, name: String @@ Validated): CreateProjectResult = {
    import CreateProjectResult._
    saveProject[CreateProjectResult](name, name => {
      val id = CreateProject.first(usrId, name)
      Success(id)
    })(NameAlreadyInUse)
  }

  def updateProject(id: ProjectId, usrId: UserId, name: String @@ Validated): UpdateProjectResult = {
    import UpdateProjectResult._
    saveProject[UpdateProjectResult](name, name => {
      if (RenameProject.first(name, id, usrId) == 0) ProjectNotFound
      else Success
    })(NameAlreadyInUse)
  }

  def findProject(id: ProjectId): Option[Project] = FindProject.firstOption(id)

  def findProjectByUc(ucId: UseCaseIdentId): Option[Project] = FindProjectByUc.firstOption(ucId)

  def summariseProjects(userId: UserId): List[ProjectSummary] = SummariseProjects.list(userId)

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

  def findAllLatestUseCaseRevs(pid: ProjectId, ids: List[UseCaseIdentId]): List[UseCaseRev] = ids match {
    case Nil       => List.empty
    case id :: Nil => findUseCaseLatestRev(id).toList
    case h :: t    => SelectLatestUseCaseRevsArb(UseCaseIdentIdIn(nel(h, t))).list(pid)
  }

  def summariseUseCases(projectId: ProjectId): List[UseCaseSummary] = SummariseUseCases.list(projectId)
  def summariseUseCases2(projectId: ProjectId): List[UseCaseSummary2] = SummariseUseCases2.list(projectId)

  // ===================================================================================================================
  // uc_field

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldTextWithFK] =
    SelectUcFields.list(ucRevId)

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
}

// #####################################################################################################################

/**
 * All database interfacing methods, including those that require a transaction.
 */
sealed trait DaoT extends DaoS {
  import lib.Misc.ShortExt
  import Sql._

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
          Success(newRev)
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
  case class Success(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case class AlreadyUpToDate(result: UseCaseRev) extends UseCaseHeaderUpdateResult
  case object UseCaseNotFound extends UseCaseHeaderUpdateResult
}

sealed trait CreateProjectResult
object CreateProjectResult {
  case class Success(id: ProjectId) extends CreateProjectResult
  case object NameAlreadyInUse extends CreateProjectResult
}

sealed trait UpdateProjectResult
object UpdateProjectResult {
  case object Success extends UpdateProjectResult
  case object NameAlreadyInUse extends UpdateProjectResult
  case object ProjectNotFound extends UpdateProjectResult
}
