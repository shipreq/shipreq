package com.beardedlogic.usecase
package db

import org.joda.time.DateTime
import org.postgresql.util.PSQLException
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery, SetParameter, GetResult}
import scala.slick.session.PositionedParameters
import lib.field.FieldDefinition
import lib.security.PasswordAndSalt
import lib.{Defaults, InputCorrection}
import lib.Types._

/**
 * Database interface.
 *
 * Methods follow this pattern:
 *
 * - `find`: Searches for a single row. Returns Option[T].
 * - `findAll`: Searches for a multiple rows. Returns List[T], possibly empty.
 * - `findOrCreate`: Searches for an item and creates it if not found. Returns T.
 *
 * - `create`: Creates a new row. Returns T.
 * - `link`: Creates a link/mapping between existing DB records. Returns unit.
 * - `log`: Records a loggable event. Returns unit.
 * - `sync`: Ensures the DB state of a record matches a provided model, changing the DB if necessary.
 * - `update`: Modifies an existing DB record. Return specialised result.
 *
 * - `perform`: Perform specialised business-logic (as opposed to CRUD-like operations).
 */
class Dao(_session: Session) {
  import lib.Misc.ShortExt
  import Sql._

  implicit final val session = _session

  final def withTransaction[T](f: => T): T = session.withTransaction(f)

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

  private def createUseCaseRevWithoutCorrection(ident: UseCaseIdent, rev: Short, h: UseCaseHeader): UseCaseRev = {
    val id = InsertUseCaseRev.first(ident, rev, h.title)
    UseCaseRev(ident, rev, id, h)
  }

  def createUseCaseRev(ucIdent: UseCaseIdent, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val uch = InputCorrection.correct(header)
    createUseCaseRevWithoutCorrection(ucIdent, rev, uch)
  }

  // TODO New-UC: Use table locking for mutex?
  def createUseCaseIdentAndRev1(projectId: ProjectId, header: UseCaseHeader): UseCaseRev = withTransaction {
    val ident = createUseCaseIdent(projectId)
    val h = InputCorrection.correct(header)
    val rev = 1: Short
    createUseCaseRevWithoutCorrection(ident, rev, h)
  }

  def findUseCaseRev(revId: UseCaseRevId): Option[UseCaseRev] = SelectUseCaseRev.firstOption(revId)

  def findUseCaseLatestRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestUseCaseRevId.firstOption(ucId)

  def findUseCaseLatestRev(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestUseCaseRev.firstOption(ucId)

  def findAllUseCaseSummaries(projectId: ProjectId): List[UseCaseSummary] = SelectUseCaseSummaries.list(projectId)

  /**
   * Updates the header of an existing use case (ie. just the contents of the `usecase` table ignoring its relations).
   *
   * When updating just the title of an Untitled rev #1 UC, the update is direct. In all other cases requiring an
   * update, a new revision is created.
   */
  def updateUseCaseHeader(ucId: UseCaseIdentId, modFn: UseCaseHeader => UseCaseHeader): UseCaseHeaderUpdateResult = withTransaction {
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

  def findOrCreateFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = withTransaction {
    SelectReusableFieldKeyId.firstOption(fkType, data)
    .map(FieldKeyRec(_, fkType, data))
    .getOrElse(createFieldKey(fkType, data))
  }

  def syncFieldList(fields: List[FieldDefinition]): FieldListRec = withTransaction {
    val fkRecs = fields.map(f => findOrCreateFieldKey(f.fieldKeyType, f.fieldKeyData))
    FieldListRec(fkRecs)
  }
}

/**
 * SQL for all functions exposed in the DAO.
 */
private[db] final object Sql {
  import SqlHelpers._
  import StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_FieldKey = GetResult {r => FieldKeyRec(r.<<, r.<<, r.<<)}
  implicit val GR_PasswordAndSalt = GetResult(r => PasswordAndSalt(r.nextString, r.nextString))
  implicit val GR_TextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldText= GetResult(r => UcFieldText(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))
  implicit val GR_UseCaseIdent = GetResult {r => UseCaseIdent(r.<<, r.<<)}
  implicit val GR_UseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<)))
  implicit val GR_UseCaseSummary = GetResult(r => UseCaseSummary.as(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword)
      pp.setString(v.salt)
    }
  }

  private[this] case class Update() extends scala.annotation.StaticAnnotation
  private[this] case class Insert() extends scala.annotation.StaticAnnotation

  // ###################################################################################################################
  // User

  private val UserDescCols = "id,username,email"
  private val PwdAndSaltCols = "password, password_salt"

  val GetUserDescCredByUsername = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")

  val GetUserDescCredByEmail = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val GetUserRegInfo = query[String, UserRegistrationInfo](
    "SELECT id, confirmation_token, confirmation_sent_at, confirmed_at FROM usr WHERE email=?")

  val GetConfirmationTokenIssuedDate = query[String, DateTime](
    "SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

  @Update val UpdateConfirmationToken = update[(String, UserId)](
    "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  @Update val LogUserLogin = update[(String, UserId)](
    "UPDATE usr SET login_count = login_count + 1, last_login_at = NOW(), last_login_ip = ? WHERE id=?")

  @Insert val InsertUserPlaceholder = update[(String, String)](
    "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

  @Update val RegisterUser = query[(String, PasswordAndSalt, String, String), UserId]( """
    UPDATE usr SET username = ?
      ,password = ?, password_salt = ?, password_changed_at = NOW()
      ,confirmation_token = NULL, confirmed_at = NOW()
      ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
    WHERE confirmation_token = ?
    RETURNING id""".sql)

  // ###################################################################################################################
  // Project

  @Insert val CreateProject = query[(UserId, String), ProjectId](
    "INSERT INTO project(usr_id, name) VALUES(?,?) RETURNING id")

  // ###################################################################################################################
  // Use Case

  private val ucrev_* = s"r.ident_id, u.number, r.rev, r.id, r.title"

  @Insert val InsertUseCaseIdent = {
    val NextUseCaseNumber = "(SELECT coalesce(max(number),0)+1 from usecase where project_id=?)"
    query[(ProjectId, ProjectId), UseCaseIdent](
      s"INSERT INTO usecase(project_id, number) VALUES(?, $NextUseCaseNumber) RETURNING id, number")
  }

  @Insert val InsertUseCaseIdentForceNum = query[(ProjectId, UseCaseNumber), UseCaseIdentId](
    "INSERT INTO usecase(project_id,number) VALUES(?,?) RETURNING id")

  @Insert val InsertUseCaseRev = query[(UseCaseIdentId, Short, String), UseCaseRevId](
    "INSERT INTO usecase_rev(ident_id, rev, title) VALUES(?,?,?) RETURNING id")

  val SelectUseCaseRev = query[UseCaseRevId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE u.id=r.ident_id AND r.id=?")

  val SelectLatestUseCaseRevId = query[UseCaseIdentId, UseCaseRevId](
    s"SELECT latest_rev_id FROM usecase WHERE id=?")

  val SelectLatestUseCaseRev = query[UseCaseIdentId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

  val SelectUseCaseSummaries = query[ProjectId, UseCaseSummary]( s"""
    SELECT ident_id, number, title, to_iso8601_str(created_at)
    FROM usecase u, usecase_rev r
    WHERE r.id = latest_rev_id and project_id = ?
    ORDER BY number """.sql)

  @Update val UpdateUseCaseTitleDirect = update[(String, UseCaseRevId)]("UPDATE usecase_rev SET title=? WHERE id=?")

  // ###################################################################################################################
  // Text

  private val textrev_* = "tr.ident_id, tr.rev, tr.id, tr.text"

  @Insert val InsertTextIdent = query[(UseCaseIdentId, FieldKeyId), TextIdentId](
    "INSERT INTO text(uc_id, fk_id) VALUES(?,?) RETURNING id")

  @Insert val InsertTextRev = query[(TextIdentId, Short, TextWithNormalisedRefs), TextRevId](
    "INSERT INTO text_rev(ident_id, rev, text) VALUES(?,?,?) RETURNING id")

  // ###################################################################################################################
  // uc_field

  @Insert val LinkUcToText = update[(UseCaseRevId, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, text_rev_id) VALUES(?,?)")

  @Insert val LinkUcToStep = update[(UseCaseRevId, LabelStr, Option[TextRevId], Short, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES(?,?,?,?,?)")

  @Insert val CopyUcFieldsBetweenRevs = update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ? """.sql)

  // Step loading depends on ORDER BY index
  val SelectUcFields = query[UseCaseRevId, UcFieldTextWithFK](s"""
    SELECT fk_id, label, parent_rev_id, index, ${textrev_*}
      FROM uc_field f, text_rev tr, text t
     WHERE text_rev_id = tr.id and tr.ident_id = t.id
       AND uc_rev_id = ?
     ORDER BY index """.sql)

  // ###################################################################################################################
  // Fields

  val SelectReusableFieldKeyId = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "SELECT id FROM field_key WHERE type_id=? AND data IS NOT DISTINCT FROM ?")

  @Insert val InsertFieldKey = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "INSERT INTO field_key(type_id, data) VALUES(?,?) RETURNING id")
}

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
