package com.beardedlogic.usecase
package db

import org.joda.time.DateTime
import org.postgresql.util.PSQLException
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery, SetParameter, GetResult}
import scala.slick.session.PositionedParameters
import lib.field.FieldDefinition
import lib.security.PasswordAndSalt
import lib.{Defaults, InputCorrection, UseCaseHeader}
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
  import DbHelpers._
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
  // Use Case

  /**
   * Creates a new `usecase` row. If a `usecase_rev` row is not inserted before the end of the transaction, then the
   * transaction will fail because `usecase.latest_rev_id` will be `NULL`.
   */
  def createUseCaseIdent(): UseCaseIdentId = InsertUseCaseIdent.first()

  private def createUseCaseRevWithoutCorrection(ucId: UseCaseIdentId, rev: Short, h: UseCaseHeader): UseCaseRev = {
    val id = InsertUseCaseRev.first(ucId, rev, h.number, h.title)
    UseCaseRev(ucId, rev, id, h)
  }

  def createUseCaseRev(ucId: UseCaseIdentId, rev: Short, header: UseCaseHeader): UseCaseRev = {
    val uch = InputCorrection.correct(header)
    createUseCaseRevWithoutCorrection(ucId, rev, uch)
  }

  // TODO Remove createUseCaseIdentAndRev1(header) after anonymous UC editing is removed
  def createUseCaseIdentAndRev1(header: UseCaseHeader): UseCaseRev = withTransaction {
    val identId = createUseCaseIdent()
    val h = InputCorrection.correct(header)
    val rev = 1: Short
    createUseCaseRevWithoutCorrection(identId, rev, h)
  }

  // TODO New-UC has GLOBAL scope.
  // TODO New-UC: Use table locking for mutex?
  // TODO New-UC: Lacking appropriate number uniqueness constraint
  // TODO need a usecase state so we can call correct() instead of correctUseCaseTitle(). Would also make stateEquals() redundant
  /**
   * Creates a `usecase` and a rev-#1 `usecase_rev`. The UC number is determined automatically.
   */
  def createUseCaseIdentAndRev1(title: String): UseCaseRev = withTransaction {
    val identId = createUseCaseIdent()
    val correctedTitle = InputCorrection.useCaseTitle(title)
    val (id, number) = InsertUseCaseRev1WithAutoNumber.first(identId, correctedTitle)
    UseCaseRev(identId, 1, id, UseCaseHeader(number, correctedTitle))
  }

  def findUseCaseRev(revId: UseCaseRevId): Option[UseCaseRev] = SelectUseCaseRev.firstOption(revId)

  def findUseCaseLatestRevId(ucId: UseCaseIdentId): Option[UseCaseRevId] = SelectLatestUseCaseRevId.firstOption(ucId)

  def findUseCaseLatestRev(ucId: UseCaseIdentId): Option[UseCaseRev] = SelectLatestUseCaseRevByIdent.firstOption(ucId)

  def findAllUseCaseSummaries(): List[UseCaseSummary] = SelectUseCaseSummaries.list

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
        else if (latest.rev == 1 && latest.header == newHeader.copy(title = Defaults.Title)) {
          UpdateUseCaseTitleDirect.execute(newHeader.title, latest.id)
          DirectUpdate(latest.copy(header = newHeader))
        }

        // Audited update
        else {
          val newRev = createUseCaseRevWithoutCorrection(ucId, latest.rev + 1, newHeader)
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
  import DbHelpers._
  import StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_FieldKey = GetResult {r => FieldKeyRec(r.<<, r.<<, r.<<)}
  implicit val GR_PasswordAndSalt = GetResult(r => PasswordAndSalt(r.nextString, r.nextString))
  implicit val GR_TextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldText= GetResult(r => UcFieldText(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))
  implicit val GR_UseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<, r.<<)))
  implicit val GR_UseCaseSummary = GetResult(r => new UseCaseSummary(r.nextId[UseCaseIdentId], r.nextShort, r.nextString, r.nextString))
  implicit val GR_UserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword)
      pp.setString(v.salt)
    }
  }

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

  val UpdateConfirmationToken = update[(String, UserId)](
    "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  val LogUserLogin = update[(String, UserId)](
    "UPDATE usr SET login_count = login_count + 1, last_login_at = NOW(), last_login_ip = ? WHERE id=?")

  val InsertUserPlaceholder = update[(String, String)](
    "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

  val RegisterUser = query[(String, PasswordAndSalt, String, String), UserId]( """
    UPDATE usr SET username = ?
      ,password = ?, password_salt = ?, password_changed_at = NOW()
      ,confirmation_token = NULL, confirmed_at = NOW()
      ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
    WHERE confirmation_token = ?
    RETURNING id""".sql)

  // ###################################################################################################################
  // Use Case

  private val ucrev_* = s"r.ident_id, r.rev, r.id, r.number, r.title"

  private val NextUseCaseNumber =
    "select coalesce(max(number),0)+1 from usecase_rev where id in (select latest_rev_id from usecase)"

  val InsertUseCaseIdent = queryNA[UseCaseIdentId]("INSERT INTO usecase DEFAULT VALUES RETURNING id")

  val InsertUseCaseRev = query[(UseCaseIdentId, Short, Short, String), UseCaseRevId](
    "INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,?,?,?) RETURNING id")

  val InsertUseCaseRev1WithAutoNumber = query[(UseCaseIdentId, String), (UseCaseRevId, Short)](
    s"INSERT INTO usecase_rev(ident_id, rev, number, title) VALUES(?,1,($NextUseCaseNumber),?) RETURNING id, number")

  val SelectLatestUseCaseRevId = query[UseCaseIdentId, UseCaseRevId](s"SELECT latest_rev_id FROM usecase WHERE id=?")

  val SelectUseCaseRev = query[UseCaseRevId, UseCaseRev](s"SELECT ${ucrev_*} FROM usecase_rev r WHERE r.id=?")

  val SelectLatestUseCaseRevByIdent = query[UseCaseIdentId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

  val SelectUseCaseSummaries = queryNA[UseCaseSummary]( s"""
    SELECT ident_id, number, title, to_iso8601_str(created_at)
    FROM usecase u, usecase_rev r
    WHERE r.id = latest_rev_id
    ORDER BY number """.sql)

  val UpdateUseCaseTitleDirect = update[(String, UseCaseRevId)]("UPDATE usecase_rev SET title=? WHERE id=?")

  // ###################################################################################################################
  // Text

  private val textrev_* = "tr.ident_id, tr.rev, tr.id, tr.text"

  val InsertTextIdent = query[(UseCaseIdentId, FieldKeyId), TextIdentId](
    "INSERT INTO text(uc_id, fk_id) VALUES(?,?) RETURNING id")

  val InsertTextRev = query[(TextIdentId, Short, TextWithNormalisedRefs), TextRevId](
    "INSERT INTO text_rev(ident_id, rev, text) VALUES(?,?,?) RETURNING id")

  // ###################################################################################################################
  // uc_field

  val LinkUcToText = update[(UseCaseRevId, TextRevId)]("INSERT INTO uc_field(uc_rev_id, text_rev_id) VALUES(?,?)")

  val LinkUcToStep = update[(UseCaseRevId, LabelStr, Option[TextRevId], Short, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES(?,?,?,?,?)")

  val CopyUcFieldsBetweenRevs = update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ?
    """.sql)

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

  val InsertFieldKey = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
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
