package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty.NonEmptySet
import java.time.Instant
import scalaz.~>
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.user._

/**
  * Naming conventions:
  *
  * =======
  * SELECT:
  * =======
  *
  * Prefixes:
  * - `get`    = SELECT [0,1]
  * - `getAll` = SELECT [0,n]
  * - `need`   = SELECT [1,1]
  *
  * Suffixes:
  * - `for<Criteria>`
  *
  * =======
  * INSERT:
  * =======
  *
  * - `save`   = A -> (Error \/)? Unit
  * - `create` = A -> (Error \/)? B
  *
  * =======
  * UPDATE:
  * =======
  *
  * - `update` = A -> _
  */
object DB {

  sealed trait UserRegistration {
    val id: UserId
  }

  object UserRegistration {
    final case class Pending(id: UserId, token: VerificationToken, tokenSentAt: Instant) extends UserRegistration
    final case class Complete(id: UserId, confirmationAt: Instant) extends UserRegistration
  }

  sealed trait UserRegistrationResult
  object UserRegistrationResult {
    final case class Success(userId: UserId) extends UserRegistrationResult
    case object TokenNotFound extends UserRegistrationResult
    case object UsernameTaken extends UserRegistrationResult
  }

  sealed trait PasswordResetState
  object PasswordResetState {
    final case class UserRegistrationPending(reg: UserRegistration.Pending) extends PasswordResetState
    final case class NoToken(reg: UserRegistration.Complete) extends PasswordResetState
    final case class TokenExists(reg: UserRegistration.Complete, token: VerificationToken, tokenSentAt: Instant) extends PasswordResetState
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Base[F[_]] {

    /** Note: This is translated immediately out of F to explicitly clarify the fact that it cannot be composed with
      * other Fs to form a transaction. This is its own isolated transaction; to attempt otherwise would result in a
      * "Cannot change transaction isolation level in the middle of a transaction" error from PostgreSQL.
      *
      * @param level See java.sql.Connection
      */
    def withTransactionLevel[D[_], A](runDB: F ~> D, level: Int)(f: F[A]): D[A]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForSecurity[F[_]] {
    def getUserAndPasswordByEmail(email: EmailAddr): F[Option[(User, PasswordAndSalt)]]
    def getUserAndPasswordByUsername(username: Username): F[Option[(User, PasswordAndSalt)]]
    def logLoginSuccess(id: UserId, ip: Option[IP]): F[Unit]
    def getProjectOwner(id: ProjectId): F[Option[UserId]]

    final def getUserAndPassword(usernameOrEmail: String): F[Option[(User, PasswordAndSalt)]] =
      if (EmailAddr.isEmailAddr(usernameOrEmail))
        getUserAndPasswordByEmail(EmailAddr(usernameOrEmail))
      else
        getUserAndPasswordByUsername(Username(usernameOrEmail))

    final def getUserAndPassword(id: Username \/ EmailAddr): F[Option[(User, PasswordAndSalt)]] =
      id.fold(getUserAndPasswordByUsername, getUserAndPasswordByEmail)
  }

  object ForSecurity {
    def trans[F[_], G[_]](f: ForSecurity[F])(t: F ~> G): ForSecurity[G] =
      new ForSecurity[G] {
        override def getUserAndPasswordByEmail(e: EmailAddr)     = t(f.getUserAndPasswordByEmail(e))
        override def getUserAndPasswordByUsername(u: Username)   = t(f.getUserAndPasswordByUsername(u))
        override def logLoginSuccess(id: UserId, ip: Option[IP]) = t(f.logLoginSuccess(id, ip))
        override def getProjectOwner(id: ProjectId)              = t(f.getProjectOwner(id))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait VerificationTokenReadOnly[F[_]] {
    def getUserRegistrationTokenIssueDate(t: VerificationToken): F[Option[Instant]]
    def getResetPasswordTokenIssueDate   (t: VerificationToken): F[Option[Instant]]
  }

  object VerificationTokenReadOnly {
    def trans[F[_], G[_]](f: VerificationTokenReadOnly[F])(g: F ~> G): VerificationTokenReadOnly[G] =
      new VerificationTokenReadOnly[G] {
        override def getUserRegistrationTokenIssueDate(t: VerificationToken) = g(f.getUserRegistrationTokenIssueDate(t))
        override def getResetPasswordTokenIssueDate   (t: VerificationToken) = g(f.getResetPasswordTokenIssueDate   (t))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForUserRegistration[F[_]] extends Base[F] with VerificationTokenReadOnly[F] {
    def getUserRegistration(e: EmailAddr): F[Option[UserRegistration]]

    /** Creates an unconfirmed user account. No username, no password until email confirmed. */
    def createUserPlaceholder(e: EmailAddr): F[VerificationToken]

    def updateUserRegistrationToken(id: UserId): F[VerificationToken]

    def completeUserRegistration(token     : VerificationToken,
                                 name      : PersonName,
                                 username  : Username,
                                 ps        : PasswordAndSalt,
                                 newsletter: Boolean): F[UserRegistrationResult]
  }

  trait ForPasswordReset[F[_]] extends Base[F] with VerificationTokenReadOnly[F] {
    def getPasswordResetState(u: Username \/ EmailAddr): F[Option[(EmailAddr, PasswordResetState)]]

    def createResetPasswordToken(id: UserId): F[VerificationToken]

    /** Updates the sent-count and sent-at attributes of an existing reset-password token. */
    def updateResetPasswordTokenOnReissue(id: UserId): F[Unit]

    /** This also clears the token */
    def updateUserPassword(token: VerificationToken, ps: PasswordAndSalt): F[Option[UserId]]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait GetProjectMetaData[F[_]] {
    def getProjectMetaData(id: ProjectId): F[Option[ProjectMetaData]]
  }

  trait GetProjectEvents[F[_]] {
    def getProjectEvents(id: ProjectId, f: EventFilter): F[ReadProjectEventError \/ VerifiedEvent.Seq]

    final def getProjectEvents(id: ProjectId): F[ReadProjectEventError \/ VerifiedEvent.Seq] =
      getProjectEvents(id, EventFilter.IncludeAll)

    final def getAllProjectEvents(id: ProjectId): F[ReadProjectEventError \/ VerifiedEvent.Seq] =
      getProjectEvents(id, EventFilter.IncludeAll)
  }

  sealed trait EventFilter
  object EventFilter {
    case object IncludeAll extends EventFilter
    final case class ExcludeUpTo(ord: EventOrd) extends EventFilter
    final case class Set(ords: NonEmptySet[EventOrd]) extends EventFilter

    def after(alreadyGot: Option[EventOrd.Latest]): EventFilter =
      alreadyGot match {
        case Some(ord) => ExcludeUpTo(ord)
        case None      => IncludeAll
      }
  }

  sealed trait ReadProjectEventError
  object ReadProjectEventError {
    final case class DecodeFailure(ord: EventOrd, logMsg: String) extends ReadProjectEventError
  }

  trait SaveProjectEvent[F[_]] {
    def saveProjectEvent(id     : ProjectId,
                         ord    : EventOrd,
                         event  : ActiveEvent,
                         project: Project,
                         userId : UserId): F[SaveProjectEventError \/ VerifiedEvent]
  }

  sealed trait SaveProjectEventError
  object SaveProjectEventError {
    case object OrdInUse extends SaveProjectEventError
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForPublicSpa[F[_]] extends ForUserRegistration[F] with ForPasswordReset[F]

  trait ForHomeSpa[F[_]] extends Base[F] with GetProjectMetaData[F] {
    def createProject(id: UserId, initEvents: Vector[ActiveEvent], project: Project): F[ProjectId]
    def getAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  trait ForProjectSpa[F[_]]
      extends Base[F]
         with GetProjectMetaData[F]
         with GetProjectEvents[F]
         with SaveProjectEvent[F] {
    def projectSpaInitPage(id: ProjectId): F[Project.Name]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForOps[F[_]] extends GetProjectEvents[F] {
    val now       : F[Instant]
    val userStats : F[ForOps.UserStats]
    val tableStats: F[List[ForOps.TableStat]]
    val dbSize    : F[Long]

    def getUserId(user: Username \/ EmailAddr): F[Option[UserId]]
    def createProject(userId: UserId, events: VerifiedEvent.Seq, project: Project): F[ProjectId]
  }

  object ForOps {

    final case class UserStats(registered: Long, total: Long) {
      def pendingRegistration: Long =
        total - registered
    }

    final case class TableStat(name: String, tableSize: Long, indexesSize: Long) {
      def totalSize: Long =
        tableSize + indexesSize
    }

    def trans[F[_], G[_]](f: ForOps[F])(t: F ~> G): ForOps[G] =
      new ForOps[G] {
        override val now                                                        = t(f.now)
        override val userStats                                                  = t(f.userStats)
        override val tableStats                                                 = t(f.tableStats)
        override val dbSize                                                     = t(f.dbSize)
        override def getProjectEvents(a: ProjectId, b: EventFilter)             = t(f.getProjectEvents(a, b))
        override def getUserId(a: Username \/ EmailAddr)                        = t(f.getUserId(a))
        override def createProject(a: UserId, b: VerifiedEvent.Seq, c: Project) = t(f.createProject(a, b, c))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Algebra[F[_]]
    extends ForPublicSpa[F]
       with ForHomeSpa[F]
       with ForProjectSpa[F]
}
