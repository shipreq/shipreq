package shipreq.webapp.server.logic

import java.time.Instant
import scalaz.{\/, ~>}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRecs
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
    final case class Pending(id: UserId, token: SecurityToken, tokenSentAt: Instant) extends UserRegistration
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
    final case class TokenExists(reg: UserRegistration.Complete, token: SecurityToken, tokenSentAt: Instant) extends PasswordResetState
  }

  final case class SaveProjectEventCmd(ord: EventOrd, event: ActiveEvent, hashes: HashRecs) {
    assert(hashes.nonEmpty, s"At least one hash is required: $this")
    assert(hashes.forall(_._2.nonEmpty), s"Empty hash set found: $this")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Base[F[_]] {
    def inDbTransaction[A](f: F[A]): F[A]

    /** @param level See java.sql.Connection */
    def inDbTransaction[A](level: Int, f: F[A]): F[A]
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

  trait SecurityTokenReadOnly[F[_]] {
    def getUserRegistrationTokenIssueDate(t: SecurityToken): F[Option[Instant]]
    def getResetPasswordTokenIssueDate   (t: SecurityToken): F[Option[Instant]]
  }

  object SecurityTokenReadOnly {
    def trans[F[_], G[_]](f: SecurityTokenReadOnly[F])(g: F ~> G): SecurityTokenReadOnly[G] =
      new SecurityTokenReadOnly[G] {
        override def getUserRegistrationTokenIssueDate(t: SecurityToken) = g(f.getUserRegistrationTokenIssueDate(t))
        override def getResetPasswordTokenIssueDate   (t: SecurityToken) = g(f.getResetPasswordTokenIssueDate   (t))
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForUserRegistration[F[_]] extends Base[F] with SecurityTokenReadOnly[F] {
    def getUserRegistration(e: EmailAddr): F[Option[UserRegistration]]

    /** Creates an unconfirmed user account. No username, no password until email confirmed. */
    def createUserPlaceholder(e: EmailAddr): F[SecurityToken]

    def updateUserRegistrationToken(id: UserId): F[SecurityToken]

    def completeUserRegistration(token     : SecurityToken,
                                 name      : PersonName,
                                 username  : Username,
                                 ps        : PasswordAndSalt,
                                 newsletter: Boolean): F[UserRegistrationResult]
  }

  trait ForPasswordReset[F[_]] extends Base[F] with SecurityTokenReadOnly[F] {
    def getPasswordResetState(u: Username \/ EmailAddr): F[Option[(EmailAddr, PasswordResetState)]]

    def createResetPasswordToken(id: UserId): F[SecurityToken]

    /** Updates the sent-count and sent-at attributes of an existing reset-password token. */
    def updateResetPasswordTokenOnReissue(id: UserId): F[Unit]

    /** This also clears the token */
    def updateUserPassword(token: SecurityToken, ps: PasswordAndSalt): F[Option[UserId]]
  }

  trait ForPublicSpa[F[_]] extends ForUserRegistration[F] with ForPasswordReset[F]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait SaveProjectEvent[F[_]] {
    def saveProjectEvent(id: ProjectId, cmd: SaveProjectEventCmd): F[Throwable \/ VerifiedEvent]
    def saveProjectEvents(id: ProjectId, cmds: Traversable[SaveProjectEventCmd]): F[Throwable \/ VerifiedEvent.Seq]
  }

  trait ForHomeSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def createEmptyProject(id: UserId, initEvents: Int): F[ProjectId]
    def getAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  trait ForProjectSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def projectSpaInitPage(id: ProjectId): F[Project.Name]
    def projectSpaInitApp(id: ProjectId): F[ProjectSpaInitApp]
    def getProjectEvents(id: ProjectId, f: EventFilter): F[VerifiedEvent.Seq]

    final def getProjectEvents(id: ProjectId): F[VerifiedEvent.Seq] = getProjectEvents(id, EventFilter.IncludeAll)
    final def getAllProjectEvents(id: ProjectId): F[VerifiedEvent.Seq] = getProjectEvents(id, EventFilter.IncludeAll)
  }

  sealed trait EventFilter
  object EventFilter {
    case object IncludeAll extends EventFilter
    final case class ExcludeUpTo(ord: EventOrd) extends EventFilter

    def given(alreadyGot: Option[EventOrd.Latest]): EventFilter =
      alreadyGot match {
        case Some(ord) => ExcludeUpTo(ord)
        case None      => IncludeAll
      }
  }

  final case class ProjectSpaInitApp(createdAt    : Instant,
                                     latestOrd    : Option[EventOrd.Latest],
                                     lastUpdatedAt: Option[Instant]) {
    val lastUpdatedOrCreatedAt = lastUpdatedAt.getOrElse(createdAt)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForOps[F[_]] {
    val now       : F[Instant]
    val userStats : F[ForOps.UserStats]
    val tableStats: F[List[ForOps.TableStat]]
    val dbSize    : F[Long]
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
        override val now        = t(f.now)
        override val userStats  = t(f.userStats)
        override val tableStats = t(f.tableStats)
        override val dbSize     = t(f.dbSize)
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Algebra[F[_]]
    extends ForPublicSpa[F]
       with ForHomeSpa[F]
       with ForProjectSpa[F]
}
