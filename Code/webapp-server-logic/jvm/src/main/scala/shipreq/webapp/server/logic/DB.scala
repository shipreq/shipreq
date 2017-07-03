package shipreq.webapp.server.logic

import java.time.Instant
import scala.collection.immutable.SortedMap
import scalaz.\/
import shipreq.webapp.base.data.{ProjectMetaData, SecurityToken}
import shipreq.webapp.base.event.{ActiveEvent, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Base[F[_]] {
    def inDbTransaction[A](f: F[A]): F[A]

    /** @param level See java.sql.Connection */
    def inDbTransaction[A](level: Int, f: F[A]): F[A]
  }

  trait SaveProjectEvent[F[_]] {
    def saveProjectEvent(id    : ProjectId)
                        (ord   : EventOrd,
                         event : ActiveEvent,
                         hashes: HashRec.Collection): F[Option[Throwable]]
  }

  trait ForUserRegistration[F[_]] extends Base[F] {
    def getUserRegistration(e: EmailAddr): F[Option[UserRegistration]]

    /** Creates an unconfirmed user account. No username, no password until email confirmed. */
    def createUserPlaceholder(e: EmailAddr): F[SecurityToken]

    def updateUserRegistrationToken(id: UserId): F[SecurityToken]

    def getUserRegistrationTokenIssueDate(t: SecurityToken): F[Option[Instant]]

    def completeUserRegistration(token     : SecurityToken,
                                 name      : PersonName,
                                 username  : Username,
                                 ps        : PasswordAndSalt,
                                 newsletter: Boolean,
                                 ip        : Option[IP]): F[UserRegistrationResult]
  }

  trait ForPasswordReset[F[_]] extends Base[F] {
    def getPasswordResetState(u: Username \/ EmailAddr): F[Option[(EmailAddr, PasswordResetState)]]

    def getResetPasswordTokenIssueDate(t: SecurityToken): F[Option[Instant]]

    def createResetPasswordToken(id: UserId): F[SecurityToken]

    /** Updates the sent-count and sent-at attributes of an existing reset-password token. */
    def updateResetPasswordTokenOnReissue(id: UserId): F[Unit]

    /** This also clears the token */
    def updateUserPassword(token: SecurityToken, ps: PasswordAndSalt): F[Unit]
  }

  trait ForPublicSpa[F[_]] extends ForUserRegistration[F] with ForPasswordReset[F]

  trait ForHomeSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def createEmptyProject          (id: UserId): F[ProjectId]
    def getAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  trait ForProjectSpa[F[_]] extends Base[F] with SaveProjectEvent[F] {
    def getProjectHeader   (id: ProjectId): F[Option[ProjectHeader]]
    def getProjectMetaData (id: ProjectId): F[Option[ProjectMetaData]]
    def getAllProjectEvents(id: ProjectId): F[ProjectEvents]
  }
  type ProjectEvents = SortedMap[EventOrd, VerifiedEvent]

  trait Algebra[F[_]]
    extends ForPublicSpa[F]
       with ForHomeSpa[F]
       with ForProjectSpa[F]
}
