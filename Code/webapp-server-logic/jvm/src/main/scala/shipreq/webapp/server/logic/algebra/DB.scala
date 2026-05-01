package shipreq.webapp.server.logic.algebra

import cats.syntax.all._
import cats.{Applicative, Monad, ~>}
import java.sql.Connection
import java.time.Instant
import shipreq.webapp.base.data._
import shipreq.webapp.member.global._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.{ActiveEvent, Event, EventOrd, VerifiedEvent}
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.util.Obfuscators

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

  type EffectTC[F[_]] = Applicative[F] with Monad[F]

  trait Effect[F[_]] {
    implicit protected def F: EffectTC[F]

    final protected def flatMapRight[E, D >: E, A, B](f: F[E \/ A])(g: A => F[D \/ B]): F[D \/ B] =
      F.flatMap(f) {
        case \/-(a) => g(a)
        case -\/(e) => F.pure(-\/(e))
      }

    final protected def reduceRight[E, D >: E, A](f: F[E \/ Unit], g: F[D \/ A]): F[D \/ A] =
      F.flatMap(f) { e => if (e.isLeft) F.pure(e.castLeft()) else g }

    final protected def reduceRight[E, D >: E, A](o: Option[F[E \/ Unit]], g: F[D \/ A]): F[D \/ A] =
      if (o.isEmpty) g else reduceRight(o.get, g)

    final protected def append[E, D <: E, A](f: F[E \/ A], g: F[D \/ Unit]): F[E \/ A] =
      F.flatMap(f) { ea =>
        if (ea.isLeft)
          F.pure(ea.castLeft())
        else
          F.map(g) { du => if (du.isLeft) du.castLeft() else ea }
      }

    final protected def append[E, D <: E, A](f: F[E \/ A], o: Option[F[D \/ Unit]]): F[E \/ A] =
      if (o.isEmpty) f else append(f, o.get)

    final protected def throwOnLeft[E, A](f: F[E \/ A]): F[A] =
      F.map(f) {
        case \/-(a) => a
        case -\/(e) => throw new RuntimeException("DB.throwOnLeft: " + e)
      }

    final protected def throwOnLeft_[E](o: Option[F[E \/ Unit]]): F[Unit] =
      o match {
        case None    => F.unit
        case Some(f) => throwOnLeft(f)
      }
  }

  trait Base[F[_]] extends Effect[F] {

    /** Note: This is translated immediately out of F to explicitly clarify the fact that it cannot be composed with
      * other Fs to form a transaction. This is its own isolated transaction; to attempt otherwise would result in a
      * "Cannot change transaction isolation level in the middle of a transaction" error from PostgreSQL.
      *
      * @param level See java.sql.Connection
      */
    def withTransactionLevel[G[_], A](runDB: F ~> G, level: Int)(f: F[A]): G[A]

    @inline final def inStrictTxn[G[_], A](runDB: F ~> G)(f: F[A]): G[A] =
      withTransactionLevel(runDB, Connection.TRANSACTION_SERIALIZABLE)(f)

    def logGlobalEvent(e: GlobalEvent): F[Unit]

    def logGlobalEventIf(cond: Boolean)(e: => GlobalEvent): F[Unit]

    final def logGlobalEventOnRight[A](e: Any \/ A)(f: A => GlobalEvent): F[Unit] =
      logGlobalEventIf(e.isRight)(f(e.asInstanceOf[\/-[A]].value))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForSecurity[F[_]] {
    def getUserAndPasswordByEmail(email: EmailAddr): F[Option[(User, PasswordAndSalt)]]
    def getUserAndPasswordByUsername(username: Username): F[Option[(User, PasswordAndSalt)]]
    def logLoginSuccess(id: UserId, ip: Option[IP]): F[Unit]
    def getProjectAccess(pid: ProjectId, uid: UserId): F[Option[ProjectPerm]]

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
        override def getProjectAccess(a: ProjectId, b: UserId)   = t(f.getProjectAccess(a, b))
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

  trait GetUserId[F[_]] {
    def getUserId(e: Username \/ EmailAddr): F[Option[UserId]]
  }

  trait ForUserRegistration[F[_]] extends Base[F] with VerificationTokenReadOnly[F] with GetUserId[F] {

    def getUserRegistration(e: EmailAddr): F[Option[UserRegistration]]

    /** Creates an unconfirmed user account. No username, no password until email confirmed. */
    def createUserPlaceholder(e: EmailAddr): F[VerificationToken]

    def updateUserRegistrationToken(id: UserId): F[VerificationToken]

    def completeUserRegistration(token     : VerificationToken,
                                 name      : PersonName,
                                 username  : Username,
                                 ps        : PasswordAndSalt,
                                 newsletter: Boolean,
                                 encKey    : UserEncryptionKey): F[UserRegistrationResult]
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
    def getProjectMetaData(pid: ProjectId, uid: UserId): F[Option[ProjectMetaData]]
  }

  trait GetProjectEvents[F[_]] {
    def getProjectEvents(id: ProjectId, f: EventFilter): F[ReadProjectEventError \/ VerifiedEvent.Seq]

    final def getProjectEvents(id: ProjectId): F[ReadProjectEventError \/ VerifiedEvent.Seq] =
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

  trait OnSaveProjectEvent[F[_]] extends Effect[F] {
    protected def updateProjectAccess(id    : ProjectId,
                                      remove: Set[UserId],
                                      add   : Map[UserId, ProjectPerm]): F[SaveProjectEventError.OnAccess \/ Unit]

    protected def updateProjectName(id: ProjectId, name: Project.Name): F[Unit]

    final protected def onSaveProjectEvent(pid: ProjectId, event: Event): Option[F[SaveProjectEventError \/ Unit]] = {
      type Out = SaveProjectEventError \/ Unit

      @inline implicit def cast(f: F[SaveProjectEventError.OnAccess \/ Unit]): F[Out] =
        f.asInstanceOf[F[Out]]

      @inline implicit def Unit(f: F[Unit]): F[Out] =
        F.map(f)(_ => \/-(()))

      event match {

        case Event.AccessUpdate(m) =>
          val remove = m.keysIterator.map(Obfuscators.userId.deobfuscateOrThrow).toSet
          val add = m.iterator.flatMap {
              case (u, Some(p)) => (Obfuscators.userId.deobfuscateOrThrow(u), p) :: Nil
              case _            => Nil
            }.toMap
          Some(updateProjectAccess(pid, remove, add))

        case e: Event.ProjectNameSet =>
          Some(updateProjectName(pid, e.name))

        case _ =>
          None
      }
    }

    final protected def onSaveProjectEvents(pid: ProjectId, events: IterableOnce[Event]): Option[F[SaveProjectEventError \/ Unit]] = {
      val it = events.iterator.flatMap(onSaveProjectEvent(pid, _))
      Option.when(it.hasNext)(it.reduce(reduceRight(_, _)))
    }
  }

  trait SaveProjectEvent[F[_]] extends OnSaveProjectEvent[F] {

    protected def _saveProjectEvent(id     : ProjectId,
                                    ord    : EventOrd,
                                    event  : ActiveEvent,
                                    project: Project,
                                    userId : UserId): F[SaveProjectEventError \/ VerifiedEvent]

    final def saveProjectEvent(id     : ProjectId,
                               ord    : EventOrd,
                               event  : ActiveEvent,
                               project: Project,
                               userId : UserId): F[SaveProjectEventError \/ VerifiedEvent] =
      reduceRight(
        onSaveProjectEvent(id, event),
        _saveProjectEvent(id, ord, event, project, userId))
  }

  sealed trait SaveProjectEventError
  object SaveProjectEventError {
    case object OrdInUse extends SaveProjectEventError

    sealed trait OnAccess extends SaveProjectEventError
    object OnAccess {
      case object CantRemoveLastAdmin extends OnAccess
      implicit def univEq: UnivEq[OnAccess] = UnivEq.derive
    }

    implicit def univEq: UnivEq[SaveProjectEventError] = UnivEq.derive
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForPublicSpa[F[_]]
      extends Base[F]
         with ForUserRegistration[F]
         with ForPasswordReset[F]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForHomeSpa[F[_]]
      extends Base[F]
        with GetProjectMetaData[F]
        with OnSaveProjectEvent[F] {

    protected def _createProject(userId    : UserId,
                                 initEvents: Vector[ActiveEvent],
                                 project   : Project,
                                 encKey    : ProjectEncryptionKey): F[ProjectId]

    final def createProject(userId    : UserId,
                            initEvents: Vector[ActiveEvent],
                            project   : Project,
                            encKey    : ProjectEncryptionKey): F[ProjectId] =
      for {
        pid <- _createProject(userId, initEvents, project, encKey)
        _   <- throwOnLeft_(onSaveProjectEvents(pid, initEvents))
      } yield pid

    def getAllProjectMetaDataForUser(id: UserId): F[List[ProjectMetaData]]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForProjectSpa[F[_]]
      extends Base[F]
         with GetUserId[F]
         with GetProjectMetaData[F]
         with GetProjectEvents[F]
         with SaveProjectEvent[F] {

    def projectSpaInitPage(id: ProjectId, uid: UserId): F[Option[ProjectSpaInitPage]]

    def getProjectRolodex(id: ProjectId, exclude: UserId): F[Rolodex]

    /** @return Either user ids for all provided usernames, or a set of invalid usernames. */
    final def getUserIdsByUsername(usernames: Set[Username]): F[NonEmptySet[Username] \/ Map[Username, UserId]] =
      if (usernames.isEmpty)
        F.pure(\/-(Map.empty))
      else
        getUserIdsByUsernameNE(NonEmptySet force usernames)

    /** @return Either user ids for all provided usernames, or a set of invalid usernames. */
    def getUserIdsByUsernameNE(usernames: NonEmptySet[Username]): F[NonEmptySet[Username] \/ Map[Username, UserId]]

    /** @return Either user ids for all provided usernames, or a set of invalid usernames. */
    final def getUsernamesByUserId(userIds: Set[UserId]): F[NonEmptySet[UserId] \/ Map[UserId, Username]] =
      if (userIds.isEmpty)
        F.pure(\/-(Map.empty))
      else
        getUsernamesByUserIdNE(NonEmptySet force userIds)

    /** @return Either usernames for all provided user ids, or a set of invalid user ids. */
    def getUsernamesByUserIdNE(userIds: NonEmptySet[UserId]): F[NonEmptySet[UserId] \/ Map[UserId, Username]]

    final def needUsernamesByUserId(userIds: Set[UserId]): F[Map[UserId, Username]] =
      if (userIds.isEmpty)
        F.pure(Map.empty)
      else
        needUsernamesByUserIdNE(NonEmptySet force userIds)

    final def needUsernamesByUserIdNE(userIds: NonEmptySet[UserId]): F[Map[UserId, Username]] =
      F.map(getUsernamesByUserIdNE(userIds)) {
        case \/-(m) => m
        case -\/(e) => throw new RuntimeException("Invalid user ids specified to needUsernamesByUserIdNE: " + e)
      }
  }

  final case class ProjectSpaInitPage(creatorId : UserId,
                                      name      : Project.Name,
                                      projectKey: ProjectEncryptionKey,
                                      userKey   : UserEncryptionKey)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForOps[F[_]] extends GetProjectEvents[F] with OnSaveProjectEvent[F] { self =>
    val now       : F[Instant]
    val userStats : F[ForOps.UserStats]
    val tableStats: F[List[ForOps.TableStat]]
    val dbSize    : F[Long]

    def getUserId(user: Username \/ EmailAddr): F[Option[UserId]]

    protected def _importProject(userId : UserId,
                                 events : VerifiedEvent.Seq,
                                 project: Project,
                                 encKey : ProjectEncryptionKey): F[ProjectId]

    final def importProject(userId : UserId,
                            events : VerifiedEvent.Seq,
                            project: Project,
                            encKey : ProjectEncryptionKey): F[ProjectId] =
      for {
        pid <- _importProject(userId, events, project, encKey)
        _   <- throwOnLeft_(onSaveProjectEvents(pid, events.iterator.map(_.event)))
      } yield pid

    def trans[G[_]](t: F ~> G)(implicit G: EffectTC[G]): ForOps[G] =
      new ForOps[G] {
        override val F = G
        override val now = t(self.now)
        override val userStats = t(self.userStats)
        override val tableStats = t(self.tableStats)
        override val dbSize = t(self.dbSize)
        override def getProjectEvents(a: ProjectId, b: EventFilter) = t(self.getProjectEvents(a, b))
        override def getUserId(a: Username \/ EmailAddr) = t(self.getUserId(a))
        override def _importProject(a: UserId, b: VerifiedEvent.Seq, c: Project, d: ProjectEncryptionKey) = t(self._importProject(a, b, c, d))
        override def updateProjectAccess(a: ProjectId, b: Set[UserId], c: Map[UserId,ProjectPerm]) = t(self.updateProjectAccess(a, b, c))
        override def updateProjectName(a: ProjectId, b: Project.Name): G[Unit] = t(self.updateProjectName(a, b))
      }
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
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait Algebra[F[_]]
    extends ForPublicSpa[F]
       with ForHomeSpa[F]
       with ForProjectSpa[F]
}
