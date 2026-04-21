package shipreq.webapp.server.logic.test

import cats.{Eval, ~>}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.project.data.{Live => _, _}
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes.projectCreatorFromUserId
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.laws.DbLaws
import shipreq.webapp.server.logic.util.Obfuscators

object MockDb {
  final case class UserEntry(id           : UserId,
                             username     : Username,
                             emailAddr    : EmailAddr,
                             ps           : PasswordAndSalt,
                             encKey       : UserEncryptionKey,
                             createdAt    : Instant,
                             resetPassword: Option[(VerificationToken, Instant)] = None) {

    def idP: UserId.Public =
      Obfuscators.userId.obfuscate(id)

    def pubids: List[Username \/ EmailAddr] =
      -\/(username) :: \/-(emailAddr) :: Nil

    def toUser: User =
      User(id, username)

    def toUserAndPassword: (User, PasswordAndSalt) =
      (toUser, ps)

    val token: Security.SessionToken[Instant] =
      Security.SessionToken.anonymous().login(toUser).copy(expiry = Instant.now())
  }
  object UserEntry {
    implicit def univEq: UnivEq[UserEntry] = UnivEq.derive
  }

  final case class ProjectEntry(projectId    : ProjectId,
                                creatorId    : UserId,
                                encKey       : ProjectEncryptionKey,
                                initEvents   : Int,
                                events       : VerifiedEvent.Seq,
                                createdAt    : Instant,
                                accessedAt   : Instant,
                                lastUpdatedAt: Option[Instant]) {

    lazy val project: Project =
      ApplyEvent.trusted(events)(Project.init(creatorId)).getOrThrow()

    def projectMetaData(perm: ProjectPerm): ProjectMetaData =
      ProjectMetaData.fromProject(project)(
        id            = Obfuscators.projectId.obfuscate(projectId),
        userId        = Obfuscated(""),
        eventsInit    = initEvents,
        eventsTotal   = events.size,
        createdAt     = createdAt,
        accessedAt    = accessedAt,
        lastUpdatedAt = lastUpdatedAt,
      ).copy(perm = Some(perm))

    def projectLoad: VerifiedEvent.Seq =
      events
  }

  final case class ProjectAccessEntry(pid: ProjectId, uid: UserId, perm: ProjectPerm)

  def withLiveClock(): MockDb =
    new MockDb(Eval.always(Instant.now()))

  private[MockDb] def implicitInstance = implicitly[DB.EffectTC[Eval]]
}

// =====================================================================================================================

final class MockDb(_now: Eval[Instant]) extends DB.Algebra[Eval] with DB.ForSecurity[Eval] with DB.ForOps[Eval] {

  override implicit protected val F = MockDb.implicitInstance

  override val now: Eval[Instant] =
    _now

  override def getUserAndPasswordByEmail(email: EmailAddr) = Eval.always[Option[(User, PasswordAndSalt)]] {
    getUser(\/-(email)).map(_.toUserAndPassword)
  }

  override def getUserAndPasswordByUsername(username: Username) = Eval.always[Option[(User, PasswordAndSalt)]] {
    getUser(-\/(username)).map(_.toUserAndPassword)
  }

  var usrLoginLog = Vector.empty[(UserId, Option[IP])]
  override def logLoginSuccess(id: UserId, ip: Option[IP]) = Eval.always[Unit] {
    usrLoginLog :+= ((id, ip))
  }

  var prevTokenId = 0L
  private def nextToken(): VerificationToken = {
    prevTokenId += 1
    prevToken()
  }

  def prevToken(): VerificationToken = {
    assert(prevTokenId > 0)
    VerificationToken(s"[token-$prevTokenId]")
  }

  def assertTokensIssued(expect: Int): Unit =
    assertEq("assertTokensIssued", prevTokenId, expect)

  def assertIssuesTokens[A](expect: Int)(a: => A): A =
    assertDifference("assertIssuesTokens", prevTokenId)(expect)(a)

  var userPlaceholders = Map.empty[EmailAddr, DB.UserRegistration.Pending]
  override def createUserPlaceholder(e: EmailAddr) =
    now.map { n =>
      assert(!userPlaceholders.contains(e))
      val t = nextToken()
      userPlaceholders += e -> DB.UserRegistration.Pending(UserId(prevTokenId), t, n)
      t
    }

  override def getUserRegistration(e: EmailAddr) = Eval.always[Option[DB.UserRegistration]] {
    userPlaceholders.get(e) orElse
      getUser(\/-(e)).map(x => DB.UserRegistration.Complete(x.id, x.createdAt))
  }

  override def updateUserRegistrationToken(id: UserId) =
    now.map { n =>
      val (e, r) = userPlaceholders.iterator.find(_._2.id ==* id) getOrElse sys.error("User not found")
      val t = nextToken()
      userPlaceholders += e -> r.copy(token = t, tokenSentAt = n)
      t
    }

  def getPendingUserRegistration(t: VerificationToken): Option[(EmailAddr, DB.UserRegistration.Pending)] =
    userPlaceholders.iterator.collect {
      case (ea, p: DB.UserRegistration.Pending) if p.token ==* t => (ea, p)
    }.nextOption()

  override def getUserRegistrationTokenIssueDate(t: VerificationToken) = Eval.always[Option[Instant]] {
    getPendingUserRegistration(t).map(_._2.tokenSentAt)
  }

  var users = List.empty[MockDb.UserEntry]

  def addUser(e: MockDb.UserEntry): Unit =
    addUsers(e)

  def addUsers(es: MockDb.UserEntry*): Unit = {
    users :::= es.toList
    for (e <- es)
    if (e.id.value > prevTokenId)
      prevTokenId = e.id.value
  }

  def newUserEntry(): MockDb.UserEntry = {
    nextToken()
    val id = UserId(prevTokenId)
    val x = "u" + id.value
    val e = MockDb.UserEntry(
      id        = id,
      username  = Username(x),
      emailAddr = EmailAddr(x + "@inmem.com"),
      ps        = PasswordAndSalt(PasswordHash(x), Salt(x)),
      encKey    = UserEncryptionKey(BinaryData.fromStringBytes(x + "-user-key")),
      createdAt = now.value,
    )
    users ::= e
    e
  }

  def newUser(): User = {
    val e = newUserEntry()
    User(e.id, e.username)
  }

  def newUserId(): UserId =
    newUser().id

  def needUser(id: UserId): MockDb.UserEntry =
    getUser(id).getOrThrow(s"User #${id.value} not found")

  def getUser(id: UserId): Option[MockDb.UserEntry] =
    users.find(_.id ==* id)

  def getUser(u: Username \/ EmailAddr): Option[MockDb.UserEntry] =
    users.find(e => u.fold(_ ==* e.username, _ ==* e.emailAddr))

  def getUserOrPlaceholder(u: Username \/ EmailAddr): Option[(EmailAddr, DB.UserRegistration.Pending) \/ MockDb.UserEntry] =
    getUser(u).map(\/-(_)) orElse u.fold(_ => None, e => userPlaceholders.get(e).map(r => -\/((e, r))))

  def updateUser(w: MockDb.UserEntry => Boolean, f: MockDb.UserEntry => MockDb.UserEntry): Unit =
    users = users.map(u => if (w(u)) f(u) else u)

  def updateUser(id: UserId, f: MockDb.UserEntry => MockDb.UserEntry): Option[MockDb.UserEntry] = {
    var r = Option.empty[MockDb.UserEntry]
    updateUser(_.id ==* id, u => {
      val u2 = f(u)
      r = Some(u2)
      u2
    })
    r
  }

  override def getUserIdsByUsernameNE(usernames: NonEmptySet[Username]) = Eval.always[NonEmptySet[Username] \/ Map[Username, UserId]] {
    var ko = Set.empty[Username]
    var ok = Map.empty[Username, UserId]
    for (u <- usernames)
      this.users.find(_.username ==* u) match {
        case Some(e) => ok += ((u, e.id))
        case None    => ko += u
      }
    NonEmptySet.option(ko).toLeft(ok)
  }

  override def getUsernamesByUserIdNE(ids: NonEmptySet[UserId]) = Eval.always[NonEmptySet[UserId] \/ Map[UserId, Username]] {
    var ko = Set.empty[UserId]
    var ok = Map.empty[UserId, Username]
    for (id <- ids)
      this.users.find(_.id ==* id) match {
        case Some(e) => ok += ((id, e.username))
        case None    => ko += id
      }
    NonEmptySet.option(ko).toLeft(ok)
  }

  override def completeUserRegistration(token     : VerificationToken,
                                        name      : PersonName,
                                        username  : Username,
                                        ps        : PasswordAndSalt,
                                        newsletter: Boolean,
                                        key       : UserEncryptionKey) =
    now.map { n =>
      (getPendingUserRegistration(token), getUser(-\/(username))) match {
        case (None, _)          => DB.UserRegistrationResult.TokenNotFound
        case (Some(_), Some(_)) => DB.UserRegistrationResult.UsernameTaken
        case (Some((ea, reg)), None) =>
          userPlaceholders = userPlaceholders - ea
          users ::= MockDb.UserEntry(reg.id, username, ea, ps, key, n)
          DB.UserRegistrationResult.Success(reg.id)
      }
    }

  override def getPasswordResetState(u: Username \/ EmailAddr) = Eval.always[Option[(EmailAddr, DB.PasswordResetState)]] {
    getUserOrPlaceholder(u) map {
      case \/-(e) =>
        val u = DB.UserRegistration.Complete(e.id, e.createdAt)
        val s = e.resetPassword match {
          case Some((t, i)) => DB.PasswordResetState.TokenExists(u, t, i)
          case None         => DB.PasswordResetState.NoToken(u)
        }
        (e.emailAddr, s)
      case -\/((e, p)) =>
        (e, DB.PasswordResetState.UserRegistrationPending(p))
    }
  }

  override def getResetPasswordTokenIssueDate(t: VerificationToken) = Eval.always[Option[Instant]] {
    users.collectFirst {
      case MockDb.UserEntry(_, _, _, _, _, _, Some((t2, i))) if t ==* t2 => i
    }
  }

  override def createResetPasswordToken(id: UserId) =
    now.map { n =>
      val t = nextToken()
      updateUser(id, _.copy(resetPassword = Some((t, n))))
      t
    }

  override def updateResetPasswordTokenOnReissue(id: UserId) =
    now.map { n =>
      updateUser(id, u => u.copy(resetPassword = u.resetPassword.map(x => (x._1, n))))
      ()
    }

  override def updateUserPassword(token: VerificationToken, ps: PasswordAndSalt) = Eval.always[Option[UserId]] {
    users.find(_.resetPassword.exists(_._1 ==* token)).map { u =>
      updateUser(_.id ==* u.id, _.copy(ps = ps, resetPassword = None))
      u.id
    }
  }

  private var projects: IMap[ProjectId, MockDb.ProjectEntry] = IMap.empty(_.projectId)
  private var projectAccess = List.empty[MockDb.ProjectAccessEntry]

  private def addProjectAccess(pid: ProjectId, uid: UserId, perm: ProjectPerm): Unit = {
    projectAccess.find(e => e.pid ==* pid && e.uid ==* uid) match {
      case None    => projectAccess ::= MockDb.ProjectAccessEntry(pid, uid, perm)
      case Some(e) => throw new RuntimeException("Duplicate entry in project_access: " + e)
    }
  }

  def addProject(projectId: ProjectId, userId: UserId, key: ProjectEncryptionKey)(events: Event*): Unit = {
    val initEvents = events.size
    val ves = verifyEvents(Project.init(userId))(events: _*)
    val now = Instant.now()
    val mde = MockDb.ProjectEntry(projectId, userId, key, initEvents, ves, now, now, Some(now))
    projects += mde
    addProjectAccess(projectId, userId, ProjectPerm.Admin)
  }

  private def getProjectAccessEntry(pid: ProjectId, uid: UserId) =
    projectAccess.find(e => e.pid ==* pid && e.uid ==* uid)

  override def getProjectAccess(pid: ProjectId, uid: UserId) = Eval.always[Option[ProjectPerm]] {
    getProjectAccessEntry(pid, uid).map(_.perm)
  }

  private def nextProjectId(): ProjectId =
    ProjectId(1 + projects.underlyingMap.keysIterator.map(_.value).foldLeft(0L)(_ max _))

  override protected def _createProject(id: UserId, initEvents: Vector[ActiveEvent], p: Project, k: ProjectEncryptionKey) = Eval.always[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, id, k)(initEvents: _*)
    pid
  }

  def createEmptyProject(id: UserId): ProjectId =
    createProject(id, Vector.empty, Project.init(id), DbLaws.genProjectEncryptionKey.sample()).value

  override def getAllProjectMetaDataForUser(id: UserId) = Eval.always[List[ProjectMetaData]] {
    val perms = projectAccess.iterator.filter(_.uid ==* id).map(e => e.pid -> e.perm).toMap

    projects.valuesIterator
      .filter(perms.keySet contains _.projectId)
      .map(e => e.projectMetaData(perms(e.projectId)))
      .toList
  }

  var loadProjectMetaDataLog = Vector.empty[ProjectId]
  override def getProjectMetaData(pid: ProjectId, uid: UserId) = Eval.always[Option[ProjectMetaData]] {
    loadProjectMetaDataLog :+= pid
    getProjectAccessEntry(pid, uid).map { a =>
      projects.need(pid).projectMetaData(a.perm)
    }
  }

  override def projectSpaInitPage(pid: ProjectId, uid: UserId) = Eval.always[Option[DB.ProjectSpaInitPage]] {
    for {
      u <- users.find(_.id ==* uid)
      p <- projects.get(pid)
    } yield
      DB.ProjectSpaInitPage(p.creatorId, p.project.name, p.encKey, u.encKey)
  }

  var loadProjectLog = Vector.empty[ProjectId]
  override def getProjectEvents(id: ProjectId, f: DB.EventFilter) = Eval.always {
    loadProjectLog :+= id
    val r = projects.need(id).projectLoad
    \/-(f match {
      case DB.EventFilter.IncludeAll     => r
      case DB.EventFilter.ExcludeUpTo(o) => r.filter(_.ord > o)
      case DB.EventFilter.Set(o)         => r.filter(x => o.contains(x.ord))
    })
  }

  override protected def _saveProjectEvent(pid: ProjectId,
                                           ord: EventOrd,
                                           e  : ActiveEvent,
                                           p  : Project,
                                           uid: UserId) = Eval.always[DB.SaveProjectEventError \/ VerifiedEvent] {
    val entry = projects.need(pid)
    def update(events: VerifiedEvent.Seq): Unit =
      projects += entry.copy(events = events, lastUpdatedAt = Some(Instant.now()))
    val ve = verifyEvent(entry.project, e)
    if (entry.events.isEmpty) {
      update(VerifiedEvent.Seq.empty + ve)
      \/-(ve)
    } else if (ord.immediatelyFollows(entry.events.lastKey.ord)) {
      update(entry.events + ve)
      \/-(ve)
    } else
      -\/(DB.SaveProjectEventError.OrdInUse)
  }

  override protected def _importProject(uid: UserId, events: VerifiedEvent.Seq, project: Project, key: ProjectEncryptionKey) = Eval.always[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, uid, key)()
    val entry = projects.need(pid)
    val newEntry = entry.copy(events = events, lastUpdatedAt = events.lastOption.map(_.createdAt).orElse(Some(Instant.now())))
    projects += newEntry
    pid
  }

  override def getUserId(user: Username \/ EmailAddr) = Eval.always[Option[UserId]] {
    getUser(user).map(_.id)
  }

  override def withTransactionLevel[G[_], A](runDB: Eval ~> G, level: Int)(f: Eval[A]): G[A] =
    runDB(f)

  var globalEvents = Vector.empty[GlobalEvent]
  override def logGlobalEvent(e: GlobalEvent) = Eval.always[Unit] {
    globalEvents :+= e
  }

  override def logGlobalEventIf(cond: Boolean)(e: => GlobalEvent) = Eval.always[Unit] {
    if (cond)
      globalEvents :+= e
  }

  def assertNoDbChange[A](a: => A): A =
    assertNoChange("assertNoChange:userPlaceholders", userPlaceholders.iterator.map(_.toString).mkString("\n"))(
      assertNoChange("assertNoChange:users", users.mkString("\n"))(
        assertNoChange("assertNoChange:projects", projects.values.mkString("\n"))(
          a)))

  override val userStats: Eval[DB.ForOps.UserStats] =
    Eval.always(DB.ForOps.UserStats(
      registered = users.size,
      total = users.size + userPlaceholders.size))

  override val tableStats =
    Eval.now(Nil)

  override val dbSize =
    Eval.now(0L)

  override def updateProjectAccess(id    : ProjectId,
                                   remove: Set[UserId],
                                   add   : Map[UserId, ProjectPerm]) = Eval.always[DB.SaveProjectEventError.OnAccess \/ Unit] {
    val orig = projectAccess
    val remove2 = remove ++ add.keys

    projectAccess = projectAccess.filterNot { e => e.pid ==* id && remove2.contains(e.uid) }

    add.foreach { case (u, perm) => addProjectAccess(id, u, perm) }

    val result: DB.SaveProjectEventError.OnAccess \/ Unit =
      projectAccess.find(e => e.pid ==* id && e.perm ==* ProjectPerm.Admin) match {
        case Some(_) => \/-(())
        case None    => -\/(DB.SaveProjectEventError.OnAccess.CantRemoveLastAdmin)
      }

    if (result.isLeft)
      projectAccess = orig

    result
  }

  def getProjectAccess(id: ProjectId) = Eval.always[ProjectAccess] {
    ProjectAccess(
      projectAccess.iterator
        .filter(_.pid ==* id)
        .map(e => (Obfuscators.userId.obfuscate(e.uid), e.perm))
        .toMap
    )
  }

  def needProjectCreator(id: ProjectId): UserId =
    projects.need(id).creatorId

  override protected def updateProjectName(id: ProjectId, name: Project.Name) = Eval.always[Unit] {
    // Hmm, we don't store the project name in ProjectEntry
  }

  override def getProjectRolodex(pid: ProjectId, exclude: UserId) = Eval.always[Rolodex] {
    val ids = projectAccess.iterator.filter(a => a.pid ==* pid && a.uid !=* exclude).map(_.uid).toSet
    Rolodex(needUsernamesByUserId(ids).value.mapKeysNow(Obfuscators.userId.obfuscate))
  }
}
