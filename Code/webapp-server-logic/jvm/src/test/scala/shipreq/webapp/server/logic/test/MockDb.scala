package shipreq.webapp.server.logic.test

import cats.syntax.all._
import cats.{Eval, ~>}
import java.time.Instant
import shipreq.base.test.Incrementor
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.project.data.{Live => _, _}
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.social._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.util.Obfuscators

object MockDb {
  final case class UserEntry(id           : UserId,
                             username     : Username,
                             emailAddr    : EmailAddr,
                             ps           : PasswordAndSalt,
                             encKey       : UserEncryptionKey,
                             createdAt    : Instant,
                             resetPassword: Option[(VerificationToken, Instant)] = None) {
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
                                userId       : UserId,
                                encKey       : ProjectEncryptionKey,
                                initEvents   : Int,
                                events       : VerifiedEvent.Seq,
                                createdAt    : Instant,
                                accessedAt   : Instant,
                                lastUpdatedAt: Option[Instant]) {

    lazy val project: Project =
      ApplyEvent.trusted(events)(Project.empty).getOrThrow()

    lazy val projectMetaData: ProjectMetaData =
      ProjectMetaData.fromProject(project)(
        id            = Obfuscators.projectId.obfuscate(projectId),
        eventsInit    = initEvents,
        eventsTotal   = events.size,
        createdAt     = createdAt,
        accessedAt    = accessedAt,
        lastUpdatedAt = lastUpdatedAt)

    def projectLoad: VerifiedEvent.Seq =
      events
  }

  def withLiveClock(): MockDb =
    new MockDb(Eval.always(Instant.now()))
}

// =====================================================================================================================

final class MockDb(_now: Eval[Instant]) extends DB.Algebra[Eval] with DB.ForSecurity[Eval] with DB.ForOps[Eval] {

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

  var prevTokenId = 0
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

  def newUserId(): UserId = {
    nextToken()
    val id = UserId(prevTokenId)
    val x = "u" + id.value
    users ::= MockDb.UserEntry(
      id        = id,
      username  = Username(x),
      emailAddr = EmailAddr(x + "@inmem.com"),
      ps        = PasswordAndSalt(PasswordHash(x), Salt(x)),
      encKey    = UserEncryptionKey(BinaryData.fromStringBytes(x + "-user-key")),
      createdAt = now.value,
    )
    id
  }

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

  private var projects: IMap[ProjectId, MockDb.ProjectEntry] =
    IMap.empty(_.projectId)

  def addProject(projectId: ProjectId, userId: UserId, key: ProjectEncryptionKey)(events: Event*): Unit = {
    val initEvents = events.size
    val ves = verifyEvents(Project.empty)(events: _*)
    val now = Instant.now()
    val mde = MockDb.ProjectEntry(projectId, userId, key, initEvents, ves, now, now, Some(now))
    projects = projects.add(mde)
  }

  override def getProjectOwner(id: ProjectId) = Eval.always[Option[UserId]] {
    projects.get(id).map(_.userId)
  }

  private def nextProjectId(): ProjectId =
    ProjectId(1 + projects.underlyingMap.keysIterator.map(_.value).foldLeft(0L)(_ max _))

  override def createProject(id: UserId, initEvents: Vector[ActiveEvent], p: Project, k: ProjectEncryptionKey) = Eval.always[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, id, k)(initEvents: _*)
    pid
  }

  override def getAllProjectMetaDataForUser(id: UserId) = Eval.always[List[ProjectMetaData]] {
    projects.valuesIterator
      .filter(_.userId ==* id)
      .map(_.projectMetaData)
      .toList
  }

  var loadProjectMetaDataLog = Vector.empty[ProjectId]
  override def getProjectMetaData(id: ProjectId) = Eval.always[Option[ProjectMetaData]] {
    loadProjectMetaDataLog :+= id
    projects.get(id).map(_.projectMetaData)
  }

  override def projectSpaInitPage(pid: ProjectId, uid: UserId) = Eval.always[Option[DB.ProjectSpaInitPage]] {
    for {
      u <- users.find(_.id ==* uid)
      p <- projects.get(pid)
    } yield DB.ProjectSpaInitPage(p.project.name, u.encKey, p.encKey)
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

  override def saveProjectEvent(pid: ProjectId,
                                ord: EventOrd,
                                e  : ActiveEvent,
                                p  : Project,
                                uid: UserId) = Eval.always[DB.SaveProjectEventError \/ VerifiedEvent] {
    val entry = projects.need(pid)
    def update(events: VerifiedEvent.Seq): Unit =
      projects = projects + entry.copy(events = events, lastUpdatedAt = Some(Instant.now()))
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

  override def createProject(uid: UserId, events: VerifiedEvent.Seq, project: Project, key: ProjectEncryptionKey) = Eval.always[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, uid, key)()
    val entry = projects.need(pid)
    val newEntry = entry.copy(events = events, lastUpdatedAt = events.lastOption.map(_.createdAt).orElse(Some(Instant.now())))
    projects = projects + newEntry
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

  // ===================================================================================================================
  // User group stuff

  private val nextUserGroupId = Incrementor.long(0, UserGroup.Id.apply)
  private var userGroups      = List.empty[UserGroup[UserGroup.Id]]
  private var userGroupTree   = Set.empty[UserGroup.Rel[UserGroup.Id, UserGroup.Id]]
  private var userGroupUsers  = Set.empty[UserGroup.Rel[UserGroup.Id, UserId]]

  private def userGroupTreeGraph(userGroupTree: Set[UserGroup.Rel[UserGroup.Id, UserGroup.Id]] = userGroupTree) =
    Digraph.BiDir(Multimap.empty[UserGroup.Id, Set, UserGroup.Id].addPairs(userGroupTree.iterator.map(_.fromTo).toSeq: _*))

  def getUserGroupUniverseU(id: UserGroup.Id): UserGroup.Universe[UserId, Unit, UserGroup.Id, Unit] =
    getUserGroupUniverseU(id, userGroups, userGroupTree, userGroupUsers)

  private def universeScope(graph: Digraph.BiDir[UserGroup.Id], usersGroupsIds: List[UserGroup.Id]): Set[UserGroup.Id] = {
    val roots = graph.terminals(Backwards, usersGroupsIds.toList)
    val tc    = graph.transitiveClosure(Forwards, graph.members ++ usersGroupsIds)
    (roots ++ usersGroupsIds).flatMap(tc(_))
  }

  private def getUserGroupUniverseU(id            : UserGroup.Id,
                                    userGroups    : List[UserGroup[UserGroup.Id]],
                                    userGroupTree : Set[UserGroup.Rel[UserGroup.Id, UserGroup.Id]],
                                    userGroupUsers: Set[UserGroup.Rel[UserGroup.Id, UserId]]): UserGroup.Universe[UserId, Unit, UserGroup.Id, Unit] = {
    val graph = userGroupTreeGraph(userGroupTree)
    val ids   = universeScope(graph, id :: Nil)
    mkUniverse(ids, userGroups, userGroupTree, userGroupUsers)(_ => (), _ => ())
  }

  override def getUserGroupUniverseForUser(id: UserId) = Eval.always[UserGroup.Universe[UserId, Username, UserGroup.Id, UserGroup[UserGroup.Id]]] {
    val usersGroupsIds = userGroupUsers.iterator.filter(_.to ==* id).map(_.from).toSet
    val graph          = userGroupTreeGraph()
    try {
      val ids = universeScope(graph, usersGroupsIds.toList)
      mkUniverse(ids)(identity, _.username)
    } catch {
      case t: Throwable =>
        import UserGroup.Perm, Perm._
        def ugu(p: Perm) = userGroupUsers.iterator.filter(_.perm ==* p).map(r => s"${r.from.value} -> ${r.to.value}").toArray.sortInPlace().mkString("{", ", ", "}")
        println()
        println(s"Error in MockDb getUserGroupUniverseForUser(${id.value})")
        println("  usersGroupsIds       = " + usersGroupsIds.iterator.map(_.value).toArray.sortInPlace().mkString("{", ", ", "}"))
        println("  userGroupTreeGraph() = " + userGroupTreeGraph().map(_.value))
        println("  userGroupUsers[a]    = " + ugu(Admin))
        println("  userGroupUsers[m]    = " + ugu(Member))
        println()
        throw t
    }
  }

  private def mkUniverse[U, G](graphIds      : Set[UserGroup.Id],
                               userGroups    : List[UserGroup[UserGroup.Id]] = this.userGroups,
                               userGroupTree : Set[UserGroup.Rel[UserGroup.Id, UserGroup.Id]] = this.userGroupTree,
                               userGroupUsers: Set[UserGroup.Rel[UserGroup.Id, UserId]] = this.userGroupUsers)
                              (g             : UserGroup[UserGroup.Id] => G,
                               u             : MockDb.UserEntry => U): UserGroup.Universe[UserId, U, UserGroup.Id, G] = {
    import UserGroup._

    val ugTree2  = userGroupTree.filter(r => graphIds.contains(r.from) && graphIds.contains(r.to))
    val ugUsers2 = userGroupUsers.filter(r => graphIds.contains(r.from))
    val userIds  = ugUsers2.iterator.map(_.to).toSet

    def pm[A, B, C](rels: Set[Rel[A, B]])(f: Multimap[A, Set, B] => C): Map[Perm, C] =
      Perm.values.iterator.flatMap { p =>
        val pairs = rels.iterator.filter(_.perm ==* p).map(_.fromTo).toSeq
        Option.when(pairs.nonEmpty) {
          val mm    = Multimap.empty[A, Set, B].addPairs(pairs: _*)
          p -> f(mm)
        }
      }.toMap

    Universe[UserId, U, Id, G](
      groupGraphMap    = pm(ugTree2)(Digraph.BiDir(_)),
      groupsToUsersMap = pm(ugUsers2)(identity),
      groups           = userGroups.iterator.filter(g => graphIds.contains(g.id)).map(e => e.id -> g(e)).toMap,
      users            = users.iterator.filter(u => userIds.contains(u.id)).map(e => e.id -> u(e)).toMap,
    )
  }

  override def createUserGroup(name  : UserGroup.Name,
                               handle: UserGroup.Handle,
                               rels  : UserGroup.ARels[Set, UserGroup.Id, UserId],
                              ) = Eval.always[NonEmptySet[UserGroup.ValidationError[UserGroup.Id]] \/ UserGroup.Id] {

    val id       = nextUserGroupId()
    val parents  = rels.parents.map(_.reverse(id))
    val children = rels.children.map(_.from(id))
    val ugUsers  = rels.users.map(_.from(id))
    val ugTree2  = userGroupTree ++ parents ++ children
    val ugUsers2 = userGroupUsers ++ ugUsers
    val ugs2     = UserGroup(id, name, handle) :: userGroups
    val universe = getUserGroupUniverseU(id, ugs2, ugTree2, ugUsers2)
    val errors   = universe.validate(checkForCycles = true)

    NonEmptySet.option(errors).toLeft {
      this.userGroups     = ugs2
      this.userGroupTree  = ugTree2
      this.userGroupUsers = ugUsers2
      id
    }
  }

  override def updateUserGroup(id    : UserGroup.Id,
                               name  : Option[UserGroup.Name],
                               handle: Option[UserGroup.Handle],
                               rels  : UserGroup.ARels[SetDiff, UserGroup.Id, UserId],
                              ) = Eval.always[Set[UserGroup.ValidationError[UserGroup.Id]]] {
    import UserGroup._

    val ug1      = userGroups.find(_.id ==* id).getOrThrow(s"User group #${id.value} not found")
    var ug2      = ug1
    var ugTree2  = userGroupTree
    var ugUsers2 = userGroupUsers

    for (n <- name) ug2 = ug2.copy(name = n)
    for (h <- handle) ug2 = ug2.copy(handle = h)

    var errors = Set.empty[ValidationError[Id]]

    def applySetDiff[A, B](d: SetDiff[A])(f: A => B, to: Set[B])(v: B => Option[ValidationError[Id]]): Set[B] = {
      var results = to

      for (a <- d.removed) {
        val b = f(a)
        v(b) match {
          case None    => results -= b
          case Some(e) => errors += e
        }
      }

      for (a <- d.added) {
        val b = f(a)
        results += b
      }

      results
    }

    def validateUG(id: Id): Option[ValidationError[Id]] =
      Option.when(userGroups.forall(id !=* _.id))(ValidationError.GroupNotFound(id))

    val validateUGUG: Rel[Id, Id] => Option[ValidationError[Id]] =
      r => validateUG(r.from) orElse validateUG(r.to)

    ugTree2  = applySetDiff(rels.children)(_.from(id)   , ugTree2 )(validateUGUG)
    ugTree2  = applySetDiff(rels.parents )(_.reverse(id), ugTree2 )(validateUGUG)
    ugUsers2 = applySetDiff(rels.users   )(_.from(id)   , ugUsers2)(r => validateUG(r.from))

    if (errors.isEmpty) {
      val ugs2     = if (ug1 eq ug2) userGroups else ug2 :: userGroups.filter(_ ne ug1)
      val universe = getUserGroupUniverseU(id, ugs2, ugTree2, ugUsers2)
      errors       = universe.validate(checkForCycles = true)

      if (errors.isEmpty) {
        this.userGroups     = ugs2
        this.userGroupTree  = ugTree2
        this.userGroupUsers = ugUsers2
      }
    }

    errors
  }
}
