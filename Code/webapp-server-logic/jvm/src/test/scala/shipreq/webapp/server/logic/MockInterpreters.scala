package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scalaz.syntax.monad._
import scalaz.{-\/, Name, NaturalTransformation, \/, \/-}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.taskman.api.{Msg, MsgId, MsgStatus, TaskmanApi}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerLogicConfig

object MockDb {
  final case class UserEntry(id           : UserId,
                             username     : Username,
                             emailAddr    : EmailAddr,
                             ps           : PasswordAndSalt,
                             createdAt    : Instant,
                             resetPassword: Option[(SecurityToken, Instant)] = None) {
    def pubids: List[Username \/ EmailAddr] =
      -\/(username) :: \/-(emailAddr) :: Nil

    def toUser: User =
      User(id, username, Set.empty)

    def toUserAndPassword: (User, PasswordAndSalt) =
      (toUser, ps)

    def token: Security.SessionToken =
      Security.SessionToken(Some(toUser))
  }
  object UserEntry {
    implicit def univEq: UnivEq[UserEntry] = UnivEq.derive
  }

  final case class ProjectEntry(projectId    : ProjectId,
                                userId       : UserId,
                                initEvents   : Int,
                                events       : VerifiedEvent.Seq,
                                createdAt    : Instant,
                                lastUpdatedAt: Option[Instant]) {

    lazy val project: Project =
      ApplyEvent.trusted.applyVerified(events)(Project.empty).needRight

    lazy val projectMetaData: ProjectMetaData =
      ProjectMetaData(id              = Obfuscators.projectId.obfuscate(projectId),
                      name            = project.name,
                      initEventCount  = initEvents,
                      totalEventCount = events.size,
                      reqCount        = project.content.reqs.size,
                      createdAt       = createdAt,
                      lastUpdatedAt   = lastUpdatedAt)

    def projectLoad: VerifiedEvent.Seq =
      events
  }
}

final class MockDb(_now: Name[Instant]) extends DB.Algebra[Name] with DB.ForSecurity[Name] with DB.ForOps[Name] {
  import DB.ForOps

  override val now: Name[Instant] =
    _now

  override def getUserAndPasswordByEmail(email: EmailAddr) = Name[Option[(User, PasswordAndSalt)]] {
    getUser(\/-(email)).map(_.toUserAndPassword)
  }

  override def getUserAndPasswordByUsername(username: Username) = Name[Option[(User, PasswordAndSalt)]] {
    getUser(-\/(username)).map(_.toUserAndPassword)
  }

  var usrLoginLog = Vector.empty[(UserId, Option[IP])]
  override def logLoginSuccess(id: UserId, ip: Option[IP]) = Name[Unit] {
    usrLoginLog :+= ((id, ip))
  }

  var prevTokenId = 0
  private def nextToken(): SecurityToken = {
    prevTokenId += 1
    prevToken()
  }

  def prevToken(): SecurityToken = {
    assert(prevTokenId > 0)
    SecurityToken(s"[token-$prevTokenId]")
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

  override def getUserRegistration(e: EmailAddr) = Name[Option[DB.UserRegistration]] {
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

  def getPendingUserRegistration(t: SecurityToken): Option[(EmailAddr, DB.UserRegistration.Pending)] =
    userPlaceholders.iterator.collect {
      case (ea, p: DB.UserRegistration.Pending) if p.token ==* t => (ea, p)
    }.nextOption()

  override def getUserRegistrationTokenIssueDate(t: SecurityToken) = Name[Option[Instant]] {
    getPendingUserRegistration(t).map(_._2.tokenSentAt)
  }

  var users = List.empty[MockDb.UserEntry]

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

  override def completeUserRegistration(token: SecurityToken,
                                        name: PersonName,
                                        username: Username,
                                        ps: PasswordAndSalt,
                                        newsletter: Boolean) =
    now.map { n =>
      (getPendingUserRegistration(token), getUser(-\/(username))) match {
        case (None, _)          => DB.UserRegistrationResult.TokenNotFound
        case (Some(_), Some(_)) => DB.UserRegistrationResult.UsernameTaken
        case (Some((ea, reg)), None) =>
          userPlaceholders = userPlaceholders - ea
          users ::= MockDb.UserEntry(reg.id, username, ea, ps, n)
          DB.UserRegistrationResult.Success(reg.id)
      }
    }

  override def getPasswordResetState(u: Username \/ EmailAddr) = Name[Option[(EmailAddr, DB.PasswordResetState)]] {
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

  override def getResetPasswordTokenIssueDate(t: SecurityToken) = Name[Option[Instant]] {
    users.collectFirst {
      case MockDb.UserEntry(_, _, _, _, _, Some((t2, i))) if t ==* t2 => i
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

  override def updateUserPassword(token: SecurityToken, ps: PasswordAndSalt) = Name[Option[UserId]] {
    users.find(_.resetPassword.exists(_._1 ==* token)).map { u =>
      updateUser(_.id ==* u.id, _.copy(ps = ps, resetPassword = None))
      u.id
    }
  }

  private var projects: IMap[ProjectId, MockDb.ProjectEntry] =
    IMap.empty(_.projectId)

  def addProject(projectId: ProjectId, userId: UserId)(events: Event*): Unit = {
    val initEvents = events.size
    val ves = verifyEvents(Project.empty)(events: _*)
    val now = Instant.now()
    val mde = MockDb.ProjectEntry(projectId, userId, initEvents, ves, now, Some(now))
    projects = projects.add(mde)
  }

  override def getProjectOwner(id: ProjectId) = Name[Option[UserId]] {
    projects.get(id).map(_.userId)
  }

  override def createEmptyProject(id: UserId, initEvents: Int) = Name[ProjectId] {
    val pid = ProjectId(1 + projects.underlyingMap.keysIterator.map(_.value).foldLeft(0L)(_ max _))
    addProject(pid, id)()
    pid
  }

  override def getAllProjectMetaDataForUser(id: UserId) = Name[List[ProjectMetaData]] {
    projects.valuesIterator
      .filter(_.userId ==* id)
      .map(_.projectMetaData)
      .toList
  }

  var loadProjectMetaDataLog = Vector.empty[ProjectId]
  override def getProjectMetaData(id: ProjectId) = Name[Option[ProjectMetaData]] {
    loadProjectMetaDataLog :+= id
    projects.get(id).map(_.projectMetaData)
  }

  override def projectSpaInitPage(id: ProjectId) = Name[Project.Name] {
    projects.get(id).fold("")(_.project.name)
  }

  var loadProjectLog = Vector.empty[ProjectId]
  override def getProjectEvents(id: ProjectId, f: DB.EventFilter) = Name[VerifiedEvent.Seq] {
    loadProjectLog :+= id
    val r = projects.need(id).projectLoad
    f match {
      case DB.EventFilter.IncludeAll     => r
      case DB.EventFilter.ExcludeUpTo(o) => r.filter(_.ord > o)
      case DB.EventFilter.Set(o)         => r.filter(x => o.contains(x.ord))
    }
  }

  override def saveProjectEvents(id: ProjectId, cmds: Traversable[DB.SaveProjectEventCmd]) = Name[Throwable \/ VerifiedEvent.Seq] {
    import scalaz.syntax.traverse._
    cmds.toList.traverse(saveProjectEvent(id, _).value).map(VerifiedEvent.Seq.empty ++ _)
  }

  override def saveProjectEvent(id: ProjectId, cmd: DB.SaveProjectEventCmd) = Name[Throwable \/ VerifiedEvent] {
    val entry = projects.need(id)
    def update(events: VerifiedEvent.Seq): Unit =
      projects = projects + entry.copy(events = events, lastUpdatedAt = Some(Instant.now()))
    val ve = verifyEvent(entry.project, cmd.event, cmd.ord)
    if (entry.events.isEmpty) {
      update(VerifiedEvent.Seq.empty + ve)
      \/-(ve)
    } else if (cmd.ord.immediatelyFollows(entry.events.lastKey.ord)) {
      update(entry.events + ve)
      \/-(ve)
    } else
      -\/(new RuntimeException(s"${cmd.ord} doesn't follow ${entry.events.lastKey.ord}"))
  }

  override def inDbTransaction[A](f: Name[A]) = f
  override def inDbTransaction[A](l: Int, f: Name[A]) = f

  def assertNoDbChange[A](a: => A): A =
    assertNoChange("assertNoChange:userPlaceholders", userPlaceholders.iterator.map(_.toString).mkString("\n"))(
      assertNoChange("assertNoChange:users", users.mkString("\n"))(
        assertNoChange("assertNoChange:projects", projects.values.mkString("\n"))(
          a)))

  override val userStats: Name[ForOps.UserStats] =
    Name(ForOps.UserStats(
      registered = users.size,
      total = users.size + userPlaceholders.size))

  override val tableStats =
    Name(Nil)

  override val dbSize =
    Name(0L)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockServer extends Server.Algebra[Name] {
  var clock = Instant.now()
  override val now = Name(clock)

  override def measureDuration[A](f: Name[A]): Name[(A, Duration)] =
    for {
      start <- now
      a     <- f
      end   <- now
    } yield (a, Duration.between(start, end))

  override def measureDuration_[A](f: Name[A]) =
    measureDuration(f).map(_._2)

  private def durationBorder(duration: Duration, tolerance: Duration = Duration.ofSeconds(2)): Validity => Duration = {
    case Valid   => duration minus tolerance
    case Invalid => duration plus tolerance
  }

  def forwardTimeToEndOfWindow(w: Duration, v: Validity): Unit =
    clock = clock plus durationBorder(w)(v)

  var onDelay = List.empty[() => Unit]
  override def delay[A](f: Name[A], d: Duration) = Name[A] {
    clock = clock plus d
    onDelay match {
      case Nil    => ()
      case h :: t => onDelay = t; h()
    }
    f.value
  }

  var forked = Vector.empty[Name[Any]]
  override def fork[A](f: Name[A]) = Name[Unit] {
    forked :+= f
  }
  def runForked(): Unit = {
    forked.foreach(_.value)
    forked = Vector.empty
  }

  var nextClientIP = Option.empty[IP]
  override val clientIP = Name(nextClientIP)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockTaskman extends TaskmanApi[Name] {
  private var prevMsgId = 0L
  var msgs = Vector.empty[(MsgId, Msg)]

  def reset(): Unit = {
    prevMsgId = 0L
    msgs = Vector.empty
  }

  override def cfgPut(key: String, value: String) = Name[Unit] {
    ()
  }

  override def submitMsg(m: Msg) = Name[MsgId] {
    prevMsgId += 1
    val id = MsgId(prevMsgId)
    msgs :+= (id, m)
    id
  }

  override def queryMsgStatus(id: MsgId) = Name[Option[MsgStatus]] {
    None
  }

  def assertSubmitted(expect: Int): Unit =
    if (msgs.length !=* expect)
      fail(s"Expected $expect Taskman tasks submitted, got ${msgs.length}: ${msgs.mkString(", ")}")

  def assertSubmits[A](expect: Int)(a: => A): A =
    assertDifference("taskman.assertSubmits", msgs.length)(expect)(a)

  def assertLastSubmitted[A](pf: PartialFunction[Msg, A]): A =
    if (msgs.isEmpty)
      fail("No tasks submitted.")
    else
      pf.lift(msgs.last._2) getOrElse
        fail(s"Unexpected Taskman task submitted: ${msgs.last._2}")

//  def assertSubmitted(msg: Msg*): Unit =
//    assertEq(msg.toVector, tasksSubmitted.map(_._2))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockSecurity(override val db: MockDb) extends Security.Algebra[Name] {
  import Security.SessionToken

  var protectedActions = 0
  override def protect[A](vulnerable: Name[A]): Name[A] =
    vulnerable.map { a =>
      protectedActions += 1
      a
    }

  override def attemptLogin(u: Username \/ EmailAddr, p: PlainTextPassword) = Name[Option[User]] {
    db.getUser(u)
      .filter(e => e.ps ==* mkPasswordAndSalt(p, e.ps.salt))
      .map(_.toUser)
  }

  var prevSalt = 0
  override def hashPassword(p: PlainTextPassword) = Name[PasswordAndSalt] {
    prevSalt += 1
    mkPasswordAndSalt(p, Salt(prevSalt.toString))
  }

  def mkPasswordAndSalt(p: PlainTextPassword, salt: Salt): PasswordAndSalt =
    PasswordAndSalt(PasswordHash(s"${salt.base64}:${p.value}"), salt)

  val cookieName = Cookie.Name("MockSecurity")

  override def sessionPersist(token: SessionToken) = Name[Cookie.Update] {
    val header = System.nanoTime().toString + ":"
    val body = token.authenticatedUser match {
      case None    => ""
      case Some(u) => s"${u.id.value} ${u.username.value}"
    }
    val cookie = Cookie(cookieName, header + body, None, None, None)
    Cookie.Update.add(cookie)
  }

  override def sessionRestore(cookies: Cookie.LookupFn) = Name[Option[SessionToken]] {
    cookies(cookieName) map { cookieValue =>
        if (cookieValue.endsWith(":"))
          SessionToken.anonymous
        else {
          val body     = cookieValue.dropWhile(_ != ':').drop(1).split(' ')
          val userId   = UserId(body(0).toInt)
          val username = Username(body(1))
          val user     = User(userId, username, Set.empty)
          SessionToken(Some(user))
        }
    }
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object MockInterpreters {

  val config = ServerLogicConfig(
    baseUrl                    = Url.Absolute.Base("https://test.shipreq.com"),
    publicRegistration         = Allow,
    googleAnalyticsTrackingId  = None,
    taskmanSchema              = "test_taskman",
    initTaskmanOnBoot          = false,
    initTaskmanRetry           = Retries.none,
    jaegerTracingConfig        = None,
    prometheus                 = ServerLogicConfig.Prometheus.default,
    security = ServerLogicConfig.Security(
      attackFrustrationDelay     = 1 hours,
      jwtCookieSecure            = false,
      jwtLifespan                = 24 hours,
      jwtSecret                  = new ServerLogicConfig.Security.JwtSecret("x"*64),
      jwtSecretPrevious          = None,
      passwordSaltLength         = 64,
      securityTokenLength        = 8,
      registrationTokenLifespan  = 7 days,
      passwordResetTokenLifespan = 4 days))
}

class MockInterpreters(modCfg: ServerLogicConfig => ServerLogicConfig = Identity[ServerLogicConfig]) {
  implicit val config         = modCfg(MockInterpreters.config)
  implicit val svr            = new MockServer
  implicit val db             = new MockDb(svr.now)
  implicit val security       = new MockSecurity(db)
  implicit val taskman        = new MockTaskman
  implicit val nameToName     = NaturalTransformation.refl[Name]
  implicit val metrics        = MetricsLogic.const(Name(()))
  implicit val trace          = Trace.Algebra.off[Name]
  implicit val redis          = new Redis.InMemory[Name]
  implicit val publicSpa      = PublicSpaLogic[Name, Name]
  implicit val homeSpa        = HomeSpaLogic[Name, Name]
  implicit val projectSpa     = ProjectSpaLogic[Name, Name]

  implicit object ops extends OpsEndpoints.Base[Name] {
    override val randomToken = Name("blah")
  }

  val user2password = PlainTextPassword("blurp12345")
  lazy val user2 = MockDb.UserEntry(
    UserId(2),
    Username("blurp"),
    EmailAddr("blurp@bar.com"),
    security.hashPassword(user2password).value,
    svr.clock minus Duration.ofDays(50))

  val user3password = PlainTextPassword("user3secret")
  lazy val user3 = MockDb.UserEntry(
    UserId(3),
    Username("user3"),
    EmailAddr("u3@test.com"),
    security.hashPassword(user3password).value,
    svr.clock minus Duration.ofDays(2))

  def assertProtected[A](a: => A): A =
    assertDifference("Protected actions", security.protectedActions)(1)(a)

  def assertUnprotected[A](a: => A): A =
    assertNoChange("Protected actions", security.protectedActions)(a)

  def forwardTimeToEndOfConfirmationWindow(v: Validity): Unit =
    svr.forwardTimeToEndOfWindow(config.security.registrationTokenLifespan, v)

  def forwardTimeToEndOfPasswordResetWindow(v: Validity): Unit =
    svr.forwardTimeToEndOfWindow(config.security.passwordResetTokenLifespan, v)
}
