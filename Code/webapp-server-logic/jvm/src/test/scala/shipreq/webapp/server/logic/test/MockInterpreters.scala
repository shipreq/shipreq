package shipreq.webapp.server.logic.test

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import scalaz.syntax.monad._
import scalaz.{Catchable, Monad, Name, NaturalTransformation, ~>}
import shipreq.base.ops.Trace
import shipreq.base.test.JsonTestUtil._
import shipreq.base.test.SyncEffect
import shipreq.base.util._
import shipreq.taskman.api.{Task, TaskId, TaskStatus, TaskmanApi}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.config.{ScalaJsManifest, ServerLogicConfig}
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.dispatch.Cookie
import shipreq.webapp.server.logic.event.ApplyEventAlgebra
import shipreq.webapp.server.logic.impl._
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
      ApplyEvent.trusted.applyVerified(events)(Project.empty).getOrThrow()

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

  val nameUnit: Name[Unit] =
    scalaz.Value(())
}

final class MockDb(_now: Name[Instant]) extends DB.Algebra[Name] with DB.ForSecurity[Name] with DB.ForOps[Name] {

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

  def getPendingUserRegistration(t: VerificationToken): Option[(EmailAddr, DB.UserRegistration.Pending)] =
    userPlaceholders.iterator.collect {
      case (ea, p: DB.UserRegistration.Pending) if p.token ==* t => (ea, p)
    }.nextOption()

  override def getUserRegistrationTokenIssueDate(t: VerificationToken) = Name[Option[Instant]] {
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

  override def getResetPasswordTokenIssueDate(t: VerificationToken) = Name[Option[Instant]] {
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

  override def updateUserPassword(token: VerificationToken, ps: PasswordAndSalt) = Name[Option[UserId]] {
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

  override def getProjectOwner(id: ProjectId) = Name[Option[UserId]] {
    projects.get(id).map(_.userId)
  }

  private def nextProjectId(): ProjectId =
    ProjectId(1 + projects.underlyingMap.keysIterator.map(_.value).foldLeft(0L)(_ max _))

  override def createProject(id: UserId, initEvents: Vector[ActiveEvent], p: Project, k: ProjectEncryptionKey) = Name[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, id, k)(initEvents: _*)
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
  override def getProjectEvents(id: ProjectId, f: DB.EventFilter) = Name {
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
                                uid: UserId) = Name[DB.SaveProjectEventError \/ VerifiedEvent] {
    val entry = projects.need(pid)
    def update(events: VerifiedEvent.Seq): Unit =
      projects = projects + entry.copy(events = events, lastUpdatedAt = Some(Instant.now()))
    val ve = verifyEvent(entry.project, e, ord)
    if (entry.events.isEmpty) {
      update(VerifiedEvent.Seq.empty + ve)
      \/-(ve)
    } else if (ord.immediatelyFollows(entry.events.lastKey.ord)) {
      update(entry.events + ve)
      \/-(ve)
    } else
      -\/(DB.SaveProjectEventError.OrdInUse)
  }

  override def createProject(uid: UserId, events: VerifiedEvent.Seq, project: Project, key: ProjectEncryptionKey) = Name[ProjectId] {
    val pid = nextProjectId()
    addProject(pid, uid, key)()
    val entry = projects.need(pid)
    val newEntry = entry.copy(events = events, lastUpdatedAt = events.lastOption.map(_.createdAt).orElse(Some(Instant.now())))
    projects = projects + newEntry
    pid
  }

  override def getUserId(user: Username \/ EmailAddr) = Name[Option[UserId]] {
    getUser(user).map(_.id)
  }

  override def withTransactionLevel[G[_], A](runDB: Name ~> G, level: Int)(f: Name[A]): G[A] =
    runDB(f)

  var globalEvents = Vector.empty[GlobalEvent]
  override def logGlobalEvent(e: GlobalEvent) = Name[Unit] {
    globalEvents :+= e
  }

  def assertNoDbChange[A](a: => A): A =
    assertNoChange("assertNoChange:userPlaceholders", userPlaceholders.iterator.map(_.toString).mkString("\n"))(
      assertNoChange("assertNoChange:users", users.mkString("\n"))(
        assertNoChange("assertNoChange:projects", projects.values.mkString("\n"))(
          a)))

  override val userStats: Name[DB.ForOps.UserStats] =
    Name(DB.ForOps.UserStats(
      registered = users.size,
      total = users.size + userPlaceholders.size))

  override val tableStats =
    Name(Nil)

  override val dbSize =
    Name(0L)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockServer[F[_]]()(implicit F: Monad[F], se: SyncEffect[F]) extends Server.Algebra[F] {
  @volatile var clock = Instant.now()

  override val now = F.point(clock)

  def incTime(d: Duration): Unit =
    clock = clock.plus(d)

  def incTimeMs(ms: Long): Unit =
    clock = clock.plusMillis(ms)

  def incTimeSec(sec: Long): Unit =
    clock = clock.plusSeconds(sec)

  override def measureDuration[A](f: F[A]): F[(A, Duration)] =
    for {
      start <- now
      a     <- f
      end   <- now
    } yield (a, Duration.between(start, end))

  override def measureDuration_[A](f: F[A]) =
    measureDuration(f).map(_._2)

  private def durationBorder(duration: Duration, tolerance: Duration = Duration.ofSeconds(2)): Validity => Duration = {
    case Valid   => duration minus tolerance
    case Invalid => duration plus tolerance
  }

  def forwardTimeToEndOfWindow(w: Duration, v: Validity): Unit =
    clock = clock plus durationBorder(w)(v)

  var onDelay = List.empty[() => Unit]
  override def delay[A](f: F[A], d: Duration) = F.point[A] {
    clock = clock plus d
    onDelay match {
      case Nil    => ()
      case h :: t => onDelay = t; h()
    }
    se.unsafeRun(f)
  }

  var forked = Vector.empty[F[_]]
  override def fork[A](f: F[A]) = F.point[Unit] {
    forked :+= f
  }
  def runForked(): Unit = {
    forked.foreach(se.unsafeRun(_))
    forked = Vector.empty
  }

  var nextClientIP = Option.empty[IP]
  override val clientIP = F.point(nextClientIP)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final class MockTaskman extends TaskmanApi[Name] {
  private var prevMsgId = 0L
  var msgs = Vector.empty[(TaskId, Task)]

  def reset(): Unit = {
    prevMsgId = 0L
    msgs = Vector.empty
  }

  override def cfgPut(key: String, value: String) = Name[Unit] {
    ()
  }

  override def submit(m: Task) = Name[TaskId] {
    prevMsgId += 1
    val id = TaskId(prevMsgId)
    msgs :+= ((id, m))
    id
  }

  override def getStatus(id: TaskId) = Name[Option[TaskStatus]] {
    None
  }

  def assertSubmitted(expect: Int): Unit =
    if (msgs.length !=* expect)
      fail(s"Expected $expect Taskman tasks submitted, got ${msgs.length}: ${msgs.mkString(", ")}")

  def assertSubmits[A](expect: Int)(a: => A): A =
    assertDifference("taskman.assertSubmits", msgs.length)(expect)(a)

  def assertLastSubmitted[A](pf: PartialFunction[Task, A]): A =
    if (msgs.isEmpty)
      fail("No tasks submitted.")
    else
      pf.lift(msgs.last._2) getOrElse
        fail(s"Unexpected Taskman task submitted: ${msgs.last._2}")

//  def assertSubmitted(msg: Msg*): Unit =
//    assertEq(msg.toVector, tasksSubmitted.map(_._2))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object MockSecurity {
  private[MockSecurity] object Codecs {
    import shipreq.webapp.server.logic.algebra.Security._

    implicit val decoderSessionId: Decoder[SessionId] =
      Decoder[String].map(SessionId.apply)

    implicit val encoderSessionId: Encoder[SessionId] =
      Encoder[String].contramap(_.value)

    implicit val decoderUserId: Decoder[UserId] =
      Decoder[Long].map(UserId.apply)

    implicit val encoderUserId: Encoder[UserId] =
      Encoder[Long].contramap(_.value)

    implicit val decoderUsername: Decoder[Username] =
      Decoder[String].map(Username.apply)

    implicit val encoderUsername: Encoder[Username] =
      Encoder[String].contramap(_.value)

    implicit val decoderUser: Decoder[User] =
      Decoder.forProduct2("id", "username")(User.apply)

    implicit val encoderUser: Encoder[User] =
      Encoder.forProduct2("id", "username")(a => (a.id, a.username))

    implicit val decoderSessionToken: Decoder[SessionToken[Instant]] =
      Decoder.forProduct3("sessionId", "authenticatedUser", "expiry")(SessionToken.apply[Instant])

    implicit val encoderSessionToken: Encoder[SessionToken[Instant]] =
      Encoder.forProduct3("sessionId", "authenticatedUser", "expiry")(a => (a.sessionId, a.authenticatedUser, a.expiry))
  }
}

final class MockSecurity(override val db: MockDb, now: Name[Instant], cfg: ServerLogicConfig.Security) extends Security.Algebra[Name] {
  import MockSecurity.Codecs._
  import shipreq.webapp.server.logic.algebra.Security._

  override val F = Monad[Name]

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

  def expiry() = now.value.plus(cfg.jwtLifespan)

  override def sessionPersist(token: SessionToken[Any]) = Name[Cookie.Update] {
    val token2 = token.copy(expiry = expiry())
    val json   = token2.asJson.noSpaces
    val cookie = Cookie(cookieName, json, None, None, None)
    Cookie.Update.add(cookie)
  }

  override def sessionRestore(cookies: Cookie.LookupFn) = Name[SessionRestoreResult[Instant]] {
    cookies(cookieName) match {
      case Some(cookieValue) =>
        SessionRestoreResult.Success(decodeOrThrow[SessionToken[Instant]](cookieValue))

      case None =>
        SessionRestoreResult.None
    }
  }
}

final class MockCrypto extends Crypto[Name] {

  private val default = Crypto.default[Name]

  private var nextKey = 0

  override def generateKey256 = Name[BinaryData] {
    val i = nextKey
    nextKey += 1
    MockCrypto.generateKey256(i)
  }

  def generateUserKey() =
    UserEncryptionKey(generateKey256.value)

  def generateProjectKey() =
    ProjectEncryptionKey(generateKey256.value)

  override def sha256(input: BinaryData): BinaryData =
    default.sha256(input)
}

object MockCrypto {
  def generateKey256(i: Int): BinaryData = {
    val a = new Array[Byte](32)
    a(31) = (i & 0xff).toByte
    a(30) = ((i >> 8) & 0xff).toByte
    BinaryData.unsafeFromArray(a)
  }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object MockInterpreters {

  val config = ServerLogicConfig(
    baseUrl                    = Url.Absolute.Base("https://test.shipreq.com"),
    staticAssetCdn             = Some(AssetManifest.StaticAssetCdn("https://static.shipreq.com")),
    publicRegistration         = Allow,
    applyEventThresholdMs      = 1000,
    googleAnalyticsTrackingId  = None,
    taskmanSchema              = "test_taskman",
    scalaJsManifest            = ScalaJsManifest("/public.js", "/home.js", "/project.js", "/ww.js"),
    ssr                        = ServerLogicConfig.SsrConfig(false),
    initTaskmanOnBoot          = false,
    initTaskmanRetry           = Retries.none,
    jaegerTracingConfig        = None,
    prometheus                 = ServerLogicConfig.Prometheus.default,
    projectSpa                 = ProjectSpaLogic.Config.default.copy(2),
    security = ServerLogicConfig.Security(
      attackFrustrationDelay     = 1 hours,
      jwtCookieSecure            = false,
      jwtLifespan                = 24 hours,
      jwtSecret                  = new ServerLogicConfig.Security.JwtSecret("x"*64),
      jwtSecretPrevious          = None,
      passwordSaltLength         = 64,
      verificationTokenLength        = 8,
      registrationTokenLifespan  = 7 days,
      passwordResetTokenLifespan = 4 days))

  implicit val catchableName: Catchable[Name] =
    new Catchable[Name] {
      override def attempt[A](f: Name[A]): Name[Throwable \/ A] =
        Name {
          try \/-(f.value)
          catch {
            case t: Throwable => -\/(t)
          }
        }
      override def fail[A](err: Throwable): Name[A] =
        Name(throw err)
    }
}

class MockInterpreters(modCfg         : ServerLogicConfig => ServerLogicConfig = Identity[ServerLogicConfig],
                       specificMockDb : Option[MockDb]                         = None,
                       specificRedis  : Option[Redis.InMemory[Name]]           = None,
                       specificTaskman: Option[MockTaskman]                    = None,
                      ) {

  import MockInterpreters.catchableName

  implicit val config         = modCfg(MockInterpreters.config)
  implicit val assetManifest  = config.assetManifest
  implicit val sjsManifest    = config.scalaJsManifest
  implicit val crypto         = new MockCrypto
  implicit val svr            = new MockServer[Name]
  implicit val db             = specificMockDb.getOrElse(new MockDb(svr.now))
  implicit val security       = new MockSecurity(db, svr.now, config.security)
  implicit val taskman        = specificTaskman.getOrElse(new MockTaskman)
  implicit val nameToName     = NaturalTransformation.refl[Name]
  implicit val apEvent        = ApplyEventAlgebra.trusted[Name]
  implicit val metrics        = MetricsAlgebra.const(Name(()))
  implicit val trace          = Trace.Algebra.off[Name]
  implicit val redis          = specificRedis.getOrElse(new Redis.InMemory[Name])
  implicit val common         = CommonProtocolLogic[Name]
  implicit val publicSpa      = PublicSpaLogic[Name, Name]
  implicit val homeSpa        = HomeSpaLogic[Name, Name]
  implicit val projectSpa     = ProjectSpaLogic[Name, Name](config.projectSpa)

  implicit object ops extends OpsEndpointLogic.Base[Name] {
    override val randomToken = Name("blah")
  }

  val user2password = PlainTextPassword("blurp12345")
  lazy val user2 = MockDb.UserEntry(
    UserId(2),
    Username("blurp"),
    EmailAddr("blurp@bar.com"),
    security.hashPassword(user2password).value,
    UserEncryptionKey(crypto.generateKey256.value),
    svr.clock minus Duration.ofDays(50))

  val user3password = PlainTextPassword("user3secret")
  lazy val user3 = MockDb.UserEntry(
    UserId(3),
    Username("user3"),
    EmailAddr("u3@test.com"),
    security.hashPassword(user3password).value,
    UserEncryptionKey(crypto.generateKey256.value),
    svr.clock minus Duration.ofDays(2))

  def withConfig(f: ServerLogicConfig => ServerLogicConfig): MockInterpreters =
    new MockInterpreters(
      modCfg          = _ => f(config),
      specificMockDb  = Some(db),
      specificRedis   = Some(redis),
      specificTaskman = Some(taskman),
    )

  def assertProtected[A](a: => A): A =
    assertDifference("Protected actions", security.protectedActions)(1)(a)

  def assertUnprotected[A](a: => A): A =
    assertNoChange("Protected actions", security.protectedActions)(a)

  def forwardTimeToEndOfConfirmationWindow(v: Validity): Unit =
    svr.forwardTimeToEndOfWindow(config.security.registrationTokenLifespan, v)

  def forwardTimeToEndOfPasswordResetWindow(v: Validity): Unit =
    svr.forwardTimeToEndOfWindow(config.security.passwordResetTokenLifespan, v)

  final implicit class MockInterpreterExtSessionToken[E](self: Security.SessionToken[E]) {
    def withExpiryNow(): Security.SessionToken[Instant] =
      self.copy(expiry = Instant.now())

    def withExpirySoon(): Security.SessionToken[Instant] =
      self.copy(expiry = Instant.now().plusSeconds(3000))
  }

  final implicit class MockInterpreterExtOptionSessionToken[E](self: Option[Security.SessionToken[E]]) {
    def withSession(from: Option[Security.SessionToken[Any]]): Option[Security.SessionToken[E]] =
      // self.map(_.copy(sessionId = from.flatMap(_.sessionId)))
     (self, from) match {
       case (Some(s), Some(f)) => Some(s.withSession(f))
       case _                  => self
     }

    def withoutExpiry: Option[Security.SessionToken[Unit]] =
      self.map(_.withoutExpiry)
  }

}
