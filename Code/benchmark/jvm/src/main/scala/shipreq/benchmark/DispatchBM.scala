package shipreq.benchmark

import cats.effect.{ExitCase, IO, Sync}
import cats.free.Trampoline
import cats.implicits._
import cats.{Eval, Monad}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.base.util._
import shipreq.taskman.api.TaskId
import shipreq.webapp.base.config.{AssetManifest, Urls}
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.config._
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.dispatch._
import shipreq.webapp.server.logic.logic._
import zio.UIO

/**
  * > sbt
  * > benchmark-jvm/jmh:run -wi 10 -i 10 -f 2 -prof gc DispatchBM
  *
  * [info] Benchmark                                               Mode  Cnt       Score     Error   Units
  *
  * [info] DispatchBM.catsIO                                       avgt   20      23.809 ±   0.125   us/op
  * [info] DispatchBM.coeval                                       avgt   20      23.966 ±   0.044   us/op
  * [info] DispatchBM.fn0                                          avgt   20      19.853 ±   0.029   us/op
  * [info] DispatchBM.name                                         avgt   20      22.485 ±   0.118   us/op
  * [info] DispatchBM.trampoline                                   avgt   20      23.785 ±   0.500   us/op
  * [info] DispatchBM.zio                                          avgt   20      81.125 ±   0.300   us/op
  *
  * [info] DispatchBM.catsIO:·gc.alloc.rate                        avgt   20    2738.391 ±  14.444  MB/sec
  * [info] DispatchBM.catsIO:·gc.alloc.rate.norm                   avgt   20   71784.004 ±   0.008    B/op
  * [info] DispatchBM.catsIO:·gc.churn.PS_Eden_Space               avgt   20    2738.271 ±  17.323  MB/sec
  * [info] DispatchBM.catsIO:·gc.churn.PS_Eden_Space.norm          avgt   20   71780.873 ± 250.948    B/op
  * [info] DispatchBM.catsIO:·gc.churn.PS_Survivor_Space           avgt   20       0.115 ±   0.017  MB/sec
  * [info] DispatchBM.catsIO:·gc.churn.PS_Survivor_Space.norm      avgt   20       3.019 ±   0.437    B/op
  * [info] DispatchBM.catsIO:·gc.count                             avgt   20    2564.000            counts
  * [info] DispatchBM.catsIO:·gc.time                              avgt   20    1720.000                ms
  * [info] DispatchBM.coeval:·gc.alloc.rate                        avgt   20    2744.635 ±   6.216  MB/sec
  * [info] DispatchBM.coeval:·gc.alloc.rate.norm                   avgt   20   72424.004 ±  71.273    B/op
  * [info] DispatchBM.coeval:·gc.churn.PS_Eden_Space               avgt   20    2744.888 ±  11.194  MB/sec
  * [info] DispatchBM.coeval:·gc.churn.PS_Eden_Space.norm          avgt   20   72430.699 ± 259.425    B/op
  * [info] DispatchBM.coeval:·gc.churn.PS_Survivor_Space           avgt   20       0.138 ±   0.025  MB/sec
  * [info] DispatchBM.coeval:·gc.churn.PS_Survivor_Space.norm      avgt   20       3.647 ±   0.651    B/op
  * [info] DispatchBM.coeval:·gc.count                             avgt   20    2652.000            counts
  * [info] DispatchBM.coeval:·gc.time                              avgt   20    1732.000                ms
  * [info] DispatchBM.fn0:·gc.alloc.rate                           avgt   20    2632.519 ±   3.904  MB/sec
  * [info] DispatchBM.fn0:·gc.alloc.rate.norm                      avgt   20   57544.003 ±   0.006    B/op
  * [info] DispatchBM.fn0:·gc.churn.PS_Eden_Space                  avgt   20    2632.722 ±   9.870  MB/sec
  * [info] DispatchBM.fn0:·gc.churn.PS_Eden_Space.norm             avgt   20   57548.386 ± 185.780    B/op
  * [info] DispatchBM.fn0:·gc.churn.PS_Survivor_Space              avgt   20       0.139 ±   0.020  MB/sec
  * [info] DispatchBM.fn0:·gc.churn.PS_Survivor_Space.norm         avgt   20       3.038 ±   0.439    B/op
  * [info] DispatchBM.fn0:·gc.count                                avgt   20    2560.000            counts
  * [info] DispatchBM.fn0:·gc.time                                 avgt   20    1720.000                ms
  * [info] DispatchBM.name:·gc.alloc.rate                          avgt   20    2750.999 ±  14.403  MB/sec
  * [info] DispatchBM.name:·gc.alloc.rate.norm                     avgt   20   68104.004 ±   0.007    B/op
  * [info] DispatchBM.name:·gc.churn.PS_Eden_Space                 avgt   20    2750.918 ±  16.699  MB/sec
  * [info] DispatchBM.name:·gc.churn.PS_Eden_Space.norm            avgt   20   68101.786 ± 145.504    B/op
  * [info] DispatchBM.name:·gc.churn.PS_Survivor_Space             avgt   20       0.134 ±   0.018  MB/sec
  * [info] DispatchBM.name:·gc.churn.PS_Survivor_Space.norm        avgt   20       3.317 ±   0.451    B/op
  * [info] DispatchBM.name:·gc.count                               avgt   20    2577.000            counts
  * [info] DispatchBM.name:·gc.time                                avgt   20    1733.000                ms
  * [info] DispatchBM.trampoline:·gc.alloc.rate                    avgt   20    2847.702 ±  47.695  MB/sec
  * [info] DispatchBM.trampoline:·gc.alloc.rate.norm               avgt   20   74544.004 ± 320.729    B/op
  * [info] DispatchBM.trampoline:·gc.churn.PS_Eden_Space           avgt   20    2847.855 ±  47.197  MB/sec
  * [info] DispatchBM.trampoline:·gc.churn.PS_Eden_Space.norm      avgt   20   74548.852 ± 412.175    B/op
  * [info] DispatchBM.trampoline:·gc.churn.PS_Survivor_Space       avgt   20       0.121 ±   0.018  MB/sec
  * [info] DispatchBM.trampoline:·gc.churn.PS_Survivor_Space.norm  avgt   20       3.169 ±   0.478    B/op
  * [info] DispatchBM.trampoline:·gc.count                         avgt   20    2497.000            counts
  * [info] DispatchBM.trampoline:·gc.time                          avgt   20    1719.000                ms
  * [info] DispatchBM.zio:·gc.alloc.rate                           avgt   20    1881.123 ±   4.929  MB/sec
  * [info] DispatchBM.zio:·gc.alloc.rate.norm                      avgt   20  168024.013 ± 213.820    B/op
  * [info] DispatchBM.zio:·gc.churn.PS_Eden_Space                  avgt   20    1881.512 ±   8.124  MB/sec
  * [info] DispatchBM.zio:·gc.churn.PS_Eden_Space.norm             avgt   20  168059.337 ± 723.357    B/op
  * [info] DispatchBM.zio:·gc.churn.PS_Survivor_Space              avgt   20       0.097 ±   0.024  MB/sec
  * [info] DispatchBM.zio:·gc.churn.PS_Survivor_Space.norm         avgt   20       8.662 ±   2.110    B/op
  * [info] DispatchBM.zio:·gc.count                                avgt   20    2365.000            counts
  * [info] DispatchBM.zio:·gc.time                                 avgt   20    1708.000                ms
  *
  * [success] Total time: 2530 s, completed 24/05/2019 9:49:43 AM
  *
  * --------------------------------------------------------------------------------------------------------------------
  *
  * > sbt -DMODE=release
  * > benchmark-jvm/jmh:run -prof gc DispatchBM
  *
  * [info] # Run complete. Total time: 00:20:09
  * [info]
  * [info] Benchmark                                                 Mode  Cnt       Score     Error   Units
  * [info] DispatchBM.trampoline1                                   thrpt  200   97969.640 ± 605.218   ops/s
  * [info] DispatchBM.trampoline1:·gc.alloc.rate                    thrpt  200     899.342 ±   6.729  MB/sec
  * [info] DispatchBM.trampoline1:·gc.alloc.rate.norm               thrpt  200   14440.004 ±  43.401    B/op
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Eden_Space           thrpt  200     899.659 ±  10.765  MB/sec
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Eden_Space.norm      thrpt  200   14445.223 ± 141.924    B/op
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Old_Gen              thrpt  200       0.008 ±   0.026  MB/sec
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Old_Gen.norm         thrpt  200       0.129 ±   0.432    B/op
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Survivor_Space       thrpt  200       0.132 ±   0.010  MB/sec
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Survivor_Space.norm  thrpt  200       2.120 ±   0.157    B/op
  * [info] DispatchBM.trampoline1:·gc.count                         thrpt  200    2425.000            counts
  * [info] DispatchBM.trampoline1:·gc.time                          thrpt  200    1831.000                ms
  * [info] DispatchBM.trampoline2                                   thrpt  200  137408.488 ± 884.108   ops/s
  * [info] DispatchBM.trampoline2:·gc.alloc.rate                    thrpt  200    1365.302 ±   8.616  MB/sec
  * [info] DispatchBM.trampoline2:·gc.alloc.rate.norm               thrpt  200   15632.003 ±  34.095    B/op
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Eden_Space           thrpt  200    1365.104 ±  15.600  MB/sec
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Eden_Space.norm      thrpt  200   15630.585 ± 156.918    B/op
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Survivor_Space       thrpt  200       0.130 ±   0.010  MB/sec
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Survivor_Space.norm  thrpt  200       1.491 ±   0.117    B/op
  * [info] DispatchBM.trampoline2:·gc.count                         thrpt  200    2470.000            counts
  * [info] DispatchBM.trampoline2:·gc.time                          thrpt  200    1853.000                ms
  * [success] Total time: 1213 s, completed 16/07/2017 5:52:16 PM
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DispatchBM {
  import DispatchBM._

  def testF[F[_]](i: Interpreters[F])(f: Interpreters[F] => Request[Request[Unit]] => F[Response]): Any = {
    val d = f(i)
    DispatchRequests.map(r => i.run(d(Request(r.method, r.path, noBody, r.param, r.cookie, r))))
  }

  def test[F[_]](i: Interpreters[F]): Any =
    testF(i)(_.dispatcher)

  @Benchmark def catsIO     = test(DispatchBM.catsIO)
  @Benchmark def eval       = test(DispatchBM.eval)
  @Benchmark def trampoline = test(DispatchBM.trampoline)
  @Benchmark def zio        = test(DispatchBM.zio)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object DispatchBM {

  implicit val config = ServerLogicConfig(
    baseUrl                    = Url.Absolute.Base("https://test.shipreq.com"),
    staticAssetCdn             = Some(AssetManifest.StaticAssetCdn("https://static.shipreq.com")),
    publicRegistration         = Allow,
    applyEventThresholdMs      = 1000,
    googleAnalyticsTrackingId  = None,
    taskmanSchema              = "test_taskman",
    initTaskmanOnBoot          = false,
    initTaskmanRetry           = Retries.none,
    jaegerTracingConfig        = None,
    prometheus                 = ServerLogicConfig.Prometheus.default.copy(enabled = false),
    projectSpa                 = ProjectSpaLogic.Config.default,
    scalaJsManifest            = ScalaJsManifest("/1.js", "/2.js", "/3.js", "/4.js"),
    ssr                        = ServerLogicConfig.SsrConfig(enabled = false),
    security = ServerLogicConfig.Security(
      attackFrustrationDelay     = 1 hours,
      jwtCookieSecure            = false,
      jwtLifespan                = 24 hours,
      jwtSecret                  = new ServerLogicConfig.Security.JwtSecret("x"*64),
      jwtSecretPrevious          = None,
      passwordSaltLength         = 64,
      verificationTokenLength    = 8,
      registrationTokenLifespan  = 7 days,
      passwordResetTokenLifespan = 4 days))

  val noBody: Eval[Option[BinaryData]] = Eval.now(None)

  val user = User(UserId(1), Username("asds"))
  val ps = PasswordAndSalt(PasswordHash("wdsef34r"), Salt("32165498bdef"))

  final class Interpreters[F[_]](val run: F[_] => Any)(implicit val _F: Monad[F]) { self =>
    private val F = _F

    implicit object db extends DB.VerificationTokenReadOnly[F] with DB.ForSecurity[F] {
      var token = Option.empty[Instant]
      var getUserOk = true
      var projectOwner: Option[UserId] = Some(user.id)

      override def getUserRegistrationTokenIssueDate(t: VerificationToken)      = F point token
      override def getResetPasswordTokenIssueDate   (t: VerificationToken)      = F point token
      override def getUserAndPasswordByEmail        (e: EmailAddr)              = F.point(Option.when(getUserOk)((user, ps)))
      override def getUserAndPasswordByUsername     (u: Username)               = F.point(Option.when(getUserOk)((user, ps)))
      override def logLoginSuccess                  (i: UserId, ip: Option[IP]) = F.point(())
      override def getProjectOwner                  (id: ProjectId)             = F point projectOwner
    }

    implicit object security extends Security.Algebra[F] {
      override val F                                  = _F
      override val db                                 = self.db
      val delay                                       = F.point(())
      override def protect[A](vulnerable: F[A])       = delay >> vulnerable
      override def hashPassword(p: PlainTextPassword) = F point ps
      private val loggedInToken                       = Security.SessionRestoreResult.Success(Security.SessionToken.anonymous().login(user).copy(expiry = Instant.now()))
      private val anonToken                           = Security.SessionRestoreResult.Success(Security.SessionToken.anonymous().copy(expiry = Instant.now()))
      private val cookieName                          = Cookie.Name("S")

      override def attemptLogin(u: Username \/ EmailAddr, p: PlainTextPassword) = F.point {
        Option.when(u.fold(_ == user.username, _ => ???))(user)
      }
      override def sessionRestore(cookies: Cookie.LookupFn) = F.point {
        cookies(cookieName) match {
          case Some("1") => loggedInToken
          case Some(_)   => anonToken
          case None      => Security.SessionRestoreResult.None
        }
      }
      override def sessionPersist(token: Security.SessionToken[Any]) = F.point {
        val value = if (token.authenticatedUser.isEmpty) "" else "1"
        val cookie = Cookie(cookieName, value, None, None, None)
        Cookie.Update.add(cookie)
      }

      override def allowProjectAccess(requester: User, projectId: ProjectId, projectOwner: UserId) =
        Allow.when(requester.id ==* projectOwner)
    }

    implicit val svrSession: Server.Session[F] = new Server.Session[F] {
      override val clientIP: F[Option[IP]] = F.pure(None)
    }

    implicit val svrTime: Server.Time[F] = new Server.Time[F] {
      override val now = F point Instant.now()
      override def measureDuration[A](f: F[A]): F[(A, Duration)] =
        for {
          start <- now
          a     <- f
          end   <- now
        } yield (a, Duration.between(start, end))
      override def measureDuration_[A](f: F[A]): F[Duration] =
        for {
          start <- now
          _     <- f
          end   <- now
        } yield Duration.between(start, end)
    }

    implicit val metrics: MetricsAlgebra[F] =
      MetricsAlgebra.const(F.pure(()))

    implicit val trace: TraceAlgebra[F, Request[Unit], Response] =
      TraceAlgebra.off

    implicit object common extends CommonProtocolLogic[F] {
      override def attemptLoginUnprotected(id      : Username \/ EmailAddr,
                                           password: PlainTextPassword,
                                           session : Security.SessionToken[Any]) = ???
      override val ajaxLogin = _ => ???
      override val ajaxReportClientError = _ => ???
      override val ajaxSubmitFeedback = _ => ???
    }

    implicit object publicSpa extends PublicSpaLogic[F] {
      override def apiRegister1(emailAddr: String) = F.pure(\/-(TaskId(1000)))
      override val ajaxLandingPage    = _ => ???
      override val ajaxRegister1      = _ => ???
      override val ajaxRegister2      = _ => ???
      override val ajaxResetPassword1 = _ => ???
      override val ajaxResetPassword2 = _ => ???
    }

    implicit object homeSpa extends HomeSpaLogic.Ajax[F] {
      override val ajaxCreateProject = (_, _) => ???
      override val ajaxCreateUserGroup = (_, _) => ???
      override val ajaxUpdateUserGroup = (_, _) => ???
    }

    implicit object ops extends OpsEndpointLogic[F] {
      override def dbStats                                            = F.pure(null)
      override def userStats                                          = F.pure(null)
      override def taskmanMsgStatus(id: TaskId)                       = F.pure(null)
      override def sendMail(e: String)                                = F.pure(null)
      override def getProjectEvents(pid: ProjectId)                   = F.pure(null)
      override def createProject(a: Username \/ EmailAddr, b: String) = F.pure(null)
    }

    val sync = new Sync[F] {
      override def pure[A](x: A): F[A] =
        F.pure(x)

      override def raiseError[A](e: Throwable): F[A] =
        F.unit.map(_ => throw e)

      override def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
        ???

      override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] =
        ???

      override def bracketCase[A, B](acquire: F[A])(use: A => F[B])(release: (A, ExitCase[Throwable]) => F[Unit]): F[B] =
        ???

      override def suspend[A](thunk: => F[A]): F[A] =
        F.unit.flatMap(_ => thunk)
    }

    val dispatchLogic = {
      implicit val s = sync
      new DispatchLogic[F, Request[Unit]](r => Request(r.method, r.path, noBody, r.param, r.cookie, r))
    }

    val dispatcher = dispatchLogic.allLogic(testMode = false)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val DispatchRequests: List[Request[Unit]] = {
    import Method._
    // implicit def autoXID(p: ProjectId): ProjectId.Public = Obfuscators.projectId.obfuscate(p)
    val param: String => Option[String] = _ => None
    val cookie: Cookie.Name => Option[String] = _ => None
    val token = VerificationToken("MnVC8cvPX9b1jiCpyxoYLk4RqQ8idHlV4lf7OHzIQctHLgw6C")
    val b = List.newBuilder[Request[Unit]]
    b ++= Urls.PublicSpaRoute.static.whole.toList.map(r => Request(Get, r.url, noBody, param, cookie, ()))
    b ++= Urls.MemberRoute.static.whole.toList.map(r => Request(Get, r.url, noBody, param, cookie, ()))
    b ++= Urls.PublicSpaRoute.needsToken.whole.toList.map(r => Request(Get, r.url(token), noBody, param, cookie, ()))
//    b ++= (1 to 10).map(i => Request(Get, Urls.project(ProjectId(i)), param))
    val rs = b.result()
    List.fill(10)(rs).flatten
  }

  implicit val monadZIO: Monad[UIO] = new Monad[UIO] {
    override def pure[A](a: A): UIO[A] = UIO(a)
    override def flatMap[A, B](fa: UIO[A])(f: A => UIO[B]): UIO[B] = fa flatMap f
    override def map[A, B](fa: UIO[A])(f: A => B): UIO[B] = fa map f
    override def tailRecM[A, B](a: A)(f: A => UIO[Either[A,B]]): UIO[B] = ???
  }

  val zioRuntime = _root_.zio.Runtime.default

  val catsIO     = new Interpreters[IO        ](_.unsafeRunSync())
  val eval       = new Interpreters[Eval      ](_.value)
  val trampoline = new Interpreters[Trampoline](_.run)
  val zio        = new Interpreters[UIO       ](zioRuntime.unsafeRun(_))
}
