package shipreq.benchmark

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import monix.eval.Coeval
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scalaz.effect.IO
import scalaz.Free.Trampoline
import scalaz.std.function.function0Instance
import scalaz.syntax.monad._
import scalaz.{Monad, Name, \/, \/-}
import shipreq.base.util._
import shipreq.taskman.api.MsgId
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.{ProjectId, SecurityToken}
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.logic._
import DispatchLogic._

/**
  * > sbt
  * > benchmark-jvm/jmh:run -wi 10 -i 10 -f 2 -prof gc DispatchBM
  *
  * [info] Benchmark                                                 Mode  Cnt       Score      Error   Units
  * [info] DispatchBM.trampoline1                                   thrpt   20  106079.139 ± 1666.218   ops/s
  * [info] DispatchBM.trampoline1:·gc.alloc.rate                    thrpt   20    1096.726 ±   21.104  MB/sec
  * [info] DispatchBM.trampoline1:·gc.alloc.rate.norm               thrpt   20   16264.016 ±   71.273    B/op
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Eden_Space           thrpt   20    1117.146 ±  175.057  MB/sec
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Eden_Space.norm      thrpt   20   16575.028 ± 2624.602    B/op
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Survivor_Space       thrpt   20       0.061 ±    0.028  MB/sec
  * [info] DispatchBM.trampoline1:·gc.churn.PS_Survivor_Space.norm  thrpt   20       0.911 ±    0.423    B/op
  * [info] DispatchBM.trampoline1:·gc.count                         thrpt   20      46.000             counts
  * [info] DispatchBM.trampoline1:·gc.time                          thrpt   20      38.000                 ms
  * [info] DispatchBM.trampoline2                                   thrpt   20  148806.597 ± 1762.087   ops/s
  * [info] DispatchBM.trampoline2:·gc.alloc.rate                    thrpt   20    1368.225 ±   43.565  MB/sec
  * [info] DispatchBM.trampoline2:·gc.alloc.rate.norm               thrpt   20   14464.011 ±  392.002    B/op
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Eden_Space           thrpt   20    1394.017 ±  139.084  MB/sec
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Eden_Space.norm      thrpt   20   14735.675 ± 1445.064    B/op
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Survivor_Space       thrpt   20       0.074 ±    0.028  MB/sec
  * [info] DispatchBM.trampoline2:·gc.churn.PS_Survivor_Space.norm  thrpt   20       0.783 ±    0.298    B/op
  * [info] DispatchBM.trampoline2:·gc.count                         thrpt   20      98.000             counts
  * [info] DispatchBM.trampoline2:·gc.time                          thrpt   20      79.000                 ms
  * [success] Total time: 124 s, completed 16/07/2017 7:18:46 PM
  *
  * > sbt -DMODE=release
  * root> benchmark-jvm/jmh:run -prof gc DispatchBM
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

//  def test[F[_]](i: Interpreters[F]): Any =
//    DispatchRequests.map(r => i.run(i.dispatcher(r)))
//
//  @Benchmark def coeval     = test(DispatchBM.coeval)
//  @Benchmark def io         = test(DispatchBM.io)
//  @Benchmark def fn0        = test(DispatchBM.fn0)
////@Benchmark def future     = test(DispatchBM.future)
//  @Benchmark def name       = test(DispatchBM.name)
//  @Benchmark def trampoline = test(DispatchBM.trampoline)

  def test[F[_]](i: Interpreters[F])(f: Interpreters[F] => Request[Request[Unit]] => F[Response]): Any = {
    val d = f(i)
    DispatchRequests.map(r => i.run(d(Request(r.method, r.path, r.param, r))))
  }

  @Benchmark def trampoline1 = test(DispatchBM.trampoline)(_.dispatcher1)
//  @Benchmark def trampoline2 = test(DispatchBM.trampoline)(_.dispatcher2)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object DispatchBM {
  import JavaTimeHelpers._

  implicit val config = ServerConfig(
    baseUrl                    = Url.Absolute.Base("https://test.shipreq.com"),
    attackFrustrationDelay     = 1 hours,
    securityTokenLength        = 8,
    registrationTokenLifespan  = 7 days,
    passwordResetTokenLifespan = 4 days,
    publicRegistration         = Allow,
    googleAnalyticsTrackingId  = None,
    taskmanSchema              = "test_taskman",
    prometheus                 = ServerConfig.Prometheus.default.copy(enabled = false),
    kamonConfFile              = None,
    initTaskmanOnBoot          = false,
    initTaskmanRetry           = RetryCriteria(2 hours, Some(666)))

  val user = User(UserId(1), Username("asds"), EmailAddr("x@x.com"), Set.empty)
  val ps = PasswordAndSalt(PasswordHash("wdsef34r"), Salt("32165498bdef"))

  final class Interpreters[F[_]](val run: F[_] => Any)(implicit val F: Monad[F]) {
    val self = this

    implicit object db extends DB.SecurityTokenReadOnly[F] with DB.ForSecurity[F] {
      var token = Option.empty[Instant]
      var getUserOk = true
      var projectOwner: Option[UserId] = Some(user.id)

      override def getUserRegistrationTokenIssueDate(t: SecurityToken)          = F point token
      override def getResetPasswordTokenIssueDate   (t: SecurityToken)          = F point token
      override def getUserAndPasswordByEmail        (e: EmailAddr)              = F.point(Option.when(getUserOk)((user, ps)))
      override def getUserAndPasswordByUsername     (u: Username)               = F.point(Option.when(getUserOk)((user, ps)))
      override def logLoginSuccess                  (i: UserId, ip: Option[IP]) = F.point(())
      override def getProjectOwner                  (id: ProjectId)             = F point projectOwner
    }

    implicit object security extends Security.Algebra[F] {
      var loginSuccess = true
      var loggedIn = Option.empty[User]

      override val db                                 = self.db
      val delay                                       = F.point(())
      override def protect[A](vulnerable: F[A])       = delay >> vulnerable
      override def hashPassword(p: PlainTextPassword) = F point ps
      override val isAuthenticated                    = F.point(loggedIn.isDefined)
      override val authenticatedUser                  = F.point(loggedIn)
      override val logout                             = F.point{loggedIn = None}

      override def attemptLogin(u: \/[Username, EmailAddr], p: PlainTextPassword) =
        F.point { loggedIn = Option.when(loginSuccess)(user); loggedIn }
    }

    implicit val svrSession: Server.Session[F] = new Server.Session[F] {
      override val clientIP: F[Option[IP]] = F.pure(None)
      override val sessionId: F[Option[SessionId]] = F.pure(None)
    }

    implicit val svrTime: Server.Time[F] = new Server.Time[F] {
      override val now = F point Instant.now()
      override def measureDuration[A](f: F[A]): F[(A, Duration)] =
        for {
          start <- now
          a     <- f
          end   <- now
        } yield (a, Duration.between(start, end))
    }

    implicit val metrics: MetricsLogic[F] =
      MetricsLogic.const(F.pure(()))

    implicit val trace: TraceLogic[F, Request[Unit], Response] =
      TraceLogic.off

    implicit val publicApi: PublicSpaLogic.ForApi[F] =
      _ => F.pure(\/-(MsgId(1000)))

    implicit object ops extends OpsEndpoints[F] {
      override def dbStats                           = F.pure(null)
      override def userStats                         = F.pure(null)
      override def taskmanMsgStatus(id: MsgId)       = F.pure(null)
      override def sendMail(e: String)               = F.pure(null)
    }

    val dispatchLogic = new DispatchLogic[F, Request[Unit], Response](
      r => Request(r.method, r.path, r.param, r), (_, r) => F.point(r))

    val dispatcher1 = dispatchLogic.Main.routes.withFallback(dispatchLogic.Main.fallback)
//    val dispatcher2 = dispatchLogic.Main.cacheUsualPaths(dispatcher1)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val DispatchRequests: List[Request[Unit]] = {
    import Method._
    implicit def autoXID(p: ProjectId): ProjectId.Public = Obfuscators.projectId.obfuscate(p)
    val param: String => Option[String] = _ => None
    val token = SecurityToken("MnVC8cvPX9b1jiCpyxoYLk4RqQ8idHlV4lf7OHzIQctHLgw6C")
    val b = List.newBuilder[Request[Unit]]
    b ++= Urls.PublicSpaRoute.static.whole.toList.map(r => Request(Get, r.url, param, ()))
    b ++= Urls.MemberRoute.static.whole.toList.map(r => Request(Get, r.url, param, ()))
    b ++= Urls.PublicSpaRoute.needsToken.whole.toList.map(r => Request(Get, r.url(token), param, ()))
//    b ++= (1 to 10).map(i => Request(Get, Urls.project(ProjectId(i)), param))
    val rs = b.result()
    List.fill(10)(rs).flatten
  }

  implicit val monadCoeval: Monad[Coeval] = new Monad[Coeval] {
    override def point[A](a: => A): Coeval[A] = Coeval(a)
    override def bind[A, B](fa: Coeval[A])(f: (A) => Coeval[B]): Coeval[B] = fa flatMap f
    override def map[A, B](fa: Coeval[A])(f: (A) => B): Coeval[B] = fa map f
  }

  implicit val monadFuture: Monad[Future] = new Monad[Future] {
    import scala.concurrent.ExecutionContext.Implicits.global
    override def point[A](a: => A): Future[A] = Future(a)
    override def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa flatMap f
    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa map f
  }

  val coeval     = new Interpreters[Coeval    ](_.apply())
  val io         = new Interpreters[IO        ](_.unsafePerformIO())
  val fn0        = new Interpreters[Function0 ](_.apply())
  val future     = new Interpreters[Future    ](Await.result(_, FiniteDuration(5, "min")))
  val name       = new Interpreters[Name      ](_.value)
  val trampoline = new Interpreters[Trampoline](_.run)
}
