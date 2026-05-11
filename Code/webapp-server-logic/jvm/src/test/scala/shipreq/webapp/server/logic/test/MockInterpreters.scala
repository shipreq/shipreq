package shipreq.webapp.server.logic.test

import cats.Eval
import cats.arrow.FunctionK
import cats.effect.{ExitCase, Sync}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.{Duration, Instant}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.config.{ScalaJsManifest, ServerLogicConfig}
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.event.ApplyEventAlgebra
import shipreq.webapp.server.logic.inmem._
import shipreq.webapp.server.logic.logic._

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
      verificationTokenLength    = 8,
      registrationTokenLifespan  = 7 days,
      passwordResetTokenLifespan = 4 days))

  implicit val syncEval: Sync[Eval] =
    new Sync[Eval] {

      override def pure[A](a: A): Eval[A] =
        Eval.now(a)

      override def raiseError[A](e: Throwable): Eval[A] =
        Eval.always(throw e)

      override def handleErrorWith[A](fa: Eval[A])(f: Throwable => Eval[A]): Eval[A] =
        Eval.always {
          try
            fa.value
          catch {
            case t: Throwable => f(t).value
          }
        }

      override def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] =
        fa flatMap f

      override def tailRecM[A, B](z: A)(f: A => Eval[Either[A,B]]): Eval[B] =
        Eval.always {
          @tailrec
          def go(a: A): B =
            f(a).value match {
              case -\/(a2) => go(a2)
              case \/-(b) => b
            }
          go(z)
        }

      override def bracketCase[A, B](acquire: Eval[A])(use: A => Eval[B])(release: (A, ExitCase[Throwable]) => Eval[Unit]): Eval[B] =
        acquire.flatMap { a =>
          Eval.always {
            val result: Throwable \/ B =
              try
                \/-(use(a).value)
              catch {
                case t: Throwable => -\/(t)
              }
            release(a, ExitCase.attempt(result)).value
            result match {
              case \/-(b) => b
              case -\/(e) => throw e
            }
          }
        }

      override def suspend[A](thunk: => Eval[A]): Eval[A] =
        Eval.defer(thunk)
    }
}

class MockInterpreters(modCfg         : ServerLogicConfig => ServerLogicConfig = Identity[ServerLogicConfig],
                       specificMockDb : Option[MockDb]                         = None,
                       specificRedis  : Option[InMemoryRedis[Eval]]            = None,
                       specificTaskman: Option[MockTaskman]                    = None,
                      ) {

  implicit def syncEval: Sync[Eval] = MockInterpreters.syncEval

  implicit val config         = modCfg(MockInterpreters.config)
  implicit val assetManifest  = config.assetManifest
  implicit val sjsManifest    = config.scalaJsManifest
  implicit val crypto         = new MockCrypto
  implicit val svr            = new MockServer[Eval]
  implicit val db             = specificMockDb.getOrElse(new MockDb(svr.now))
  implicit val security       = new MockSecurity(db, svr.now, config.security)
  implicit val taskman        = specificTaskman.getOrElse(new MockTaskman)
  implicit val nameToName     = FunctionK.id[Eval]
  implicit val apEvent        = ApplyEventAlgebra.trusted[Eval]
  implicit val metrics        = MetricsAlgebra.const(Eval.Unit)
  implicit val trace          = Trace.Algebra.off[Eval]
  implicit val redis          = specificRedis.getOrElse(new InMemoryRedis[Eval])
  implicit val common         = CommonProtocolLogic[Eval]
  implicit val publicSpa      = PublicSpaLogic[Eval, Eval]
  implicit val homeSpa        = HomeSpaLogic[Eval, Eval]
  implicit val projectSpa     = ProjectSpaLogic[Eval, Eval](config.projectSpa)

  implicit object ops extends OpsEndpointLogic.Base[Eval] {
    override val randomToken = Eval.now("blah")
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
