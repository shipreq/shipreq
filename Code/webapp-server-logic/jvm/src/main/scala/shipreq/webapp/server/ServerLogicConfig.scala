package shipreq.webapp.server

import io.jaegertracing.Configuration
import japgolly.clearconfig._
import java.nio.charset.Charset
import java.time.Duration
import monocle.macros.Lenses
import scalaz.syntax.applicative._
import shipreq.base.ops._
import shipreq.base.util.FxModule._
import shipreq.base.util._
import shipreq.webapp.server.logic.{DispatchLogic, ProjectSpaLogic}

@Lenses
final case class ServerLogicConfig(baseUrl: Url.Absolute.Base,

                                   /** Whether or not public registrations are allowed.
                                     * (Registration tokens already issued will still be accepted.)
                                     */
                                   publicRegistration: Permission,

                                   applyEventThresholdMs: Int,

                                   googleAnalyticsTrackingId: Option[String],

                                   /** The DB schema in which the Taskman interfaces reside. */
                                   taskmanSchema: String,

                                   initTaskmanOnBoot: Boolean,
                                   initTaskmanRetry: Retries,

                                   projectSpa: ProjectSpaLogic.Config,
                                   prometheus: ServerLogicConfig.Prometheus,
                                   security: ServerLogicConfig.Security,
                                   ssr: ServerLogicConfig.SsrConfig,
                                   jaegerTracingConfig: Option[Configuration]) {

  lazy val traceAlgebraFx: Trace.Algebra[Fx] =
    Trace.Algebra(
      jaegerTracingConfig.map(c => OpenTracing.algebraFx(c.getTracer)).toList)
}

object ServerLogicConfig {

  @Lenses
  final case class Security(/** A short amount of time, unnoticeable to humans, to sleep in order to
                              * frustrate automated security attacks.
                              *
                              * The http response time metrics histogram buckets have been crafted around
                              * this delay being the default 120ms so only reconfigure this in unit tests.
                              */
                            attackFrustrationDelay: Duration,

                            /** Whether to set the secure flag on JWT cookies */
                            jwtCookieSecure: Boolean,

                            /** How long before a JWT expires */
                            jwtLifespan: Duration,

                            /** Secret by which JWTs are signed and verified */
                            jwtSecret: Security.JwtSecret,

                            /** Additional JWT secret for verification only */
                            jwtSecretPrevious: Option[Security.JwtSecret],

                            /** Number of bytes that will comprise the salt for new passwords */
                            passwordSaltLength: Int,

                            /** Number of characters in tokens used for email & reset-password verification. */
                            verificationTokenLength: Int,

                            /** How long registration tokens are valid for after issuing. */
                            registrationTokenLifespan: Duration,

                            /** How long password-reset tokens are valid for after issuing. */
                            passwordResetTokenLifespan: Duration) {

    val attackFrustrationDelayMs = attackFrustrationDelay.toMillis
    val jwtCookieHttpOnlySome    = Some(true)
    val jwtCookieMaxAgeInSecSome = Some((jwtLifespan.toMillis / 1000).toInt)
    val jwtCookieSecureSome      = Some(jwtCookieSecure)
    val jwtLifespanMs            = jwtLifespan.toMillis
  }

  object Security {

    final class JwtSecret(val string: String) {
      val bytes: Array[Byte] =
        string.getBytes(Charset.forName("ASCII"))

      assert(bytes.length == string.length,
        "Non-ASCII not permitted in JWT secrets: " + string.iterator.map(_.toInt).mkString("[", ",", "]"))
    }

    object JwtSecret {
      val MinLen = 64 // hs256=32, hs512=64, hs768=96
      private def minLenErrMsg = s"Must be at least ${MinLen} chars."

      implicit def configValueParser: ConfigValueParser[JwtSecret] =
        ConfigValueParser.id
          .ensure(_.length >= MinLen, minLenErrMsg)
          .map(new JwtSecret(_))
    }

    def config: ConfigDef[Security] =
      ( ConfigDef.getOrUse[Duration ]("attack_frustration_delay", Duration.ofMillis(120)) |@|
        ConfigDef.need    [Boolean  ]("jwt.cookie.secure") |@|
        ConfigDef.need    [Duration ]("jwt.lifespan") |@|
        ConfigDef.need    [JwtSecret]("jwt.secret") |@|
        ConfigDef.get     [JwtSecret]("jwt.secret.previous") |@|
        ConfigDef.getOrUse[Int      ]("password.salt", 64) |@|
        ConfigDef.need    [Int      ]("verification_token.length") |@|
        ConfigDef.need    [Duration ]("verification_token.lifespan.register") |@|
        ConfigDef.need    [Duration ]("verification_token.lifespan.resetpw")
      ) (apply)
  }

  final case class Prometheus(enabled    : Boolean,
                              hikaricp   : Boolean,
                              hotspot    : Boolean,
                              jdbc       : Boolean,
                              path       : String,
                              bearerToken: Option[String])
  object Prometheus {
    val default = apply(
      enabled     = true,
      hikaricp    = true,
      hotspot     = true,
      jdbc        = true,
      path        = (DispatchLogic.opsRoot / "metrics").relativeUrl,
      bearerToken = None)

    def config: ConfigDef[Prometheus] =
      ( ConfigDef.getOrUse[Boolean]("enabled"    , default.enabled) |@|
        ConfigDef.getOrUse[Boolean]("hikaricp"   , default.hikaricp) |@|
        ConfigDef.getOrUse[Boolean]("hotspot"    , default.hotspot) |@|
        ConfigDef.getOrUse[Boolean]("jdbc"       , default.jdbc) |@|
        ConfigDef.getOrUse[String ]("path"       , default.path).map(_.replaceFirst("^/*", "/")) |@|
        ConfigDef.get     [String ]("bearerToken")
    ) (apply)
  }

  final case class SsrConfig(enabled: Boolean)

  object SsrConfig {
    def config: ConfigDef[SsrConfig] =
      ConfigDef.getOrUse("enabled", true).map(apply)
  }

  def config: ConfigDef[ServerLogicConfig] =
    JaegerTracingConfig.external *>
    ( ConfigDef.need       [String  ]       ("url").map(Url.Absolute.Base.apply) |@|
      ConfigDef.getOrUse   [Boolean ]       ("feature.publicRegistration", true).map(Allow.when) |@|
      ConfigDef.getOrUse   [Int     ]       ("applyEvent.thresholdMs", 200).ensure_>=(0).ensure_<(1000) |@|
      ConfigDef.get        [String  ]       ("googleAnalytics.trackingId") |@|
      ConfigDef.need       [String  ]       ("taskman.schema") |@|
      ConfigDef.getOrUse   [Boolean ]       ("taskman.init", true) |@|
      RetriesJvm.config.withPrefix          ("taskman.init.retry.") |@|
      ProjectSpaLogic.Config.defn.withPrefix("projectSpa.") |@|
      Prometheus.config.withPrefix          ("prometheus.") |@|
      Security.config.withPrefix            ("security.") |@|
      SsrConfig.config.withPrefix           ("ssr.") |@|
      JaegerTracingConfig.main              ("webapp")
  ) (apply)
      .withPrefix("shipreq.")

}