package shipreq.webapp.server

import japgolly.clearconfig._
import java.time.Duration
import monocle.macros.Lenses
import scalaz.syntax.applicative._
import shipreq.base.ops._
import shipreq.base.util._
import shipreq.base.util.FxModule._
import shipreq.webapp.server.logic.DispatchLogic

@Lenses
final case class ServerConfig(baseUrl: Url.Absolute.Base,

                              /** A short amount of time, unnoticeable to humans, to sleep in order to
                                * frustrate automated security attacks.
                                *
                                * The http response time metrics histogram buckets have been crafted around
                                * this delay being the default 120ms so only reconfigure this in unit tests.
                                */
                              attackFrustrationDelay: Duration,

                              /** Number of characters in tokens used for email & reset-password verification. */
                              securityTokenLength: Int,

                              /** How long registration tokens are valid for after issuing. */
                              registrationTokenLifespan: Duration,

                              /** How long password-reset tokens are valid for after issuing. */
                              passwordResetTokenLifespan: Duration,

                              /** Whether or not public registrations are allowed.
                                * (Registration tokens already issued will still be accepted.)
                                */
                              publicRegistration: Permission,

                              googleAnalyticsTrackingId: Option[String],

                              /** Filename or classpath-resource-name of Kamon config */
                              kamonConfFile: Option[String],

                              prometheus: ServerConfig.Prometheus,

                              ssrEnabled: Boolean,

                              /** The DB schema in which the Taskman interfaces reside. */
                              taskmanSchema: String,

                              initTaskmanOnBoot: Boolean,
                              initTaskmanRetry: RetryCriteria) {

  val attackFrustrationDelayMs: Long =
    attackFrustrationDelay.toMillis

  lazy val traceAlgebraFx: Trace.Algebra[Fx] =
    Trace.Algebra(
      kamonConfFile.map(_ => TraceWithKamon.algebraFx).toList)
}

object ServerConfig {

  final case class Prometheus(enabled: Boolean,
                              hikaricp: Boolean,
                              hotspot: Boolean,
                              jdbc: Boolean,
                              ssr: Boolean,
                              path: String)
  object Prometheus {
    val default = apply(
      enabled = true,
      hikaricp = true,
      hotspot = true,
      jdbc = true,
      ssr = true,
      path = (DispatchLogic.opsRoot / "metrics").relativeUrl)

    def config: ConfigDef[Prometheus] =
      ( ConfigDef.getOrUse[Boolean]("enabled",  default.enabled) |@|
        ConfigDef.getOrUse[Boolean]("hikaricp", default.hikaricp) |@|
        ConfigDef.getOrUse[Boolean]("hotspot",  default.hotspot) |@|
        ConfigDef.getOrUse[Boolean]("jdbc",     default.hotspot) |@|
        ConfigDef.getOrUse[Boolean]("ssr",      default.ssr) |@|
        ConfigDef.getOrUse[String ]("path",     default.path).map(_.replaceFirst("^/*", "/"))
    ) (apply)
  }

  def config: ConfigDef[ServerConfig] =
    ((ConfigDef.need    [String  ]("url").map(Url.Absolute.Base.apply) |@|
      ConfigDef.getOrUse[Duration]("attack_frustration_delay", Duration.ofMillis(120)) |@|
      ConfigDef.need    [Int     ]("token.length") |@|
      ConfigDef.need    [Duration]("token.lifespan.register") |@|
      ConfigDef.need    [Duration]("token.lifespan.resetpw") |@|
      ConfigDef.getOrUse[Boolean ]("feature.publicRegistration", true).map(Allow.when) |@|
      ConfigDef.get     [String  ]("googleAnalytics.trackingId")).tupled |@| (
      ConfigDef.get     [String  ]("kamon.conf") |@|
      Prometheus.config.withPrefix("prometheus.") |@|
      ConfigDef.getOrUse[Boolean ]("ssr", true) |@|
      ConfigDef.need    [String  ]("taskman.schema") |@|
      ConfigDef.getOrUse[Boolean ]("taskman.init", true) |@|
      RetryCriteria.config.withPrefix("taskman.init.retry.")
    ).tupled) {
      case ((
          baseUrl,
          attackFrustrationDelay,
          securityTokenLength,
          registrationTokenLifespan,
          passwordResetTokenLifespan,
          publicRegistration,
          googleAnalyticsTrackingId), (
          kamonConfFile,
          prometheus,
          ssrEnabled,
          taskmanSchema,
          initTaskmanOnBoot,
          initTaskmanRetry,
        )) =>
        apply(
          baseUrl                    = baseUrl,
          attackFrustrationDelay     = attackFrustrationDelay,
          securityTokenLength        = securityTokenLength,
          registrationTokenLifespan  = registrationTokenLifespan,
          passwordResetTokenLifespan = passwordResetTokenLifespan,
          publicRegistration         = publicRegistration,
          googleAnalyticsTrackingId  = googleAnalyticsTrackingId,
          kamonConfFile              = kamonConfFile,
          prometheus                 = prometheus,
          ssrEnabled                 = ssrEnabled,
          taskmanSchema              = taskmanSchema,
          initTaskmanOnBoot          = initTaskmanOnBoot,
          initTaskmanRetry           = initTaskmanRetry,
        )
    }.withPrefix("shipreq.")

}