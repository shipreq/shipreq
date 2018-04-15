package shipreq.webapp.server

import japgolly.microlibs.config._
import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config.JavaTimeConfigParsers._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import monocle.macros.Lenses
import scalaz.syntax.applicative._
import shipreq.base.ops._
import shipreq.base.util._
import shipreq.base.util.FxModule._

@Lenses
final case class ServerConfig(baseUrl: Url.Absolute.Base,

                              /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
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

  final case class Prometheus(enabled: Boolean, hotspot: Boolean, path: String)
  object Prometheus {
    def config: Config[Prometheus] =
      ( Config.getOrUse[Boolean]("enabled", true) |@|
        Config.getOrUse[Boolean]("hotspot", true) |@|
        Config.getOrUse[String ]("path", "/ops/metrics").map(_.replaceFirst("^/*", "/"))
    ) (apply)
  }

  def config: Config[ServerConfig] =
    ( Config.need    [String  ]      ("url").map(Url.Absolute.Base.apply) |@|
      Config.need    [Duration]      ("attack_frustration_delay") |@|
      Config.need    [Int     ]      ("token.length") |@|
      Config.need    [Duration]      ("token.lifespan.register") |@|
      Config.need    [Duration]      ("token.lifespan.resetpw") |@|
      Config.getOrUse[Boolean ]      ("feature.publicRegistration", true).map(Allow.when) |@|
      Config.get     [String  ]      ("googleAnalytics.trackingId") |@|
      Config.get     [String  ]      ("kamon.conf") |@|
      Prometheus.config.withPrefix   ("prometheus.") |@|
      Config.need    [String  ]      ("taskman.schema") |@|
      Config.getOrUse[Boolean ]      ("taskman.init", true) |@|
      RetryCriteria.config.withPrefix("taskman.init.retry.")
    ) (apply).withPrefix("shipreq.")

}