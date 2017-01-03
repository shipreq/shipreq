package shipreq.webapp.server

import japgolly.microlibs.config._
import java.time.Duration
import scalaz.syntax.applicative._
import shipreq.webapp.server.util.{CachePolicy, ExpireAfter}
import ConfigParser.Implicits.Defaults._
import JavaTimeConfigParsers._

final case class ServerConfig(

  supportEmailAddress: String,

  baseUrl: String,

  /** A short amount of time, unnoticeable to humans, to sleep in order to frustrate automated security attacks. */
  attackFrustrationDelay: Duration,

  /** Number of characters in tokens used for email & reset-password verification. */
  confirmationTokenLength: Int,

  /** The DB schema in which the Taskman interfaces reside. */
  taskmanSchema: String,

  /** How long confirmation tokens are valid for after issuing. */
  tokenLifespan: Duration,

  /** How long password-reset tokens are valid for after issuing. */
  passwordResetTokenLifespan: Duration,

  /**
    * Whether or not new registrations are allowed.
    * (Registration tokens already issued will still be accepted.)
    */
  allowRegister: Boolean,

  /** Maximum time a flash variable will be retained. (default) */
  flashVarTTL: Duration,

  quoteCachePolicy: CachePolicy[Any]) {

  val attackFrustrationDelayMs: Long =
    attackFrustrationDelay.toMillis
}

object ServerConfig {

  def config: Config[ServerConfig] =
    ( Config.need[String]("support.email") |@|
      Config.need[String]("url") |@|
      Config.need[Duration]("attack_frustration_delay") |@|
      Config.need[Int]("token.length") |@|
      Config.need[String]("taskman.schema") |@|
      Config.need[Duration]("token.lifespan.email_conf") |@|
      Config.need[Duration]("token.lifespan.resetpw") |@|
      Config.getOrUse[Boolean]("allow.register", true) |@|
      Duration.ofMinutes(12).pure[Config] |@|
      ExpireAfter(Duration ofMinutes 30).pure[Config]
    ) (apply).withPrefix("shipreq.")

}