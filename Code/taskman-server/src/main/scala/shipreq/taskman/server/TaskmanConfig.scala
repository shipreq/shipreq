package shipreq.taskman.server

import japgolly.clearconfig._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import javax.mail.Session
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.applicative._
import shipreq.base.util.{Retries, RetriesJvm}
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.server.business._
import shipreq.taskman.server.business.JavaMail.ConfigValueParsers._
import shipreq.taskman.server.business.FreshDesk.ConfigValueParsers._
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.business._

final case class TaskmanConfig(mail      : TaskmanConfig.Mail,
                               mailchimp : MailChimp.Props,
                               freshdesk : FreshDesk.Props,
                               prometheus: TaskmanConfig.Prometheus,
                               shipreq   : TaskmanConfig.ShipReq,
                               taskman   : TaskmanConfig.Taskman)

object TaskmanConfig extends HasLogger {

  def config: ConfigDef[TaskmanConfig] =
    (mail |@| mailchimp |@| freshdesk |@| prometheus |@| shipreq |@| taskman) (apply)

  def mailTokens: ConfigDef[Email.TokenValues] =
    (ConfigDef.need[String](CfgKeys.Webapp.appName)
      |@| ConfigDef.need[String](CfgKeys.Webapp.loginUrl)
      ) (Email.TokenValues)

  // TODO Put props and parsers in Business classes

  // ===================================================================================================================

  final case class Mail(publicFrom    : Email.Addr,
                        archiveAddrs  : List[Email.Addr],
                        mechanism     : TaskmanConfig.JavaMail \/ MailGun.Props,
                        concurrencyMax: Int) {
    def envelopeProps = Email.EnvelopeProps(publicFrom, archiveAddrs)
  }

  def mail: ConfigDef[Mail] =
    ((ConfigDef.need[Email.Addr]("public.from")
      |@| ConfigDef.getOrUse[List[Email.Addr]]("archive.to", Nil)
      |@| ConfigDef.need[Int]("concurrency.max").ensure(_ >= 1, "Must be ≥ 1.")
      ).tupled.withPrefix("mail.")
      |@| mailMechanism) { case ((a, b, c), m) => Mail(a, b, m, c) }

  def mailMechanism: ConfigDef[TaskmanConfig.JavaMail \/ MailGun.Props] =
    ConfigDef.need[String]("mail.via").map(_.toLowerCase).chooseAttempt {
      case "javamail" => \/-(javaMail.map(-\/(_)))
      case "mailgun"  => \/-(mailGun.map(\/-(_)))
      case _          => -\/("Legal values are [JavaMail, MailGun].")
    }

  final case class JavaMail(sessionFn: () => Session)

  def javaMail: ConfigDef[JavaMail] =
    JavaMailConfig.sessionFn.map(JavaMail.apply)

  def mailGun: ConfigDef[MailGun.Props] =
    (ConfigDef.need[String]("domain")
      |@| ConfigDef.need[String]("key").secret
      ) (MailGun.Props.apply)
      .withPrefix("mailgun.")

  // ===================================================================================================================

  def mailchimp: ConfigDef[MailChimp.Props] =
    (ConfigDef.need[String]("dc")
      |@| ConfigDef.need[String]("key").secret.map(MailChimp.ApiKey)
      |@| ConfigDef.need[String]("masterList")
      ) (MailChimp.Props)
      .withPrefix("mailchimp.")

  // ===================================================================================================================

  def freshdesk: ConfigDef[FreshDesk.Props] =
    (ConfigDef.need[String]("domain")
      |@| ConfigDef.need[String]("key").secret
      |@| ConfigDef.need[String]("taskmanEmail")
      |@| ConfigDef.need[FreshDesk.UnverifiedTicketOrg]("org.landingPage")
      |@| ConfigDef.need[FreshDesk.UnverifiedTicketOrg]("org.failure")
      ) (FreshDesk.Props)
      .withPrefix("freshdesk.")

  // ===================================================================================================================

  final case class Prometheus(enabled: Boolean,
                              hotspot: Boolean)
  object Prometheus {
    val default = apply(
      enabled = true,
      hotspot = true)
  }

  def prometheus: ConfigDef[Prometheus] =
    ( ConfigDef.getOrUse[Boolean]("enabled", Prometheus.default.enabled) |@|
      ConfigDef.getOrUse[Boolean]("hotspot", Prometheus.default.hotspot)
    ) (Prometheus.apply)
      .withPrefix("prometheus.")

  // ===================================================================================================================

  final case class ShipReq(schema: Option[String])

  def shipreq: ConfigDef[ShipReq] =
    ConfigDef.get[String]("schema")
      .map(ShipReq)
      .withPrefix("shipreq.")

  // ===================================================================================================================

  /**
    * @param healthFile A file that will be touched very regularly so that the health of the system is externally
    *                   observable.
    */
  final case class Taskman(remoteCfgRetry: Retries,
                           queueSize     : Int,
                           trustPeriod   : AssignmentTrustPeriod,
                           pollEvery     : Duration,
                           pollGap       : Duration,
                           healthFile    : Option[String])

  def taskman: ConfigDef[Taskman] =
    (RetriesJvm.config.withPrefix("remoteCfg.retry.")
      |@| ConfigDef.need[Int]("queueSize").ensure(_ >= 1, "Must be ≥ 1.")
      |@| ConfigDef.need[Duration]("trustPeriod").ensure(!_.isShorterThan(10 seconds), "Must be at least 10 seconds.")
      |@| ConfigDef.need[Duration]("poll.every").ensure(!_.isShorterThan(50 millis), "Must be at least 50 ms.")
      |@| ConfigDef.get[Duration]("poll.gap").ensure(_.fold(true)(!_.isShorterThan(50 millis)), "Must be at least 50 ms.")
      |@| ConfigDef.get[String]("healthFile")
      ) { (remoteCfgRetry, qs, tp, pollEvery, pollGapO, healthFile) =>
      val pollGap = pollGapO getOrElse pollEvery
      if (pollGap isLongerThan pollEvery)
        logger.warn(s"The minimum poll gap ($pollGap) is larger than the poll time ($pollEvery). Wasteful.")
      Taskman(remoteCfgRetry, qs, AssignmentTrustPeriod(tp), pollEvery, pollGap, healthFile)
    }
      .withPrefix("taskman.")
}
