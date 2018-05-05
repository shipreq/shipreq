package shipreq.taskman.server

import japgolly.microlibs.config.ConfigParser.Implicits.Defaults._
import japgolly.microlibs.config.JavaTimeConfigParsers._
import japgolly.microlibs.config._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import javax.mail.Session
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.applicative._
import shipreq.base.util.JavaTimeHelpers._
import shipreq.base.util.RetryCriteria
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.server.business._
import shipreq.taskman.server.business.JavaMail.ConfigParsers._
import shipreq.taskman.server.business.FreshDesk.ConfigParsers._
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.business._

final case class TaskmanConfig(mail     : TaskmanConfig.Mail,
                               mailchimp: MailChimp.Props,
                               freshdesk: FreshDesk.Props,
                               shipreq  : TaskmanConfig.ShipReq,
                               taskman  : TaskmanConfig.Taskman)

object TaskmanConfig extends HasLogger {

  def config: Config[TaskmanConfig] =
    (mail |@| mailchimp |@| freshdesk |@| shipreq |@| taskman) (apply)

  def mailTokens: Config[Email.TokenValues] =
    (Config.need[String](CfgKeys.Webapp.appName)
      |@| Config.need[String](CfgKeys.Webapp.loginUrl)
      ) (Email.TokenValues)

  // TODO Put props and parsers in Business classes

  // ===================================================================================================================

  case class Mail(publicFrom    : Email.Addr,
                  archiveAddrs  : List[Email.Addr],
                  mechanism     : TaskmanConfig.JavaMail \/ MailGun.Props,
                  concurrencyMax: Int) {
    def envelopeProps = Email.EnvelopeProps(publicFrom, archiveAddrs)
  }

  def mail: Config[Mail] =
    ((Config.need[Email.Addr]("public.from")
      |@| Config.getOrUse[List[Email.Addr]]("archive.to", Nil)
      |@| Config.need[Int]("concurrency.max").ensure(_ >= 1, "Must be ≥ 1.")
      ).tupled.withPrefix("mail.")
      |@| mailMechanism) { case ((a, b, c), m) => Mail(a, b, m, c) }

  def mailMechanism: Config[TaskmanConfig.JavaMail \/ MailGun.Props] =
    Config.need[String]("mail.via").map(_.toLowerCase).chooseAttempt {
      case "javamail" => \/-(javaMail.map(-\/(_)))
      case "mailgun"  => \/-(mailGun.map(\/-(_)))
      case _          => -\/("Legal values are [JavaMail, MailGun].")
    }

  case class JavaMail(sessionFn: () => Session)

  def javaMail: Config[JavaMail] =
    JavaMailConfig.sessionFn.map(JavaMail.apply)

  def mailGun: Config[MailGun.Props] =
    (Config.need[String]("domain")
      |@| Config.need[String]("key").obfuscateInReport
      ) (MailGun.Props.apply)
      .withPrefix("mailgun.")

  // ===================================================================================================================

  def mailchimp: Config[MailChimp.Props] =
    (Config.need[String]("dc")
      |@| Config.need[String]("key").obfuscateInReport
      |@| Config.need[String]("masterList")
      ) (MailChimp.Props)
      .withPrefix("mailchimp.")

  // ===================================================================================================================

  def freshdesk: Config[FreshDesk.Props] =
    (Config.need[String]("domain")
      |@| Config.need[String]("key").obfuscateInReport
      |@| Config.need[String]("taskmanEmail")
      |@| Config.need[FreshDesk.UnverifiedTicketOrg]("org.landingPage")
      |@| Config.need[FreshDesk.UnverifiedTicketOrg]("org.failure")
      ) (FreshDesk.Props)
      .withPrefix("freshdesk.")

  // ===================================================================================================================

  case class ShipReq(schema: Option[String])

  def shipreq: Config[ShipReq] =
    Config.get[String]("schema")
      .map(ShipReq)
      .withPrefix("shipreq.")

  // ===================================================================================================================

  /**
    * @param healthFile A file that will be touched very regularly so that the health of the system is externally
    *                   observable.
    */
  case class Taskman(remoteCfgRetry: RetryCriteria,
                     queueSize     : Int,
                     trustPeriod   : AssignmentTrustPeriod,
                     pollEvery     : Duration,
                     pollGap       : Duration,
                     healthFile    : Option[String])

  def taskman: Config[Taskman] =
    (RetryCriteria.config.withPrefix("remoteCfg.retry.")
      |@| Config.need[Int]("queueSize").ensure(_ >= 1, "Must be ≥ 1.")
      |@| Config.need[Duration]("trustPeriod").ensure(!_.isShorterThan(10 seconds), "Must be at least 10 seconds.")
      |@| Config.need[Duration]("poll.every").ensure(!_.isShorterThan(50 millis), "Must be at least 50 ms.")
      |@| Config.get[Duration]("poll.gap").ensure(_.fold(true)(!_.isShorterThan(50 millis)), "Must be at least 50 ms.")
      |@| Config.get[String]("healthFile")
      ) { (remoteCfgRetry, qs, tp, pollEvery, pollGapO, healthFile) =>
      val pollGap = pollGapO getOrElse pollEvery
      if (pollGap isLongerThan pollEvery)
        log.warn(s"The minimum poll gap ($pollGap) is larger than the poll time ($pollEvery). Wasteful.")
      Taskman(remoteCfgRetry, qs, AssignmentTrustPeriod(tp), pollEvery, pollGap, healthFile)
    }
      .withPrefix("taskman.")
}
