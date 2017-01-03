package shipreq.taskman.server

import japgolly.microlibs.config._
import scalaz.syntax.applicative._
import shipreq.base.util.log.{HasLogger, LogLevel}
import shipreq.base.util.JavaTimeHelpers._
import ConfigParser.Implicits.Defaults._
import JavaTimeConfigParsers._
import java.time.Duration
import javax.mail.Session
import shipreq.taskman.server.business._
import EmailImpl.ConfigParsers._
import FreshDesk.ConfigParsers._
import LogLevel.configParserShipReqLogLevel
import shipreq.taskman.api.CfgKeys

final case class TaskmanConfig(mail     : TaskmanConfig.Mail,
                               mailchimp: MailChimp.Props,
                               freshdesk: FreshDesk.Props,
                               shipreq  : TaskmanConfig.ShipReq,
                               taskman  : TaskmanConfig.Taskman,
                               report   : ConfigReport)

object TaskmanConfig extends HasLogger {

  def config: Config[TaskmanConfig] =
  // TODO Does Config.report work like this? It should probably be forced last via .withReport
    // (mail |@| mailchimp |@| freshdesk |@| shipreq |@| taskman |@| Config.report) (apply)
    (mail |@| mailchimp |@| freshdesk |@| shipreq |@| taskman).tupled
      .withReport
      .map { case ((a, b, c, d, e), z) => apply(a, b, c, d, e, z) }

  // TODO Put props and parsers in Business classes

  // ===================================================================================================================

  case class Mail(publicFrom: Email.Addr,
                  archiveAddrs: List[Email.Addr],
                  concurrencyMax: Int,
                  sessionFn: () => Session) {
    def envelopeProps = Email.EnvelopeProps(publicFrom, archiveAddrs)
  }

  def mail: Config[Mail] = {
    val custom =
      (Config.need[Email.Addr]("public.from")
        |@| Config.getOrUse[List[Email.Addr]]("archive.to", Nil)
        |@| Config.need[Int]("concurrency.max").ensure(_ >= 1, "Must be ≥ 1.")
        ).tupled.withPrefix("mail.")
    (custom |@| JavaMailConfig.sessionFn) { case ((a, b, c), s) => Mail(a, b, c, s) }
  }

  def mailTokens: Config[Email.TokenValues] =
    (Config.need[String](CfgKeys.Webapp.appName)
      |@| Config.need[String](CfgKeys.Webapp.loginUrl)
      ) (Email.TokenValues)

  // ===================================================================================================================

  def mailchimp: Config[MailChimp.Props] =
    (Config.need[String]("dc")
      |@| Config.need[String]("key")
      |@| Config.need[String]("masterList")
      |@| Config.need[LogLevel]("logLevel")
      ) (MailChimp.Props)
      .withPrefix("mailchimp.")

  // ===================================================================================================================

  def freshdesk: Config[FreshDesk.Props] =
    (Config.need[String]("domain")
      |@| Config.need[String]("key")
      |@| Config.need[String]("taskmanEmail")
      |@| Config.need[FreshDesk.TicketOrg]("org.landingPage")
      |@| Config.need[FreshDesk.TicketOrg]("org.failure")
      |@| Config.need[LogLevel]("logLevel")
      ) (FreshDesk.Props)
      .withPrefix("freshdesk.")

  // ===================================================================================================================

  case class ShipReq(schema: Option[String])

  def shipreq: Config[ShipReq] =
    Config.get[String]("schema")
      .map(ShipReq)
      .withPrefix("shipreq.")

  // ===================================================================================================================

  case class Taskman(queueSize  : Int,
                     trustPeriod: AssignmentTrustPeriod,
                     pollEvery  : Duration,
                     pollGap    : Duration)

  def taskman: Config[Taskman] =
    (Config.need[Int]("queueSize").ensure(_ >= 1, "Must be ≥ 1.")
      |@| Config.need[Duration]("trustPeriod").ensure(!_.isShorterThan(10 seconds), "Must be at least 10 seconds.")
      |@| Config.need[Duration]("pollEvery").ensure(!_.isShorterThan(50 millis), "Must be at least 50 ms.")
      |@| Config.get[Duration]("pollGap").ensure(_.fold(true)(!_.isShorterThan(50 millis)), "Must be at least 50 ms.")
      ) { (qs, tp, pollEvery, pollGapO) =>
      val pollGap = pollGapO getOrElse pollEvery
      if (pollGap isLongerThan pollEvery)
        log.warn.z(s"The minimum poll gap ($pollGap) is larger than the poll time ($pollEvery). Wasteful.")
      Taskman(qs, AssignmentTrustPeriod(tp), pollEvery, pollGap)
    }
      .withPrefix("taskman.")
}
