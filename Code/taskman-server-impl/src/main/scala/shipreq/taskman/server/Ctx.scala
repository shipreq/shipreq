package shipreq.taskman.server

import com.squareup.okhttp.OkHttpClient
import doobie.imports._
import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ExecutorService, TimeUnit}
import java.util.Properties
import scalaz.-\/
import scalaz.effect.IO
//import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util._
import shipreq.base.util.ScalaExt.Tuple2Ext
import shipreq.base.util.effect.IOE
import shipreq.base.util.JavaTimeHelpers._
import shipreq.base.util.log.{HasLogger, LogLevel}
import shipreq.taskman.api.{CfgKeys, UserId}
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.server.business._
import shipreq.taskman.server.business.MailingList.API.GetListId
import ErrorOr.Implicits._

//==========================================================================================

final class TaskmanProps(evr: StringBasedValueReader) extends HasLogger {
  import evr._
  private val jtr = JavaTimeValueRetrievers(retrieverS)
  import jtr.retrieverDuration
  private implicit def llr = LogLevel.evr

  private def atLeast(min: Duration) =
    valTest[Duration](d => !d.isShorterThan(min), s"Must be at least $min.")

  private def atLeast(min: Int) =
    valTest[Int](_ >= min, s"Must be at least $min.")

  private def mkPropMap(kvs: (String, Any)*)(implicit s: PropScope): List[(String, Any)] =
    kvs.toList.map(_.map1(s.run))

  def logContent(): Unit = {
    log info "Properties:"
    val ps = propmap.sortBy(_._1)
    val maxKeyLen = ps.map(_._1.length).max
    for ((k,v) <- ps)
      log.info.fmt(s"    %-${maxKeyLen}s = %s", k, v)
  }

  def propmap = mail.propmap ++ mailchimp.propmap ++ freshdesk.propmap ++ shipreq.propmap ++ taskman.propmap

  // --------------------------------------------------------------------------

  object mail extends Email.EnvelopeProps {
    import Email._
    private implicit def scope: PropScope = scopeByNS("mail")
    private val rEA = new EmailImpl.Retrievers()
    import rEA._

    private[TaskmanProps] def propmap = mkPropMap(
      "public.from" -> publicFrom, "archive.to" -> archiveAddrs, "concurrency.max" -> concurrencyMax)

    override val publicFrom   = need[Addr]("public.from")
    override val archiveAddrs = tryNeed[List[Addr]]("archive.to", Nil)
    val concurrencyMax        = validate("concurrency.max", need[Int])(atLeast(1))
  }

  object mailchimp extends MailChimp.Props {
    private implicit def scope: PropScope = scopeByNS("mailchimp")
    private[TaskmanProps] def propmap = mkPropMap(
      "dc" -> dc, "key" -> key, "masterList" -> masterList, "logLevel" -> logLevel)

    override val dc         = need[String]("dc")
    override val key        = need[String]("key")
    override val masterList = need[String]("masterList")
    override val logLevel   = need[LogLevel]("logLevel")
  }

  object freshdesk extends FreshDesk.Props {
    import FreshDesk._
    private implicit def scope: PropScope = scopeByNS("freshdesk")
    private implicit def rTO = ticketOrgRetriever
    private[TaskmanProps] def propmap = mkPropMap(
      "domain" -> domain, "key" -> key, "logLevel" -> logLevel, "taskmanEmail" -> taskmanEmail,
      "org.landingPage" -> landingPage, "org.failure" -> failure)

    override val domain       = need[String]("domain")
    override val key          = need[String]("key")
    override val taskmanEmail = need[String]("taskmanEmail")
    override val landingPage  = need[TicketOrg]("org.landingPage")
    override val failure      = need[TicketOrg]("org.failure")
    override val logLevel     = need[LogLevel]("logLevel")
  }

  object shipreq {
    private implicit def scope: PropScope = scopeByNS("shipreq")
    private[TaskmanProps] def propmap = mkPropMap("schema" -> schema)

    val schema = getO[String]("schema")
  }

  object taskman {
    private implicit def scope: PropScope = scopeByNS("taskman")
    private[TaskmanProps] def propmap = mkPropMap(
      "queueSize" -> queueSize, "trustPeriod" -> trustPeriod.value, "poll.every" -> pollEvery, "poll.gap" -> pollGap)

    val queueSize   = validate("queueSize", need[Int])(atLeast(1))
    val trustPeriod = AssignmentTrustPeriod(validate("trustPeriod", need[Duration])(atLeast(10 seconds)))
    val pollEvery   = validate("poll.every", need[Duration])(atLeast(50 millis))
    val pollGap     = validate("poll.gap", n => getO[Duration](n) getOrElse pollEvery)(atLeast(50 millis))

    if (pollGap isLongerThan pollEvery)
      log.warn.z(s"The minimum poll gap ($pollGap) is larger than the poll time ($pollEvery). Wasteful.")
  }
}

//==========================================================================================

object TaskmanCtx {
  class EmailTokenValues(evr: StringBasedValueReader) extends Email.TokenValues {
    private implicit def scope = GlobalScope
    import evr._
    override val shipreqName = need[String](CfgKeys.Webapp.appName)
    override val loginUrl    = need[String](CfgKeys.Webapp.loginUrl)
  }
}

class TaskmanCtx(val db: Transactor[IO], mailProps: Properties, evr: StringBasedValueReader) extends HasLogger {
  import TaskmanCtx._

  val props = new TaskmanProps(evr)
  def cfgFromApiReader = SopImpl.cfgValueReader(db)

  private object async {
    def each(f: ExecutorService => Unit): Unit = f(emailS)
    val (emailS, email) = Async.newPool("email", props.mail.concurrencyMax)
  }

  private def runPrerequisite_![A](io: IOE[A]): A =
    ErrorOr.require_!(io.unsafePerformIO())

  private def getMailChimpListId(name: String): IOE[MailingList.ListId] =
    mailchimp.run(GetListId(name)) >=> (ErrorOr.fromOptionS(_, s"Mailing list not found: $name"))

  val email      = new EmailImpl(EmailImpl.loadSession(mailProps))
  val emails     = new Emails(props.mail, new EmailTokenValues(cfgFromApiReader))
  val http       = new OkHttpClient()
  val mailchimp  = new MailChimp(http, props.mailchimp)
  val freshdesk0 = new FreshDesk0(http, props.freshdesk)

  val freshdesk     = runPrerequisite_!(freshdesk0.upgrade)
  val mailingListId = runPrerequisite_!(getMailChimpListId(props.mailchimp.masterList))

  private val clockClock = Clock.systemUTC()

  implicit def trustPeriod   = props.taskman.trustPeriod
  implicit val aopReifier    = new TaskmanApi(TaskmanApi.Context(None), db)
  implicit val bopReifier    = new BopImpl(db, email, mailchimp, freshdesk, props.shipreq.schema)
  implicit val sopReifier    = new SopImpl(db, new Worker.FailureHandler(emails, bopReifier))
  implicit val msgProcessor  = new BusinessLogic(bopReifier, emails, async.email, mailingListId)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = IO(clockClock.instant())
  implicit val nodeId        = sopReifier.getNextNodeId.unsafePerformIO()

  def logContent(): Unit = {
    props.logContent()
    val p = "    "
    log info "Settings"
    log.info z s"${p}FreshDesk failure group = ${freshdesk.propsI.failure.group}"
    log.info z s"${p}FreshDesk LP group      = ${freshdesk.propsI.landingPage.group}"
    log.info z s"${p}Mailing list ID         = ${mailingListId.value}"
    log.info z s"${p}Node ID                 = ${nodeId.value}"
  }

  def testConnections(): Unit = {
    log debug "Testing connections..."
    val io = bopReifier.applyUntimed(Bop.FindShipReqUser(-\/(UserId(1))))
    ErrorOr require_! io.unsafePerformIO()
  }

  def shutdown(asyncWait: Option[Duration] = Some(Duration ofSeconds 20)): IO[Unit] =
    IO {
      ErrorOr.safe {
        for (p <- asyncWait) {
          val until = Instant.now().plus(p).getNano
          async.each(_.shutdown())
          async.each(e => {
            val rem = until - Instant.now().getNano
            if (rem > 0)
              e.awaitTermination(rem, TimeUnit.NANOSECONDS)
          })
        }
        async.each(_.shutdownNow())
      }.leftMap(e => log.error(e, "Error shutting down ctx."))
      ()
    }
}
