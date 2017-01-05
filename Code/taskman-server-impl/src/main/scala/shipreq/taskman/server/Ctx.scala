package shipreq.taskman.server

import com.squareup.okhttp.OkHttpClient
import japgolly.microlibs.config.{ConfigReport, Sources => ConfigSources}
import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ExecutorService, TimeUnit}
import scalaz.-\/
import scalaz.effect.IO
import shipreq.base.db.DbAccess
import shipreq.base.util._
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.UserId
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.server.business.MailingList.API.GetListId
import shipreq.taskman.server.business._
import ErrorOr.Implicits._

object TaskmanCtx {

  def apply(dbAccess: DbAccess, config: TaskmanConfig, configReport: ConfigReport): TaskmanCtx =
    apply(dbAccess, config, configReport, SopImpl.configSource(dbAccess))

  def apply(dbAccess: DbAccess, config: TaskmanConfig, configReport: ConfigReport, emailTokenSource: ConfigSources[IO]): TaskmanCtx =
    new TaskmanCtx(dbAccess, config, configReport, emailTokenSource)
}

final class TaskmanCtx(val dbAccess: DbAccess, val config: TaskmanConfig, configReport: ConfigReport, emailTokenSource: ConfigSources[IO]) extends HasLogger {

  private object async {
    val (emailS, email) = Async.newPool("email", config.mail.concurrencyMax)
    def each(f: ExecutorService => Unit): Unit = f(emailS)
  }

  private def runPrerequisite_![A](io: IOE[A]): A =
    ErrorOr.require_!(io.unsafePerformIO())

  private lazy val (emailTokens, emailTokensReport) =
    TaskmanConfig.mailTokens
      .withReport
      .run(emailTokenSource)
      .unsafePerformIO()
      .getOrDie()

  private def getMailChimpListId(name: String): IOE[MailingList.ListId] =
    mailchimp.run(GetListId(name)) >=> (ErrorOr.fromOptionS(_, s"Mailing list not found: $name"))

  val email      = new EmailImpl(config.mail.sessionFn())
  val emails     = new Emails(config.mail.envelopeProps, emailTokens)
  val http       = new OkHttpClient()
  val mailchimp  = new MailChimp(http, config.mailchimp)
  val freshdesk0 = new FreshDesk0(http, config.freshdesk)

  val freshdesk     = runPrerequisite_!(freshdesk0.upgrade)
  val mailingListId = runPrerequisite_!(getMailChimpListId(config.mailchimp.masterList))

  private val clockClock = Clock.systemUTC()

  implicit def trustPeriod   = config.taskman.trustPeriod
  implicit val aopReifier    = new TaskmanApi(TaskmanApi.Context(None), dbAccess.io)
  implicit val bopReifier    = new BopImpl(dbAccess.io, email, mailchimp, freshdesk, config.shipreq.schema)
  implicit val sopReifier    = new SopImpl(dbAccess.io, new Worker.FailureHandler(emails, bopReifier))
  implicit val msgProcessor  = new BusinessLogic(bopReifier, emails, async.email, mailingListId)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = IO(clockClock.instant())
  implicit val nodeId        = sopReifier.getNextNodeId.unsafePerformIO()

  def logConfig(): Unit = {
    log.info(configReport.report)
    log.info(emailTokensReport.report)
  }

  def testConnections(): Unit = {
    log debug "Testing connections..."
    val io = bopReifier.applyUntimed(Bop.FindShipReqUser(-\/(UserId(1))))
    ErrorOr require_! io.unsafePerformIO()
  }

  def shutdown: IO[Unit] =
    shutdown(Some(Duration ofSeconds 20))

  def shutdown(asyncWait: Option[Duration]): IO[Unit] =
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
