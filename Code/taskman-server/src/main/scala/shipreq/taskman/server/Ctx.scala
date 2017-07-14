package shipreq.taskman.server

import com.squareup.okhttp.OkHttpClient
import japgolly.microlibs.config.{Sources => ConfigSources}
import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ExecutorService, TimeUnit}
import scalaz.-\/
import scalaz.effect.IO
import shipreq.base.db.DbAccess
import shipreq.base.util.FxModule._
import shipreq.base.util._
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.UserId
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.taskman.server.business.MailingList.API.GetListId
import shipreq.taskman.server.business._
import ErrorOr.Implicits._

object TaskmanCtx {

  def apply(dbAccess: DbAccess, config: TaskmanConfig): TaskmanCtx =
    apply(dbAccess, config, SopImpl.configSource(dbAccess))

  def apply(dbAccess: DbAccess, config: TaskmanConfig, emailTokenSource: ConfigSources[Fx]): TaskmanCtx =
    new TaskmanCtx(dbAccess, config, emailTokenSource)
}

final class TaskmanCtx(val dbAccess: DbAccess, val config: TaskmanConfig, emailTokenSource: ConfigSources[Fx]) extends HasLogger {

  private object async {
    val (emailS, email) = Async.newPool("email", config.mail.concurrencyMax)
    def each(f: ExecutorService => Unit): Unit = f(emailS)
  }

  private def runPrerequisite_![A](io: IOE[A]): A =
    ErrorOr.require_!(io.unsafePerformIO())

  private val (emailTokens, emailTokensReport) =
    TaskmanConfig.mailTokens
      .withReport
      .run(emailTokenSource)
      .map(_.getOrDie())
      .retryOnException((n, t) => config.taskman.remoteCfgRetry(n).map(d => Fx {
        log.warn(s"Remote config error occurred. Retrying...\n${t.getMessage}")
        Thread sleep d.toMillis
      }))
      .unsafeRun()

  log.info(emailTokensReport.report)

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
  implicit val taskmanApi    = TaskmanApiImpl(TaskmanApiImpl.Context(None), dbAccess.fx.trans)
  implicit val bopReifier    = new BopImpl(dbAccess.io, email, mailchimp, freshdesk, config.shipreq.schema)
  implicit val sopReifier    = new SopImpl(dbAccess.io, new Worker.FailureHandler(emails, bopReifier))
  implicit val msgProcessor  = new BusinessLogic(bopReifier, emails, async.email, mailingListId)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = IO(clockClock.instant())
  implicit val nodeId        = sopReifier.getNextNodeId.unsafePerformIO()

  def testConnections(): Unit = {
    log debug "Testing connections..."
    val io = bopReifier.applyUntimed(Bop.FindShipReqUser(-\/(UserId(1))))
    ErrorOr require_! io.unsafePerformIO()
  }

  def shutdown: Fx[Unit] =
    shutdown(Some(Duration ofSeconds 20))

  def shutdown(asyncWait: Option[Duration]): Fx[Unit] =
    Fx {
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
