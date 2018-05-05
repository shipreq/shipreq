package shipreq.taskman.server

import okhttp3.OkHttpClient
import japgolly.microlibs.config.{Sources => ConfigSources}
import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ExecutorService, TimeUnit}
import scalaz.{-\/, \/-}
import scalaz.syntax.catchable._
import shipreq.base.db.DbAccess
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.taskman.server.business._
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.business._

object TaskmanCtx {

  def apply(dbAccess: DbAccess, config: TaskmanConfig): TaskmanCtx =
    apply(dbAccess, config, ServerOpFx.configSource(dbAccess))

  def apply(dbAccess: DbAccess, config: TaskmanConfig, emailTokenSource: ConfigSources[Fx]): TaskmanCtx =
    new TaskmanCtx(dbAccess, config, emailTokenSource)
}

final class TaskmanCtx(val dbAccess: DbAccess, val config: TaskmanConfig, emailTokenSource: ConfigSources[Fx]) extends HasLogger {

  private def getMailChimpListId(name: String): Fx[MailingList.ListId] =
    mailchimp(MailingList.API.GetListId(name)) getOrFail s"Mailing list not found: $name"

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

  private object async {
    val (emailExecutorService, emailScheduler) = Async.newPool("email", config.mail.concurrencyMax)

    def each(f: ExecutorService => Unit): Unit =
      f(emailExecutorService)
  }

  private val http = new OkHttpClient()

  val sendMail: BusinessOp.SendEmail => Fx[Unit] =
    config.mail.mechanism match {
      case \/-(p) => new MailGun(p)(http)
      case -\/(p) => new JavaMail(p.sessionFn())
    }

  val emails        = new Emails(config.mail.envelopeProps, emailTokens)
  val freshdesk     = new FreshDesk0(config.freshdesk)(http).upgrade.unsafeRun()
  val mailchimp     = new MailChimp(config.mailchimp)(http)
  val mailingListId = getMailChimpListId(config.mailchimp.masterList).unsafeRun()

  private val clockClock = Clock.systemUTC()

  implicit def trustPeriod   = config.taskman.trustPeriod
  implicit val taskmanApi    = TaskmanApiImpl(TaskmanApiImpl.Context(None), dbAccess.fx.trans)
  implicit val businessOpFx  = new BusinessOpFx(sendMail, mailchimp, freshdesk, dbAccess.fx, config.shipreq.schema)
  implicit val serverOpFx    = new ServerOpFx(dbAccess.fx, new Worker.FailureHandler(emails)(businessOpFx))
  implicit val msgProcessor  = new BusinessLogic(emails, async.emailScheduler, mailingListId)(businessOpFx)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = Fx(clockClock.instant())
  implicit val nodeId        = serverOpFx.getNextNodeId.unsafeRun()

  def testConnections(): Unit = {
    log debug "Testing connections..."
    businessOpFx.applyUntimed(BusinessOp.FindShipReqUsers(None)).unsafeRun()
  }

  def shutdown: Fx[Unit] =
    shutdown(Some(Duration ofSeconds 20))

  def shutdown(asyncWait: Option[Duration]): Fx[Unit] =
    Fx {
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
    }
      .attempt
      .map(_.swap.foreach(log.error("Error shutting down ctx.", _)))
}