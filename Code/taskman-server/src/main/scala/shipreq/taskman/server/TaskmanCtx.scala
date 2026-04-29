package shipreq.taskman.server

import cats.effect.Resource
import cats.~>
import japgolly.clearconfig.ConfigSources
import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ExecutorService, TimeUnit}
import okhttp3.OkHttpClient
import shipreq.base.db._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.api.impl.TaskmanApiImpl
import shipreq.taskman.server.business._
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.business._

object TaskmanCtx {

  def apply(db: DbAccessor, config: TaskmanConfig, xa: XA): Resource[Fx, TaskmanCtx] =
    apply(db, config, xa, ServerOpFx.configSource(db, xa))

  def apply(db: DbAccessor, config: TaskmanConfig, xa: XA, emailTokenSource: ConfigSources[Fx]): Resource[Fx, TaskmanCtx] =
    Resource.make(Fx(new TaskmanCtx(db, config, xa, emailTokenSource)))(_.shutdown)

}

final class TaskmanCtx(val db          : DbAccessor,
                       val config      : TaskmanConfig,
                       val xa          : XA,
                       emailTokenSource: ConfigSources[Fx]) extends HasLogger {

  private val (emailTokens, emailTokensReport) =
    TaskmanConfig.mailTokens
      .withReport
      .run(emailTokenSource)
      .map(_.getOrDie())
      .retryOnException((n, t) => config.taskman.remoteCfgRetry(n).map(d => Fx {
        logger.warn(s"Remote config error occurred. Retrying...\n${t.getMessage}")
        Thread sleep d.toMillis
      }))
      .unsafeRun()

  logger.info(s"Config report: (for email tokens)\n${emailTokensReport.full}")

  private object async {
    val (emailExecutorService, emailScheduler) =
      Async.newPool("email", config.mail.concurrencyMax)

    def each(f: ExecutorService => Unit): Unit =
      f(emailExecutorService)
  }

  val emails = new Emails(config.mail.envelopeProps, emailTokens)

  private val http = new OkHttpClient()

  val mailingList: MailingList.API ~> Fx =
    config.mailingList match {
      case TaskmanConfig.MailingListProps.NoOp            => MailingListNoOp
      case TaskmanConfig.MailingListProps.ViaMailChimp(p) => new MailChimp(p)(http)
    }

  val sendMail: BusinessOp.SendEmail => Fx[Unit] =
    config.mail.mechanism match {
      case \/-(p) => new MailGun(p)(http)
      case -\/(p) => new JavaMail(p.sessionFn())
    }

  val supportDesk: Support.API ~> Fx =
    config.supportDesk match {
      case -\/(props) => new SupportViaMail(props, sendMail)
      case \/-(props) => new FreshDesk0(props)(http).upgrade.unsafeRun()
    }

  private val clockClock = Clock.systemUTC()

  implicit def trustPeriod   = config.taskman.trustPeriod
  implicit val taskmanApi    = TaskmanApi.addLogging(TaskmanApiImpl(None).trans(xa.trans))
  implicit val businessOpFx  = new BusinessOpFx(sendMail, mailingList, supportDesk, xa.trans, config.shipreq.schema)
  implicit val serverOpFx    = new ServerOpFx(xa, new Worker.FailureHandler(emails)(businessOpFx))
  implicit val businessLogic = new BusinessLogic(emails, async.emailScheduler)(businessOpFx)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = Fx(clockClock.instant())
  implicit val nodeId        = serverOpFx.getNextNodeId.unsafeRun()

  private[TaskmanCtx] def shutdown: Fx[Unit] =
    shutdown(Some(Duration ofSeconds 20))

  private[TaskmanCtx] def shutdown(asyncWait: Option[Duration]): Fx[Unit] =
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
      .map(_.swap.foreach(logger.warn("Error shutting down ctx.", _)))
}
