package shipreq.taskman.server

import com.squareup.okhttp.OkHttpClient
import java.util.concurrent.{ExecutorService, TimeUnit}
import java.util.Properties
import org.joda.time.{DateTime, Period}
import scala.slick.session.Database
import scalaz.-\/
import scalaz.effect.IO
import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util._
import shipreq.base.util.ScalaExt.Tuple2Ext
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.base.util.jodatime.JodaTimeValueRetrievers
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.api.Types._
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.server.business._
import shipreq.taskman.server.business.MailingList.API.GetListId
import ErrorOr.Implicits._

//==========================================================================================

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._
  override protected def newConnection = DatabaseConnection.establish_!()
  def slick = _slick
}

//==========================================================================================

final class TaskmanProps(evr: StringBasedValueReader) extends HasLogger {
  import evr._
  private val jtr = JodaTimeValueRetrievers(retrieverS)
  import jtr.retrieverPeriod

  private def atLeast(min: Period) =
    valTest[Period](_.toStandardDuration isLongerThan min.toStandardDuration, s"Must be at least $min.")

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

  def propmap = mail.propmap ++ mailchimp.propmap ++ shipreq.propmap ++ taskman.propmap

  // --------------------------------------------------------------------------

  object mail extends Email.EnvelopeProps {
    import Email._
    private implicit def scope: PropScope = scopeByNS("mail")
    private[this] implicit def rEA = EmailImpl.addressLoader
    private[this] implicit def rEE = EmailImpl.envelopeLoader
    private[this] implicit def rEF = EmailImpl.envelopeFrontLoader
    private[TaskmanProps] def propmap = mkPropMap(
      "public.from" -> publicFrom, "landingPage" -> landingPageEnv, "support" -> supportEnv,
      "concurrency.max" -> concurrencyMax)

    val publicFrom     = need[Addr]("public.from")
    val landingPageEnv = need[EnvelopeFront]("landingPage")
    val supportEnv     = need[Envelope]("support")
    val concurrencyMax = validate("concurrency.max", need[Int])(atLeast(1))
  }

  object mailchimp extends MailChimp.Props {
    private implicit def scope: PropScope = scopeByNS("mailchimp")
    private[TaskmanProps] def propmap = mkPropMap("dc" -> dc, "key" -> key, "masterList" -> masterList)

    val dc         = need[String]("dc")
    val key        = need[String]("key")
    val masterList = need[String]("masterList")
  }

  object shipreq {
    private implicit def scope: PropScope = scopeByNS("shipreq")
    private[TaskmanProps] def propmap = mkPropMap("schema" -> schema)

    val schema = getO[String]("schema")
  }

  object taskman {
    private implicit def scope: PropScope = scopeByNS("taskman")
    private[TaskmanProps] def propmap = mkPropMap(
      "queueSize" -> queueSize, "trustPeriod" -> trustPeriod, "poll.every" -> pollEvery, "poll.min" -> pollGap)

    val queueSize   = validate("queueSize", need[Int])(atLeast(1))
    val trustPeriod = AssignmentTrustPeriod(validate("trustPeriod", need[Period])(atLeast(10 seconds)))
    val pollEvery   = validate("poll.every", need[Period])(atLeast(50 ms))
    val pollGap     = validate("poll.min", n => getO[Period](n) getOrElse pollEvery)(atLeast(50 ms))

    if (pollGap.toStandardDuration isLongerThan pollEvery.toStandardDuration)
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

class TaskmanCtx(val db: Database, mailProps: Properties, evr: StringBasedValueReader) extends HasLogger {
  import TaskmanCtx._

  val props = new TaskmanProps(evr)
  def cfgFromApiReader = SopImpl.cfgValueReader(db)

  private object async {
    def each(f: ExecutorService => Unit): Unit = f(emailS)
    val (emailS, email) = Async.newPool("email", props.mail.concurrencyMax)
  }

  private def getMailChimpListId_!(name: String): MailingList.ListId =
    ErrorOr.require_!(
      mailchimp.run(GetListId(name)).emapE {
        case None     => ErrorOr.error(s"Mailing list not found: $name")
        case Some(id) => ErrorOr(id)
      }.unsafePerformIO()
    )

  val email     = new EmailImpl(EmailImpl.loadSession(mailProps))
  val emails    = new Emails(props.mail, new EmailTokenValues(cfgFromApiReader))
  val http      = new OkHttpClient()
  val mailchimp = new MailChimp(http, props.mailchimp)

  val mailingListId = getMailChimpListId_!(props.mailchimp.masterList)

  implicit def trustPeriod   = props.taskman.trustPeriod
  implicit val aopReifier    = new TaskmanApi(TaskmanApi.Context(None), db)
  implicit val bopReifier    = new BopImpl(db, email, mailchimp, props.shipreq.schema)
  implicit val sopReifier    = new SopImpl(db, emails, bopReifier)
  implicit val msgProcessor  = new BusinessLogic(bopReifier, emails, async.email, mailingListId)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = IO(new DateTime)
  implicit val nodeId        = sopReifier.getNextNodeId.unsafePerformIO()

  def logContent(): Unit = {
    props.logContent()
    val p = "    "
    log info "Settings"
    log.info z s"${p}Mailing list ID = ${mailingListId.value}"
    log.info z s"${p}Node ID         = ${nodeId.value}"
  }

  def testConnections(): Unit = {
    log debug "Testing connections..."
    val io = bopReifier.applyUntimed(Bop.LookupShipReqUser(-\/(1.tag)))
    ErrorOr require_! io.unsafePerformIO()
  }

  def shutdown(asyncWait: Option[Period] = Some(Period seconds 20)): Unit = {
    for (p <- asyncWait) {
      val until = DateTime.now.plus(p).getMillis
      async.each(_.shutdown())
      async.each(e => {
        val rem = until - DateTime.now.getMillis
        if (rem > 0)
          e.awaitTermination(rem, TimeUnit.MILLISECONDS)
      })
    }
    async.each(_.shutdownNow())
  }
}
