package shipreq.taskman.server

import com.google.common.util.concurrent.ThreadFactoryBuilder
import java.util.concurrent.{TimeUnit, Callable, ExecutorService, Executors}
import java.util.Properties
import org.joda.time.{DateTime, Period}
import scala.slick.session.Database
import scalaz.~>
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
import shipreq.taskman.server.business.{BusinessLogic, Failure, Email}

//==========================================================================================

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._

  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick
}

//==========================================================================================

final class TaskmanProps(evr: StringBasedValueReader) extends HasLogger {
  private type EA = EmailImpl.EA
  private def addrParser  = EmailImpl.AddressParser

  import evr._
  private val jtr = JodaTimeValueRetrievers(retrieverS)
  import jtr.retrieverPeriod

  private def atLeast(min: Period) =
    valTest[Period](_.toStandardDuration isLongerThan min.toStandardDuration, s"Must be at least $min.")

  private def atLeast(min: Int) =
    valTest[Int](_ >= min, s"Must be at least $min.")

  private def mkPropMap(kvs: (String, Any)*)(implicit s: PropScope): List[(String, Any)] =
    kvs.toList.map(_.map1(s.run))

  object mail {
    private implicit def scope: PropScope = scopeByNS("taskman.mail")
    private[this] implicit def rEA = retrieverS.map(s => addrParser(s.tag[IsEmailAddr]))
    private[this] implicit def rEE = EmailImpl.envelopeLoader(rEA)

    val publicFrom     = validate("public.from", need[EA])(valTestNotError)
    val supportEnv     = need[Email.Envelope[EA]]("support")
    val concurrencyMax = validate("concurrency.max", need[Int])(atLeast(1))

    private[TaskmanProps] def propmap = mkPropMap(
      "public.from" -> publicFrom, "support" -> supportEnv, "concurrency.max" -> concurrencyMax)
  }

  object work {
    private implicit def scope: PropScope = scopeByNS("taskman.work")

    val queueSize   = validate("queueSize", need[Int])(atLeast(1))
    val trustPeriod = AssignmentTrustPeriod(validate("trustPeriod", need[Period])(atLeast(10 seconds)))
    val pollEvery   = validate("poll.every", need[Period])(atLeast(50 ms))
    val pollGap     = validate("poll.min", n => getO[Period](n) getOrElse pollEvery)(atLeast(50 ms))

    if (pollGap.toStandardDuration isLongerThan pollEvery.toStandardDuration)
      log.warn.z(s"The minimum poll gap ($pollGap) is larger than the poll time ($pollEvery). Wasteful.")

    private[TaskmanProps] def propmap = mkPropMap(
      "queueSize" -> queueSize, "trustPeriod" -> trustPeriod, "poll.every" -> pollEvery, "poll.min" -> pollGap)
  }

  def propmap = mail.propmap ++ work.propmap
}

//==========================================================================================

object TaskmanAsync {
  type Future[A] = java.util.concurrent.Future[A]

  final case class CallableIO[A](io: IO[A]) extends Callable[A] {
    def call() = io.unsafePerformIO()
  }

  def ioSubmitter(es: ExecutorService): (IO ~> Future) =
    new (IO ~> Future) {
      def apply[A](io: IO[A]): Future[A] = es submit CallableIO(io)
    }
}

//==========================================================================================

class TaskmanCtx(val db: Database, mailProps: Properties, evr: StringBasedValueReader)
  extends Email.Ctx[EmailImpl.EA] with EmailImpl.Ctx with BopImpl.Ctx with HasLogger {

  protected def fromDb = SopImpl.cfgValueReader(db)

  override val mailSession = EmailImpl.loadSession(mailProps)
  override val addrParser  = EmailImpl.AddressParser
  override val emailer     = new EmailImpl(this)

  val props = new TaskmanProps(evr)

  override val shipreq  = need(CfgKeys.Webapp.appName )(GlobalScope, fromDb.retrieverS)
  override val loginUrl = need(CfgKeys.Webapp.loginUrl)(GlobalScope, fromDb.retrieverS)

  override val publicFrom = props.mail.publicFrom
  override val supportEnv = props.mail.supportEnv

  private[TaskmanCtx] object async {
    val emailThreadPool = Executors.newFixedThreadPool(props.mail.concurrencyMax,
      new ThreadFactoryBuilder().setNameFormat("async-email-%d").build)
    // TODO each worker should wrap async task with own mdc
    val email = TaskmanAsync.ioSubmitter(emailThreadPool)

    def each(f: ExecutorService => Unit): Unit = f(emailThreadPool)
  }

  def logContent(): Unit = {
    log info "Properties:"
    val ps = props.propmap.sortBy(_._1)
    val maxKeyLen = ps.map(_._1.length).max
    for ((k,v) <- ps)
      log.info.fmt(s"    %-${maxKeyLen}s = %s", k, v)
    log.info.z(s"Node ID is ${nodeId.value}.")
  }

  implicit def trustPeriod   = props.work.trustPeriod
  implicit val aopReifier    = new TaskmanApi(TaskmanApi.Context(None), db)
  implicit val bopReifier    = new BopImpl(this)
  implicit val sopReifier    = new SopImpl(db, this, bopReifier)
  implicit val msgProcessor  = new BusinessLogic(this, bopReifier, async.email)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val clock         = IO(new DateTime)
  implicit val nodeId        = sopReifier.getNextNodeId.unsafePerformIO()

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
