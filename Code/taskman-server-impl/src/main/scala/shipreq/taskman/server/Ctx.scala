package shipreq.taskman.server

import java.util.Properties
import org.joda.time.{DateTime, Period}
import scala.slick.session.Database
import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util._
import shipreq.base.util.jodatime.JodaTimeValueRetrievers
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.api.Types._
import shipreq.taskman.server.business.{BusinessLogic, Failure, Email}
import scalaz.effect.IO

//==========================================================================================

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._

  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick
}

//==========================================================================================

class TaskmanCtx(db: Database, mailProps: Properties, evr: StringBasedValueReader)
  extends Email.Ctx with EmailImpl.Ctx with BopImpl.Ctx with Logger {

  implicit val sopReifier = new SopImpl(db)

  protected def fromDb = CfgValueReader(sopReifier)
  protected implicit def scope: PropScope = scopeByNS("taskman")
  protected implicit def _retrieverS = evr.retrieverS
  val jtr = JodaTimeValueRetrievers(_retrieverS)

  import evr.retrieverI
  import jtr.retrieverPeriod

  override def mailSession = EmailImpl.loadSession(mailProps)
  override val defaultFromAddress = need[String]("mail.from").tag
  override val shipreq  = need(CfgKeys.Webapp.appName )(GlobalScope, fromDb.retrieverS)
  override val loginUrl = need(CfgKeys.Webapp.loginUrl)(GlobalScope, fromDb.retrieverS)
  override val emailer  = new EmailImpl(this)

  object manager {
    private implicit def scope: PropScope = scopeByNS("taskman.manager")
    private def minimumTrustPeriodSec = 10
    private def minimumTrustPeriod = Period.seconds(minimumTrustPeriodSec).toStandardDuration

    val queueSize = validate("queueSize", need[Int])(valTest(_ >= 1, "Must be at least 1."))

    val trustPeriod = validate("trustPeriod", need[Period])(valTest(
      _.toStandardDuration isLongerThan minimumTrustPeriod,
      s"Must be at least $minimumTrustPeriodSec seconds."))
  }

  def loggable = Map[String, Any](
    "defaultFromAddress" -> defaultFromAddress
    , "shipreq" -> shipreq
    , "loginUrl" -> loginUrl
    , "manager.queueSize" -> manager.queueSize
    , "manager.trustPeriod" -> manager.trustPeriod
  )
  log.info("Config: {}", loggable.toList
    .sortBy(kv => (kv._1.count(_ == '.'), kv._1))
    .map{case (k,v) => "\n  %-20s = %s".format(k,v) }
    .mkString
  )

  implicit val bopReifier = new BopImpl(this)
  implicit val failurePolicy = Failure.failurePolicy
  implicit val msgProcessor = BusinessLogic(this, bopReifier)
  implicit val clock = IO(new DateTime)

  implicit val nodeId = sopReifier.getNextNodeId.unsafePerformIO()
  log.info("Node ID is {}.", nodeId.value)
}
