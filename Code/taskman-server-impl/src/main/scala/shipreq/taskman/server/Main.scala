package shipreq.taskman.server

import java.util.Properties
import shipreq.base.util._
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.server.business.{BusinessLogic, Failure, Email}
import scala.slick.session.Database

//==========================================================================================

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._

  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick
}

//==========================================================================================

class TaskmanCtx(db: Database, mailProps: Properties, evr: StringBasedValueReader)
  extends Email.Ctx with EmailImpl.Ctx with BopImpl.Ctx with Logger {

  val sopReifier = new SopImpl(db)

  protected def fromDb = CfgValueReader(sopReifier)
  protected implicit def scope: PropScope = scopeByNS("taskman")
  protected implicit def _retrieverS = evr.retrieverS
  import evr.retrieverI

  override def mailSession = EmailImpl.loadSession(mailProps)
  override val defaultFromAddress = need[String]("mail.from").tag
  override val shipreq  = need(CfgKeys.Webapp.appName )(GlobalScope, fromDb.retrieverS)
  override val loginUrl = need(CfgKeys.Webapp.loginUrl)(GlobalScope, fromDb.retrieverS)
  override val emailer  = new EmailImpl(this)

  object manager {
    implicit def scope: PropScope = scopeByNS("taskman.manager")
    val queueSize = validate("queueSize", need[Int])(valTest(_ >= 1, "Must be at least 1."))
    val assignmentTrustPeriod = 5 minutes
  }

  def loggable = Map[String, Any](
    "defaultFromAddress" -> defaultFromAddress
    , "shipreq" -> shipreq
    , "loginUrl" -> loginUrl
    , "manager.queueSize" -> manager.queueSize
  )
  log.info("Config: {}", loggable.toList.map{case (k,v) => s"$k=$v"}.sorted.mkString(", "))

  val bopReifier = new BopImpl(this)
  val failurePolicy = Failure.failurePolicy
  val msgProcessor = BusinessLogic(this, bopReifier)
  val nodeId = sopReifier.getNextNodeId.unsafePerformIO()
  log.debug("Node ID is {}.", nodeId.value)
}

//==========================================================================================

object Main extends Logger {

  def main(args: Array[String]) {

    implicit def scope = GlobalScope

    // Determine run mode
    implicit val _rm = RunMode.retrieverFromSysProps
    val runMode: RunMode = tryNeed("run.mode", RunMode.detectFromStackTrace())
    log.info("Run mode: {}", runMode)

    // Config 1
    val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
    val propsR = JPropertiesValueReader(props)

    // Init database
    val db = new Db(propsR)
    db.init()

    try {

      // Config 2
      val ctx = new TaskmanCtx(db.slick, props, propsR)
      //    log.debug("Config: {}", ctx.loggable)

      // Taskman
      // TODO worker/manager ctx
      //    log.info("Node ID is {}.", ctx.nodeId.value)

      // Send an email
      /*
    val from: EmailAddr = "whatever@gmail.com".tag
    val to: EmailAddr = "japgolly+test@gmail.com".tag
    val io = bopImpl(SendEmail(
      Email.Envelope(from, NonEmptyList(to)),
      Email.Content("TEST from taskman", s"Hello at ${new DateTime}.")
    ))
    val r = io.unsafePerformIO()
    log.info("DONE: {}", r)
    */

    } finally {
      ErrorOr.safe(db.shutdown()).leftMap(e => log.error("Error closing database connections.", e.throwable))
    }
  }
}
