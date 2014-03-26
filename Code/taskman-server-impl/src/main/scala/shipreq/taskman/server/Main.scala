package shipreq.taskman.server

import java.util.Properties
import shipreq.base.util._
import shipreq.base.util.ExternalValueReader._
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.CfgKeys
import shipreq.taskman.server.business.{BusinessLogic, Failure, Email}

//==========================================================================================

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._

  override protected def newConnection = DatabaseConnection.establish_!()

  def slick = _slick
}

//==========================================================================================

trait TaskmanCtx extends Email.Ctx with EmailImpl.Ctx with BopImpl.Ctx {
  protected def mailProps: Properties
  protected def fromDb: StringBasedValueReader
  protected implicit def scope: PropScope
  protected implicit def retrieverS: Retriever[String]

  override def mailSession = EmailImpl.loadSession(mailProps)
  override val defaultFromAddress = need[String]("mail.from").tag

  override val shipreq  = need[String](CfgKeys.Webapp.appName )(GlobalScope, fromDb.retrieverS)
  override val loginUrl = need[String](CfgKeys.Webapp.loginUrl)(GlobalScope, fromDb.retrieverS)

  override val emailer = new EmailImpl(this)

  def loggable = Map[String, Any](
    "defaultFromAddress" -> defaultFromAddress
    , "shipreq" -> shipreq
    , "loginUrl" -> loginUrl
  )
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

    val sopImpl = new SopImpl(db.slick)

    // Config 2
    val ctx: TaskmanCtx = new TaskmanCtx {
      override protected def mailProps = props
      override protected def fromDb = CfgValueReader(sopImpl)
      override protected implicit def scope = GlobalScope //scopeByNS("taskman")
      override protected implicit def retrieverS = propsR.retrieverS
    }
    log.debug("Config: {}", ctx.loggable)

    // Taskman
    // TODO worker/manager ctx
    val limit = 10
    val assignmentTrustPeriod = 5 minutes
    val bopImpl = new BopImpl(ctx)
    val failurePolicy = Failure.failurePolicy
    val msgProcessor = BusinessLogic(ctx, bopImpl)
    val nodeId = sopImpl.getNextNodeId.unsafePerformIO()

    log.info("Node ID is {}.", nodeId.value)

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

    db.shutdown()
  }
}
