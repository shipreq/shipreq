package shipreq.taskman.server

import java.util.Properties
import scalaz.effect.IO
import shipreq.taskman.server.business.Bop.SendEmail
import org.joda.time.{Period, DateTime}
import scalaz._, Scalaz._
import shipreq.base.util._
import shipreq.base.db.{DatabaseConnection, DbTemplate}
import shipreq.taskman.server.business.{Failure, Email, Bop}
import shipreq.taskman.server.Worker.FailurePolicy
import shipreq.taskman.api.Types._
import javax.mail._
import javax.mail.internet._

class Db(props: StringBasedValueReader) extends DbTemplate {
  import props._

  override protected lazy val connection = DatabaseConnection.establish_!()

  def slick = _slick
}

object Main {

  val log = Logger.forClass(getClass)

  def main(args: Array[String]) {

    import shipreq.base.util.ExternalValueReader._

    implicit def scope = GlobalScope

    // Determine run mode
    implicit val _rm = RunMode.retrieverFromSysProps
    val runMode: RunMode = tryNeed("run.mode", RunMode.detectFromStackTrace())
    log.info("Run mode: {}", runMode)

    // Config
    implicit val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
    val propsR = JPropertiesValueReader(props)
    import propsR._

    // Init database
//    val db = new Db(propsR)
//    db.init()

    // Mail
    val ctx = EmailImpl.ctx
    val emailer = new EmailImpl(ctx)
    val bopImpl = new BopImpl(emailer)

    // Send an email
    val from: EmailAddr = "whatever@gmail.com".tag
    val to: EmailAddr = "japgolly+test@gmail.com".tag
    val io = bopImpl(SendEmail(
      Email.Envelope(from, NonEmptyList(to)),
      Email.Content("TEST from taskman", s"Hello at ${new DateTime}.")
    ))
    val r = io.unsafePerformIO()
    log.info("DONE: {}", r)
  }

//  class FullCtx(
//    override val shipreq: String,
//    override val loginUrl: String,
//    override val defaultFromAddress: EmailAddr,
//    override val mailSession: Session) extends Email.Ctx with BopImpl.Ctx

//  def sop(): Sop ~> IO = ???

//  def bop(): Bop ~> IOE = new BopImpl(ctx)
//  def failurePolicy: FailurePolicy = Failure.failurePolicy

  /*
  Manager
    limit: Int
    assignmentTrustPeriod: Period
    NodeId
    Sop ~> IO

  Worker
    worker: WorkerId
    NodeId
    Sop ~> IO
    FailurePolicy
    MsgProcessor

  BusinessLogic
    Email.Ctx
    Bop ~> IOE

   */
}
