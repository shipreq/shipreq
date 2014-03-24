package shipreq.taskman.server

import java.util.Properties
import scalaz.effect.IO
import shipreq.taskman.server.business.Bop.SendEmail
import org.joda.time.{Period, DateTime}

//import scalaz.~>
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
    val props = Props.loadUsingStandardStrategy(runMode)(new Properties)
    val propsR = JPropertiesValueReader(props)
    import propsR._

    // Init database
//    val db = new Db(propsR)
//    db.init()

    // Mail
    val mailAuth: Option[Authenticator] = {
      implicit def scope = scopeByNS("mail")

      getO[String]("user").map(user => {
        log.info("SMTP account: {}", user)
        new Authenticator {
          override def getPasswordAuthentication =
            new PasswordAuthentication(user, need[String]("password"))}
      })
    }
    val session = Session.getInstance(props, mailAuth getOrElse null)

    // Send an email
    val ctx: BopImplCtx = new BopImplCtx {
      override def mailSession: Session = session
    }
    val bopImpl = new BopImpl(ctx)

    val from: EmailAddr = "whatever@gmail.com".tag
    val to: EmailAddr = "japgolly+test@gmail.com".tag
    val io = bopImpl(SendEmail(
      Email.Envelope(from, NonEmptyList(to)),
      Email.Content("TEST from taskman", s"Hello at ${new DateTime}.")
    ))
    val r = io.unsafePerformIO()
    log.info("DONE: {}", r)
  }

  /*

  http://stackoverflow.com/questions/12732584/threadsafety-in-javamail

  Multiple threads can use a Session.

  Since a Transport represents a connection to a mail server, and only a single thread can use the connection at a time, a Transport will synchronize access from multiple threads to maintain thread safety, but you'll really only want to use it from a single thread.

  Similarly, a Store can be used by multiple threads, but access to the underlying connection will be synchronized and single threaded.

  A Message should only be modified by a single thread at a time, but multiple threads should be able to read a message safely (although it's not clear why you would want to do that).


   */

  trait BopImplCtx {
    def mailSession: Session
    val mailCharset = "UTF-8"
  }

  type FullCtx = Email.Ctx with BopImplCtx

  def ctx: FullCtx = ???
  def sop(): Sop ~> IO = ???

  def bop(): Bop ~> IOE = new BopImpl(ctx)
  def failurePolicy: FailurePolicy = Failure.failurePolicy

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

  import Bop._

  class BopImpl(ctx: BopImplCtx) extends (Bop ~> IOE) {
    import ctx._

    // TODO memo with LRU cache
    def parseEmailAddress(ea: EmailAddr): ErrorOr[Address] =
      ErrorOr.catchAndTag(Deterministic){
        val as = InternetAddress.parse(ea)
        if (as.size == 1)
          ErrorOr(as.head)
        else
          Error(s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}")
      }

    def buildEmail(e: Email.Envelope, c: Email.Content): ErrorOr[MimeMessage] = {
      val r = for {
        from <- parseEmailAddress(e.from)
        to   <- e.to.traverse[ErrorOr, Address](parseEmailAddress)
        cc   <- e.cc.traverse[ErrorOr, Address](parseEmailAddress)
        bcc  <- e.bcc.traverse[ErrorOr, Address](parseEmailAddress)
      } yield {
        val m = new MimeMessage(mailSession)
        m.setSentDate(new java.util.Date)
        ErrorOr.safeT(Deterministic) {
          m.setFrom(from)
          m.setRecipients(Message.RecipientType.TO, Array(to.toList: _*))
          m.setRecipients(Message.RecipientType.CC, Array(cc.toList: _*))
          m.setRecipients(Message.RecipientType.BCC, Array(bcc.toList: _*))
          m.setSubject(c.subject, mailCharset)
          m.setText(c.body, mailCharset)
          m
        }
      }
      r.join
    }

    val clock = IO{System.currentTimeMillis}

    override def apply[A](op: Bop[A]): IOE[A] = {
      for {
        _     <- IO{ log.debug("Starting {}", op) }
        start <- clock
        r     <- ErrorOr.catchExceptionM(apply2(op))
        end   <- clock
        _     <- IO{ r match {
                       case \/-(_) =>
                         log.info("{} completed in {}ms.", op.getClass.getSimpleName, end - start)
                       case -\/(e) =>
                         log.error("{} failed after {}ms with [{}]. Op: {}", op.getClass.getSimpleName, java.lang.Long.valueOf(end - start), Error.msg(e), op)
                 }}
      } yield r
    }

    def apply2[A](op: Bop[A]): IOE[A] = op match {

      case SendEmail(env, content) => IO(
        buildEmail(env, content) map { m =>
//          Transport.send(m)
          log.info("Email sent: {} [{}]", env.to.head, content.subject, null) // TODO
        }
      )

    }
  }

}
