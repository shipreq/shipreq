package shipreq.taskman.server

import java.util.Properties
import javax.mail._
import javax.mail.internet.{MimeMessage, InternetAddress}
import scalaz.NonEmptyList
import scalaz.effect.IO
import scalaz.std.list._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._
import shipreq.base.util.{JPropertiesValueReader, Error, ErrorOr}
import shipreq.base.util.log.HasLogger
import shipreq.base.util.ExternalValueReader._
import shipreq.taskman.api.Types
import shipreq.taskman.server.business.Bop.SendEmail
import shipreq.taskman.server.business.Email._

object EmailImpl extends HasLogger {

  type EA = ErrorOr[Address]

  trait Ctx {
    def mailSession: Session
    val mailCharset = "UTF-8"
  }

  def loadSession(props: Properties): Session = {
    val pr = JPropertiesValueReader(props)
    import pr._

    val mailAuth: Option[Authenticator] = {
      implicit def scope = scopeByNS("mail")
      getO[String]("user").map(user => {
        log.info.z(s"SMTP account: $user")
        new Authenticator {
          override def getPasswordAuthentication =
            new PasswordAuthentication(user, need[String]("password"))
        }
      })
    }
    Session.getInstance(props, mailAuth getOrElse null)
  }

  def envelopeLoader(implicit rea: Retriever[EA]): Retriever[Envelope[EA]] =
    Retriever[Envelope[EA]](k => {
      implicit val s = PropScope(n => s"$k.$n")
      def get(n: String) = validate(n, need[EA])(valTestNotError)
      val from = get("from")
      val to   = get("to")
      Some(ErrorOr(Envelope(from, NonEmptyList(to))))
    })

  // TODO memo with LRU cache ?
  case object AddressParser extends AddrParser[EA] {
    override def apply(ea: Types.EmailAddr): EA =
      ErrorOr.catchAndTag(Deterministic) {
        val as = InternetAddress.parse(ea)
        if (as.size == 1)
          ErrorOr(as.head)
        else
          ErrorOr error s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}"
      }
  }
}

final class EmailImpl(ctx: EmailImpl.Ctx) extends HasLogger {
  import EmailImpl.EA
  import ctx._

  def buildEmail(e: Envelope[EA], c: Content): ErrorOr[MimeMessage] = {
    val r = for {
      from <- e.from
      to   <- e.to.sequence[ErrorOr, Address]
      cc   <- e.cc.sequence[ErrorOr, Address]
      bcc  <- e.bcc.sequence[ErrorOr, Address]
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

  def send(op: SendEmail[EA]): IOE[Unit] = IO(
    buildEmail(op.e, op.c).map(m => {
      Transport.send(m)
      log.info.z(s"Email sent: ${op.e.to.head} [${op.c.subject}]")
    })
  )
}

