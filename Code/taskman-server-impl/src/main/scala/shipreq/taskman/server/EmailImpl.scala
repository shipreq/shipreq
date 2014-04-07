package shipreq.taskman.server

import java.util.Properties
import javax.mail._
import javax.mail.internet.{MimeMessage, InternetAddress}
import scalaz.effect.IO
import scalaz.std.list._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._
import shipreq.base.util.{JPropertiesValueReader, Error, ErrorOr, Logger}
import shipreq.base.util.ExternalValueReader._
import shipreq.taskman.api.Types._
import shipreq.taskman.server.business.Email
import shipreq.taskman.server.business.Bop.SendEmail

object EmailImpl extends Logger {

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
        log.info("SMTP account: {}", user)
        new Authenticator {
          override def getPasswordAuthentication =
            new PasswordAuthentication(user, need[String]("password"))
        }
      })
    }
    Session.getInstance(props, mailAuth getOrElse null)
  }
}

final class EmailImpl(ctx: EmailImpl.Ctx) extends Logger {
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

  def send(op: SendEmail): IOE[Unit] = IO(
    buildEmail(op.e, op.c).map(m => {
      Transport.send(m)
      log.info("Email sent: {} [{}]", op.e.to.head, op.c.subject, null)
    })
  )
}

