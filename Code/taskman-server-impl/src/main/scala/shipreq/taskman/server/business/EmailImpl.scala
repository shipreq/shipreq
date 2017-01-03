package shipreq.taskman.server.business

import japgolly.microlibs.config.{Config, ConfigParser}
import javax.mail._
import javax.mail.internet.{InternetAddress, MimeMessage}
import scalaz.{-\/, Traverse, \/-}
import scalaz.effect.IO
import scalaz.old.NonEmptyList
import scalaz.std.list._
import scalaz.syntax.bind._
import shipreq.base.util.ErrorOr
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.server.Deterministic
import Bop.SendEmail
import Email._

object EmailImpl extends HasLogger {

  val getParsed: PartialFunction[AnyRef, Address] = {
    case a: Address => a
  }

  def parse1(ea: EmailAddr): ErrorOr[Address] =
    ErrorOr.catchAndTag(Deterministic) {
      val as = InternetAddress.parse(ea.value)
      if (as.size == 1)
        ErrorOr(as.head)
      else
        ErrorOr error s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}"
    }

  def parseN(s: String): ErrorOr[List[Address]] =
    ErrorOr.safeT(Deterministic)(InternetAddress.parse(s).toList)

  object ConfigParsers {

    implicit def parseAddr1(implicit p: ConfigParser[String]): ConfigParser[Addr] =
      p.mapAttempt { s =>
        val ea = EmailAddr(s)
        parse1(ea).bimap(_.msg, p => Addr(ea, Some(p)))
      }

    implicit def parseAddrN(implicit p: ConfigParser[String]): ConfigParser[List[Addr]] =
      p.mapAttempt(parseN(_).bimap(_.msg, _.map(a => Addr(EmailAddr(a.toString), Some(a)))))

    implicit def parseAddrNEL(implicit p: ConfigParser[String]): ConfigParser[NonEmptyList[Addr]] =
      parseAddrN.mapAttempt {
        case Nil    => -\/("At least one address required.")
        case h :: t => \/-(NonEmptyList.nel(h, t))
      }

    // TODO What's the difference between a ConfigParser and a Config ?
    // Config has a key; ap only
    // Parser has no key; monad
    def configEnvelopeFront(implicit p: ConfigParser[String]): Config[EnvelopeFront] =
      (
        Config.need[NonEmptyList[Addr]]("to") |@|
        Config.getOrUse[List[Addr]]("cc", Nil) |@|
        Config.getOrUse[List[Addr]]("bcc", Nil)
      )(EnvelopeFront)

    def configEnvelope(implicit p: ConfigParser[String]): Config[Envelope] =
      (configEnvelopeFront |@| Config.need[Addr]("from"))(_ from _)
  }

  implicit class EAExt(val ea: Addr) extends AnyVal {
    def parsed = ea.tryParse(getParsed, parse1)
  }

  implicit class EAExtF[F[_]](val f: F[Addr]) extends AnyVal {
    def parsed(implicit F: Traverse[F]): ErrorOr[F[Address]] = F.traverse[ErrorOr, Addr, Address](f)(_.parsed)
  }
}

final class EmailImpl(val mailSession: Session) extends HasLogger {
  import EmailImpl._

  val charset = "UTF-8"

  def buildEmail(e: Envelope, c: Content): ErrorOr[MimeMessage] = {
    val r = for {
      from <- e.from.parsed
      to   <- e.to.parsed
      cc   <- e.cc.parsed
      bcc  <- e.bcc.parsed
    } yield {
      val m = new MimeMessage(mailSession)
      m.setSentDate(new java.util.Date)
      ErrorOr.safeT(Deterministic) {
        m.setFrom(from)
        m.setRecipients(Message.RecipientType.TO, to.list.toArray)
        m.setRecipients(Message.RecipientType.CC, cc.toArray)
        m.setRecipients(Message.RecipientType.BCC, bcc.toArray)
        m.setSubject(c.subject, charset)
        m.setText(c.body, charset)
        m
      }
    }
    r.join
  }

  def send(op: SendEmail): IOE[Unit] = IO(
    buildEmail(op.e, op.c).map(m => {
      Transport.send(m)
      log.info.z(s"Email sent: ${op.e.to.head} [${op.c.subject}]")
    })
  )
}

