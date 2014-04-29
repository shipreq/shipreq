package shipreq.taskman.server.business

import java.util.Properties
import javax.mail._
import javax.mail.internet.{MimeMessage, InternetAddress}
import scalaz.{Traverse, NonEmptyList}
import scalaz.effect.IO
import scalaz.std.list._
import scalaz.syntax.bind._
import scalaz.syntax.traverse._
import shipreq.base.util.{JPropertiesValueReader, ErrorOr}
import shipreq.base.util.effect.IOE
import shipreq.base.util.log.HasLogger
import shipreq.base.util.ExternalValueReader._
import shipreq.taskman.api.Types._
import shipreq.taskman.server.Deterministic
import Bop.SendEmail
import Email._

object EmailImpl extends HasLogger {

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

  val getParsed: PartialFunction[AnyRef, Address] = {
    case a: Address => a
  }

  val parser: EmailAddr => ErrorOr[Address] = // TODO memoise?
    ea => ErrorOr.catchAndTag(Deterministic) {
      val as = InternetAddress.parse(ea)
      if (as.size == 1)
        ErrorOr(as.head)
      else
        ErrorOr error s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}"
    }

  def addressLoader(implicit rs: Retriever[String]): Retriever[Addr] =
    rs.emap(s => {
      val ea = s.tag[IsEmailAddr]
      parser(ea).map(p => Addr(ea, Some(p)))
    })

  def envelopeLoader(implicit rea: Retriever[Addr]): Retriever[Envelope] =
    envelopeLoaderG[Envelope](get => envelopeFrontLoaderF(get).from(get("from")))

  def envelopeFrontLoader(implicit rea: Retriever[Addr]): Retriever[EnvelopeFront] =
    envelopeLoaderG[EnvelopeFront](envelopeFrontLoaderF)

  private[this] def envelopeFrontLoaderF(get: (String => Addr)) : EnvelopeFront = {
    val to = get("to")
    EnvelopeFront(NonEmptyList(to))
  }

  private[this] def envelopeLoaderG[E](f: (String => Addr) => E)(implicit rea: Retriever[Addr]): Retriever[E] =
    Retriever[E](k => {
      implicit val s = PropScope(n => s"$k.$n")
      def get(n: String) = need[Addr](n)
      Some(ErrorOr(f(get)))
    })

  implicit class EAExt(val ea: Addr) extends AnyVal {
    def parsed = ea.tryParse(getParsed, parser)
  }

  implicit class EAExtF[F[_]](val f: F[Addr]) extends AnyVal {
    def parsed(implicit F: Traverse[F]): ErrorOr[F[Address]] = F.traverse[ErrorOr, Addr, Address](f)(_.parsed)
  }
}

final class EmailImpl(mailSession: Session) extends HasLogger {
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
        m.setRecipients(Message.RecipientType.TO, Array(to.toList: _*))
        m.setRecipients(Message.RecipientType.CC, Array(cc.toList: _*))
        m.setRecipients(Message.RecipientType.BCC, Array(bcc.toList: _*))
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

