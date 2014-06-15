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
import shipreq.base.util.TaggedTypes._
import shipreq.taskman.api.EmailAddr
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

  def parse1(ea: EmailAddr): ErrorOr[Address] =
    ErrorOr.catchAndTag(Deterministic) {
      val as = InternetAddress.parse(ea)
      if (as.size == 1)
        ErrorOr(as.head)
      else
        ErrorOr error s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}"
    }

  def parseN(s: String): ErrorOr[List[Address]] =
    ErrorOr.safeT(Deterministic)(InternetAddress.parse(s).toList)

  final class Retrievers(implicit rs: Retriever[String]) {

    implicit val addr1: Retriever[Addr] =
      rs.emap(s => {
        val ea = EmailAddr(s)
        parse1(ea).map(p => Addr(ea, Some(p)))
      })

    implicit val addrN: Retriever[List[Addr]] =
      rs.emap(parseN).map(as =>
          as.map(a => Addr(EmailAddr(a.toString), Some(a))))

    implicit val addrNEL: Retriever[NonEmptyList[Addr]] =
      addrN.emap {
        case Nil    => ErrorOr error "At least one address required."
        case h :: t => ErrorOr(NonEmptyList.nel(h, t))
      }

    implicit val envelopeFront: Retriever[EnvelopeFront] =
      Retriever[EnvelopeFront](k => {
        implicit val s = PropScope(n => s"$k.$n")
        for (
          toE <- getOE[NonEmptyList[Addr]]("to")
        ) yield for {
            to  <- toE
            cc  <- get[List[Addr]]("cc", Nil)
            bcc <- get[List[Addr]]("bcc", Nil)
          } yield
            EnvelopeFront(to, cc, bcc)
      })

    implicit val envelope: Retriever[Envelope] =
      Retriever[Envelope](k => {
        implicit val s = PropScope(n => s"$k.$n")
        (getOE[Addr]("from"), envelopeFront.run(k)) match {
          case (None, None)       => None
          case (Some(_), None)    => Some(ErrorOr error "'from' specified without 'to'.")
          case (None, Some(_))    => Some(ErrorOr error "'to' specified without 'from'.")
          case (Some(a), Some(b)) => Some(for {f <- a; env <- b} yield env.from(f))
        }
      })
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

