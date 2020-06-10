package shipreq.taskman.server.business

import japgolly.clearconfig._
import japgolly.microlibs.nonempty.NonEmptyVector
import javax.mail._
import javax.mail.internet.{InternetAddress, MimeMessage}
import scala.runtime.AbstractFunction1
import scalaz.std.list._
import scalaz.syntax.bind._
import scalaz.{-\/, Traverse, \/, \/-}
import shipreq.base.util.ArticulateError
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.EmailAddr
import shipreq.taskman.server.logic.business.BusinessOp
import shipreq.taskman.server.logic.business.BusinessOp.SendEmail
import shipreq.taskman.server.logic.business.Email._

object JavaMail extends HasLogger {

  val getParsed: PartialFunction[AnyRef, Address] = {
    case a: Address => a
  }

  def parse1(ea: EmailAddr): ArticulateError \/ Address =
    ArticulateError.safe {
      val as = InternetAddress.parse(ea.value)
      if (as.size == 1)
        \/-(as.head)
      else
        -\/(ArticulateError(s"Email address '$ea' is expected to parse into a single address, but parsed into ${as.toList}"))
    }.leftMap(_.tagDeterministic)

  def parseN(s: String): ArticulateError \/ List[Address] =
    ArticulateError.attempt(InternetAddress.parse(s).toList)
      .leftMap(_.tagDeterministic)

  object ConfigValueParsers {

    implicit def parseAddr1: ConfigValueParser[Addr] =
      ConfigValueParser.id.mapAttempt { s =>
        val ea = EmailAddr(s)
        parse1(ea).bimap(_.getMessage, p => Addr(ea, Some(p)))
      }

    implicit def parseAddrN: ConfigValueParser[List[Addr]] =
      ConfigValueParser.id
        .mapAttempt(parseN(_).bimap(_.getMessage, _.map(a => Addr(EmailAddr(a.toString), Some(a)))))

    implicit def parseAddrNEL: ConfigValueParser[NonEmptyVector[Addr]] =
      parseAddrN.mapAttempt {
        case Nil    => -\/("At least one address required.")
        case h :: t => \/-(NonEmptyVector(h, t.toVector))
      }

    def configEnvelopeFront: ConfigDef[EnvelopeFront] =
      ( ConfigDef.need[NonEmptyVector[Addr]]("to") |@|
        ConfigDef.getOrUse[List[Addr]]("cc", Nil) |@|
        ConfigDef.getOrUse[List[Addr]]("bcc", Nil)
      )(EnvelopeFront)

    def configEnvelope: ConfigDef[Envelope] =
      (configEnvelopeFront |@| ConfigDef.need[Addr]("from"))(_ from _)
  }

  implicit class EAExt(val ea: Addr) extends AnyVal {
    def parsed = ea.tryParse(getParsed, parse1)
  }

  implicit class EAExtF[F[_]](val f: F[Addr]) extends AnyVal {
    def parsed(implicit F: Traverse[F]): ArticulateError \/ F[Address] =
      F.traverse[ArticulateError \/ ?, Addr, Address](f)(_.parsed)
  }
}

final class JavaMail(val mailSession: Session) extends AbstractFunction1[BusinessOp.SendEmail, Fx[Unit]] with HasLogger {
  import JavaMail._

  private[this] final val charset = "UTF-8"

  private def buildEmail(e: Envelope, c: Content): ArticulateError \/ MimeMessage = {
    val r = for {
      from <- e.from.parsed
      to   <- e.to.parsed
      cc   <- e.cc.parsed
      bcc  <- e.bcc.parsed
    } yield {
      val m = new MimeMessage(mailSession)
      m.setSentDate(new java.util.Date)
      ArticulateError.attempt {
        m.setFrom(from)
        m.setRecipients(Message.RecipientType.TO, to.iterator.toArray)
        m.setRecipients(Message.RecipientType.CC, cc.toArray)
        m.setRecipients(Message.RecipientType.BCC, bcc.toArray)
        m.setSubject(c.subject, charset)
        m.setText(c.body, charset)
        m
      }.leftMap(_.tagDeterministic)
    }
    r.join
  }

  override def apply(op: SendEmail): Fx[Unit] =
    for {
      m <- Fx.lift(buildEmail(op.envelope, op.content))
      _ <- Fx(Transport.send(m))
    } yield logger.info(s"Email sent: ${op.envelope.showTo} [${op.content.subject}]")
}

