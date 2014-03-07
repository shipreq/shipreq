package shipreq.webapp.lib

import net.liftweb.util.Mailer.{PlainMailBodyType, From, MailTypes, Subject, To}
import scalaz.NonEmptyList
import shipreq.webapp.app.{DI, AppConfig}
import MailHelpers.{MailContent, AddressedMail}

object MailHelpers extends MailHelpers with MailCompositionHelpers with DI {

  val defaultMailFrom = From(AppConfig.MailFromAddress)

  case class MailContent(subject: Subject, rest: List[MailTypes]) {
    def addressedTo(emailAddress: String) =
      AddressedMail(defaultMailFrom, NonEmptyList(To(emailAddress)), subject, rest)
  }

  case class AddressedMail(from: From, to: NonEmptyList[To], subject: Subject, rest: List[MailTypes]) {
    def allPartsMinusFromAndSubject: List[MailTypes] = to.head :: to.tail ::: rest
  }
}

trait MailHelpers {
  this: DI =>

  def sendMail(m: AddressedMail): Unit =
    mailer.sendMail(m.from, m.subject, m.allPartsMinusFromAndSubject: _*)

  def sendMailSync(m: AddressedMail): Unit =
    mailer.blockingSendMail(m.from, m.subject, m.allPartsMinusFromAndSubject: _*)
}

trait MailCompositionHelpers {

  def plainTextMail(subj: String, body: String) =
    MailContent(Subject(subj), List(PlainMailBodyType(body)))

}