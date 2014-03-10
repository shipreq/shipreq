package shipreq.webapp.test

import javax.mail.internet.MimeMessage
import net.liftweb.util.Mailer
import org.scalatest.Matchers
import shipreq.webapp.app.DI

object TestMailer {
  def install[R](f: => R): MailTestResult[R] = {
    val t = new TestMailer
    val r = DI.Mailer.doWith(t)(f)
    MailTestResult(t, r)
  }
}

/**
 * Test implementation of Lift's Mailer. Captures rather than sends email.
 */
class TestMailer extends Mailer {
  import Mailer._

  var sent = List.empty[MimeMessage]

  override def sendMail(from: From, subject: Subject, rest: MailTypes*) {
    blockingSendMail(from, subject, rest: _*)
  }

  override protected def performTransportSend(msg: MimeMessage) {
    sent :+= msg
  }
}

/**
 * Contains the result of an executed function along with the test mailer impl used and methods to assert email
 * functionality.
 */
case class MailTestResult[R](mailer: TestMailer, result: R) extends Matchers {

  def sent = mailer.sent

  def assertEmail(emailFrags: Option[List[String]]) = {
    TestHelpers.testListOfZeroOrOne(emailFrags, sent)(mail =>
      for (f <- emailFrags.get) mail.getContent.toString should include(f)
    )
    this
  }

  def assertSent(emailFrags: String*) = assertEmail(Some(List(emailFrags: _*)))

  def assertNothingSent() = assertEmail(None)

}
