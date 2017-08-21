package shipreq.taskman.server.app

import java.time.Instant
import scalaz.old.NonEmptyList
import shipreq.taskman.api._
import shipreq.base.util.Error
import shipreq.base.util.ErrorOr.Implicits._
import shipreq.base.util.FxModule._
import shipreq.base.util.TaggedTypes._
import shipreq.taskman.server._
import shipreq.taskman.server.business._
import MailingList.API._
import Support.API._

object Tmp extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx(ctx => Fx {
      //ctx.testConnections()

      println(ctx.config.mail)
      val op = Bop.SendEmail(
        Email.Envelope(
          from = Email.Addr(EmailAddr("david.barri@shipreq.com")),
          to = NonEmptyList(Email.Addr(EmailAddr("japgolly+test@gmail.com")))),
        Email.Content(
          subject = "Email Test",
          body = "Hello"))
      val result = ctx.email.send(op).unsafeRun()
      println(result)

      /*
      import javax.mail._
      import javax.mail.internet._

      val msg = new MimeMessage(ctx.email.mailSession)
      msg.setFrom(new InternetAddress("admin@example.com"))
//      msg.addRecipient(Message.RecipientType.TO, new InternetAddress("japgolly@gmail.com", "David Barri"))
      msg.addRecipient(Message.RecipientType.TO, new InternetAddress("shipreqcomcontact@shipreq.freshdesk.com"))
      msg.setSubject("TEST")
      msg.setText("BODY")
      msg.setSentDate(new java.util.Date)
      msg.setSender(new InternetAddress("ex-1@blah.com"))
      msg.setReplyTo(Array(new InternetAddress("ex-2@blah.com")))
      msg.setHeader("On-Behalf-Of", "ex-3@blah.com")
      Transport.send(msg)
      */

      /*
      val mi = ctx.mailchimp
      val id = ctx.mailingListId
      log info "Ready...."

      val s = Subscription("tmp-mailchimp-app@shipreq.com".tag, "Tmp MailChimp App", true, AccountStatus.Never)

      val batch = mi.run(BatchSubscribe(id, NonEmptyList(s)))
      val sub   = mi.run(Subscribe(id, s, false))
      val upd   = mi.run(UpdateMember(id, s))

      val io = batch |>==> sub |>==> upd
      io.unsafePerformIO()
      */

      // val e = Sop.NotifySupportTaskmanError(DateTime.now, Error("Test from Tmp", new RuntimeException), None)
      // ctx.sopReifier(e).unsafePerformIO()

      /*
      val now = Instant.now()
      val m = MsgDetail(MsgHeader(MsgId(0), Priority.Medium, now), Msg.RegistrationCompleted(UserId(0)), 0)
      val f = Sop.NotifySupportWorkerFailed(now, m ,Error("Test from Tmp", new RuntimeException))
      ctx.sopReifier(f).unsafeRun()
      */
    }).unsafeRun()
}
