package shipreq.taskman.server.app

import shipreq.taskman.server.business.MailingList._
import shipreq.taskman.server.business.MailingList.API._
import shipreq.taskman.api.Types._
import scalaz.NonEmptyList
import shipreq.base.util.ErrorOr.Implicits._

object Tmp extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx { ctx =>
      ctx.logContent()
      //ctx.testConnections()

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
    }
}
