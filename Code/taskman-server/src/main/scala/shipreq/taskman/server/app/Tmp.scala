package shipreq.taskman.server.app

import java.time.Instant
import scalaz.Equal
import scalaz.old.NonEmptyList
import scalaz.syntax.bind._
import shipreq.taskman.api._
import shipreq.base.util.univeq._
import shipreq.base.util.FxModule._
import shipreq.base.util.TaggedTypes._
import shipreq.taskman.server._
import shipreq.taskman.server.business._
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.business._
import MailingList.API._
import Support.API._

object Tmp extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx(_ => Fx.unit).unsafeRun()
//    withTaskmanCtx(testFreshDesk).unsafeRun()
//    withTaskmanCtx(testMailChimp).unsafeRun()
//    withTaskmanCtx(testSendMail).unsafeRun()

//    withTaskmanCtx(ctx => Fx {
//      //ctx.testConnections()
//
//      /*
//      import javax.mail._
//      import javax.mail.internet._
//
//      val msg = new MimeMessage(ctx.email.mailSession)
//      msg.setFrom(new InternetAddress("admin@example.com"))
////      msg.addRecipient(Message.RecipientType.TO, new InternetAddress("japgolly@gmail.com", "David Barri"))
//      msg.addRecipient(Message.RecipientType.TO, new InternetAddress("shipreqcomcontact@shipreq.freshdesk.com"))
//      msg.setSubject("TEST")
//      msg.setText("BODY")
//      msg.setSentDate(new java.util.Date)
//      msg.setSender(new InternetAddress("ex-1@blah.com"))
//      msg.setReplyTo(Array(new InternetAddress("ex-2@blah.com")))
//      msg.setHeader("On-Behalf-Of", "ex-3@blah.com")
//      Transport.send(msg)
//      */
//
//      /*
//      val mi = ctx.mailchimp
//      val id = ctx.mailingListId
//      log info "Ready...."
//
//      val s = Subscription("tmp-mailchimp-app@shipreq.com".tag, "Tmp MailChimp App", true, AccountStatus.Never)
//
//      val batch = mi.run(BatchSubscribe(id, NonEmptyList(s)))
//      val sub   = mi.run(Subscribe(id, s, false))
//      val upd   = mi.run(UpdateMember(id, s))
//
//      val io = batch |>==> sub |>==> upd
//      io.unsafePerformIO()
//      */
//
//      // val e = Sop.NotifySupportTaskmanError(DateTime.now, Error("Test from Tmp", new RuntimeException), None)
//      // ctx.sopReifier(e).unsafePerformIO()
//
//      /*
//      val now = Instant.now()
//      val m = MsgDetail(MsgHeader(MsgId(0), Priority.Medium, now), Msg.RegistrationCompleted(UserId(0)), 0)
//      val f = Sop.NotifySupportWorkerFailed(now, m ,Error("Test from Tmp", new RuntimeException))
//      ctx.sopReifier(f).unsafeRun()
//      */
//    }).unsafeRun()

  private def assertResult[A: Equal](name: String, expect: A, actual: A): Unit = {
    import Console._
    if (Equal[A].equal(expect, actual))
      println(s"$GREEN$BOLD✓$RESET $name ($actual)")
    else
      println(s"$RED$BOLD✘$RESET $RED$name$RESET ($RED$actual$RESET) ≠ $expect")
  }

  protected def testFreshDesk(ctx: TaskmanCtx): Fx[Unit] =
    ctx.freshdesk(Support.API.ReportFailure("Manual test", "Hi. This is a test", Support.Priority.Low))
      .unsafeTap(r => println(s"Raised ticket: $r"))
      .void

  protected def testMailChimp(ctx: TaskmanCtx): Fx[Unit] = {
    import MailingList._

    val listId = ctx.mailingListId

    val subscription = Subscription(
      EmailAddr(s"japgolly+${System.currentTimeMillis}@gmail.com"),
      "David Barri",
      false,
      AccountStatus.Active)

    val subscribe = ctx.mailchimp(API.Subscribe(listId, subscription, sendConfEmail = false))
    val update = ctx.mailchimp(API.UpdateMember(listId, subscription.copy(newsletter = true)))

    for {
      u1 <- update
      s1 <- subscribe
      s2 <- subscribe
      u2 <- update
    } yield {
      assertResult("Update non-existing", NotSubscribed, u1)
      assertResult("Subscribe new", Ok, s1)
      assertResult("Subscribe existing", AlreadySubscribed, s2)
      assertResult("Update existing", Ok, u2)
    }
  }

  protected def testSendMail(ctx: TaskmanCtx): Fx[Unit] = Fx {
    println("Mail Config: " + ctx.config.mail)
    val op = ctx.emails.sendToUser(
      Email.Addr(EmailAddr("japgolly+test@gmail.com")),
      Email.Content(
        subject = "Email Test",
        body = s"Hello!\n\nThis was sent from ${getClass.getCanonicalName}"))
    ctx.sendMail(op).unsafeRun()
  }
}
