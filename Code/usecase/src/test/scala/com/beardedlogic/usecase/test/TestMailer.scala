package com.beardedlogic.usecase.test

import javax.mail.internet.MimeMessage
import net.liftweb.util.Mailer
import Mailer._

class TestMailer extends Mailer {
  var sent = List.empty[MimeMessage]

  override def sendMail(from: From, subject: Subject, rest: MailTypes*) {
    blockingSendMail(from, subject, rest: _*)
  }

  override protected def performTransportSend(msg: MimeMessage) {
    sent :+= msg
  }
}
