package com.beardedlogic.usecase.test

import com.beardedlogic.usecase.lib.SnippetHelpers
import com.beardedlogic.usecase.lib.msg.JavaScriptReaction

class SnippetTester[S <: SnippetHelpers](val snippet: S) extends TestHelpers {
  val js = new JavaScriptReaction
  val mailer = new TestMailer

  snippet.mailer = mailer

  def jsReaction = js.result.toJsCmd

  def assertJsAlert(errorMsg: Option[String]) = {
    if (errorMsg.isDefined) {
      jsReaction.toLowerCase should include ("alert")
      jsReaction should include(errorMsg.get)
    } else
      jsReaction.toLowerCase should not include ("alert")
    this
  }

  def assertEmail(emailFrags: Option[List[String]]) = {
    testListOfZeroOrOne(emailFrags, mailer.sent)(mail =>
      for (f <- emailFrags.get) mail.getContent.toString should include(f)
    )
    this
  }
}
