package com.beardedlogic.usecase
package test

import lib.{SnippetHelpers}

class SnippetTester[S <: SnippetHelpers](val snippet: S) extends TestHelpers {
  val mailer = new TestMailer

  snippet.mailer = mailer

  def assertEmail(emailFrags: Option[List[String]]) = {
    testListOfZeroOrOne(emailFrags, mailer.sent)(mail =>
      for (f <- emailFrags.get) mail.getContent.toString should include(f)
    )
    this
  }
}
