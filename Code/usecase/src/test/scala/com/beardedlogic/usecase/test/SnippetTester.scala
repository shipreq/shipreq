package com.beardedlogic.usecase.test

import com.beardedlogic.usecase.lib.SnippetHelpers
import com.beardedlogic.usecase.lib.msg.JavaScriptReaction

class SnippetTester[S <: SnippetHelpers](val snippet: S) {
  val js = new JavaScriptReaction
  val mailer = new TestMailer

  snippet.mailer = mailer

  def jsReaction = js.result.toJsCmd
}

