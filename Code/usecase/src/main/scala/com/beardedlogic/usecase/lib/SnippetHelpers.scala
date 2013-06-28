package com.beardedlogic.usecase.lib

import net.liftweb.common.Logger
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.{ResponseShortcutException, LiftResponse}
import net.liftweb.util.{Mailer, Props}
import net.liftweb.util.Mailer.{MailTypes, From, Subject}
import com.beardedlogic.usecase.lib.HttpResponses.ShouldNeverHappenResponse
import com.beardedlogic.usecase.lib.security.Oshiro
import com.beardedlogic.usecase.lib.msg.{JavaScriptReaction, Reactor}
import com.beardedlogic.usecase.model.DAO
import com.beardedlogic.usecase.lib.db.DaoProvider
import com.beardedlogic.usecase.app.AppConfig

/**
 * @since 11/06/2013
 */
trait SnippetHelpers extends Misc with Logger {

  @inline implicit final def jsExpToJsCmd(in: JsExp) = in.cmd

  type JsCallback = () => JsCmd

  def jsCallback(f: => Reactor => Any): JsCallback = () => JavaScriptReaction(f(_))

  def jsCallbackWithDao(f: => (Reactor, DAO) => Any): JsCallback =
    jsCallback(r =>
      daoProvider.withTransaction(dao =>
        f(r, dao)
      ))

  // TODO typo: should be respond
  def responseImmediately(response: LiftResponse): Nothing = throw ResponseShortcutException.shortcutResponse(response)

  def shouldNeverHappen_! = responseImmediately(ShouldNeverHappenResponse())

  def loggedInUser = Oshiro.loggedInUser

  var daoProvider: DaoProvider = DAO

  type Mail = (Subject, List[MailTypes])
  var mailer: Mailer = Mailer
  def defaultMailFrom = From(AppConfig.MailFromAddress)
  def sendMail(subject: Subject, rest: MailTypes*): Unit = mailer.sendMail(defaultMailFrom, subject, rest: _*)
  def sendMail(mail: Mail, additional: MailTypes*): Unit = sendMail(mail._1, (mail._2 ++ additional): _*)
}
