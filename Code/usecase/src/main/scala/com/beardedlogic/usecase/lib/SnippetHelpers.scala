package com.beardedlogic.usecase.lib

import net.liftweb.common.Logger
import net.liftweb.http.js.{JsCmd, JsExp}
import com.beardedlogic.usecase.lib.msg.{JavaScriptReaction, Reactor}
import com.beardedlogic.usecase.model.DAO
import net.liftweb.http.{ResponseShortcutException, LiftResponse}
import com.beardedlogic.usecase.lib.HttpResponses.ShouldNeverHappenResponse
import com.beardedlogic.usecase.lib.security.Oshiro

/**
 * @since 11/06/2013
 */
trait SnippetHelpers extends Logger {

  @inline implicit final def jsExpToJsCmd(in: JsExp) = in.cmd

  type JsCallback = () => JsCmd

  def jsCallback(f: => Reactor => Any): JsCallback = () => JavaScriptReaction(f(_))

  def jsCallbackWithDao(f: => (Reactor, DAO) => Any): JsCallback =
    jsCallback(r =>
      DAO.withTransaction(dao =>
        f(r, dao)
      ))

  def responseImmediately(response: LiftResponse): Nothing = throw ResponseShortcutException.shortcutResponse(response)

  def shouldNeverHappen_! = responseImmediately(ShouldNeverHappenResponse())

  def loggedInUser = Oshiro.loggedInUser
}
