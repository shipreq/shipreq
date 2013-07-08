package com.beardedlogic.usecase.lib

import net.liftweb.common.{Failure, Full, Box, Logger}
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.{StatefulSnippet, ResponseShortcutException, LiftResponse}
import net.liftweb.json.{NoTypeHints, Serialization}
import net.liftweb.util.Mailer.{MailTypes, From, Subject}
import net.liftweb.util.{CssSel, Mailer}
import scala.xml.{Elem, Text, NodeSeq, UnprefixedAttribute}

import com.beardedlogic.usecase.app.AppConfig
import com.beardedlogic.usecase.lib.security.Oshiro
import com.beardedlogic.usecase.model.DAO
import com.beardedlogic.usecase.snippet.Notices
import com.beardedlogic.usecase.util.HttpResponses.ShouldNeverHappenResponse
import com.beardedlogic.usecase.util.JsExt._
import com.beardedlogic.usecase.util.{ErrorMessages, Reactor, JavaScriptReaction, JavaScript}
import SnippetHelpers._

object SnippetHelpers {
  final val DefaultAjaxErrorId = "ajaxErr"
}

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends Misc with DI with Logger {

  @inline implicit final def jsExpToJsCmd(in: JsExp) = in.cmd

  @inline implicit def ConvertStringToNode(i: String) = Text(i)
  @inline implicit def ConvertSeqStringToNodes(i: Seq[String]) = i.map(Text(_))
  @inline implicit def OptionToBox[T](option: Option[T]): Box[T] = Box(option)

//  implicit class BoxExt[T](val box: Box[T]) extends AnyVal {
//    def ~~>(errMsg: String) = box ~> reactWithError(errMsg)
//    def ~~>(errMsg: NodeSeq) = box ~> reactWithError(errMsg)
//  }

  protected implicit val jsonFormats = Serialization.formats(NoTypeHints)

  // -------------------------------------------------------------------------------------------------------------------

  type JsCallback = () => JsCmd

  def jsCallback(f: => Reactor => Any): JsCallback = () => JavaScriptReaction(f(_))

  def jsCallbackWithDao(f: => (Reactor, DAO) => Any): JsCallback =
    jsCallback(r =>
      daoProvider.withTransaction(dao =>
        f(r, dao)
      ))

  def respondImmediately(response: LiftResponse): Nothing = throw ResponseShortcutException.shortcutResponse(response)

  def shouldNeverHappen_! = respondImmediately(ShouldNeverHappenResponse())

  def shouldNeverHappen_!(msg: String) = respondImmediately(ShouldNeverHappenResponse(msg))

  def loggedInUser = Oshiro.loggedInUser

  type Mail = (Subject, List[MailTypes])
  var mailer: Mailer = Mailer
  def defaultMailFrom = From(AppConfig.MailFromAddress)
  def sendMail(subject: Subject, rest: MailTypes*): Unit = mailer.sendMail(defaultMailFrom, subject, rest: _*)
  def sendMail(mail: Mail, additional: MailTypes*): Unit = sendMail(mail._1, (mail._2 ++ additional): _*)

  // -------------------------------------------------------------------------------------------------------------------
  // Error propagation

  def removeError(id: String = DefaultAjaxErrorId)(implicit reactor: Reactor) {
    reactor(JavaScript)(JqId(id) ~> JqRemove)
  }

  def reactWithError(errMsg: NodeSeq, id: String = DefaultAjaxErrorId)(implicit reactor: Reactor) =
    _reactWithError(id, Notices.renderSingle(Notices.ErrorClasses, errMsg))

  def reactWithErrors(errMsgs: Seq[NodeSeq], id: String = DefaultAjaxErrorId)(implicit reactor: Reactor) {
    errMsgs match {
      case Nil                => removeError(id)
      case singleError :: Nil => reactWithError(singleError, id)
      case _                  => _reactWithError(id, Notices.renderMsgs(Notices.ErrorClasses, errMsgs).asInstanceOf[Elem])
    }
  }

  private def _reactWithError(id: String, jsErrNode: => Elem)(implicit reactor: Reactor) {
    removeError(id)
    reactor(JavaScript) {
      val errNode = jsErrNode % new UnprefixedAttribute("id", id, xml.Null)
      JqExpr(errNode) ~> JqAppendTo("#notices") ~> JqHighlight()
    }
  }

  def reactToOptionalError(box: Box[_], id: String = DefaultAjaxErrorId)(implicit reactor: Reactor) {
    box match {
      case Full(_)            => removeError()
      case Failure(err, _, _) => reactWithError(err, id)
      case _                  => reactWithError(ErrorMessages.Generic, id)
    }
  }
}

/**
 * A stateful snippet with only one rendering method.
 */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: CssSel
}