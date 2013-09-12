package com.beardedlogic.usecase.lib

import scalaz.syntax.monoid._
import net.liftweb.common.{Failure, Full, Box, Logger, Empty}
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.{StatefulSnippet, ResponseShortcutException, LiftResponse}
import net.liftweb.json.{NoTypeHints, Serialization}
import net.liftweb.util.Mailer.{MailTypes, From, Subject}
import net.liftweb.util.{CssSel, Mailer}
import scala.xml.{Elem, Text, NodeSeq, UnprefixedAttribute}

import com.beardedlogic.usecase.app.AppConfig
import com.beardedlogic.usecase.lib.security.Oshiro
import com.beardedlogic.usecase.snippet.Notices
import com.beardedlogic.usecase.util.HttpResponses.ShouldNeverHappenResponse
import com.beardedlogic.usecase.util.JsExt._
import com.beardedlogic.usecase.util.ErrorMessages
import Types.JsCmdMonoid
import SnippetHelpers._

object SnippetHelpers extends StaticSnippetHelpers {
  final val DefaultAjaxErrorId = "ajaxErr"
}

trait StaticSnippetHelpers {

  def respondImmediately(response: LiftResponse): Nothing = throw ResponseShortcutException.shortcutResponse(response)

  def shouldNeverHappen_! = respondImmediately(ShouldNeverHappenResponse())

  def shouldNeverHappen_!(msg: String) = respondImmediately(ShouldNeverHappenResponse(msg))

  def loggedInUser = Oshiro.loggedInUser
}

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends StaticSnippetHelpers with Misc with DI with Logger {

  @inline implicit final def jsExpToJsCmd(in: JsExp) = in.cmd

  @inline implicit def ConvertStringToNode(i: String) = Text(i)
  @inline implicit def ConvertSeqStringToNodes(i: Seq[String]) = i.map(Text(_))
  @inline implicit def OptionToBox[T](option: Option[T]): Box[T] = Box(option)

  protected implicit lazy val jsonFormats = Serialization.formats(NoTypeHints)

  // -------------------------------------------------------------------------------------------------------------------

  type Mail = (Subject, List[MailTypes])
  var mailer: Mailer = Mailer
  def defaultMailFrom = From(AppConfig.MailFromAddress)
  def sendMail(subject: Subject, rest: MailTypes*): Unit = mailer.sendMail(defaultMailFrom, subject, rest: _*)
  def sendMail(mail: Mail, additional: MailTypes*): Unit = sendMail(mail._1, (mail._2 ++ additional): _*)

  // -------------------------------------------------------------------------------------------------------------------
  // Error propagation

  def jsClearError(id: String = DefaultAjaxErrorId): JsCmd = JqId(id) ~> JqRemove

  def jsShowError(errMsg: NodeSeq, id: String = DefaultAjaxErrorId): JsCmd =
    jsClearAndShowError(id, Notices.renderSingle(Notices.ErrorClasses, errMsg))

  def jsShowErrors(errMsgs: Seq[NodeSeq], id: String = DefaultAjaxErrorId): JsCmd = errMsgs match {
    case Nil                => jsClearError(id)
    case singleError :: Nil => jsShowError(singleError, id)
    case _                  => jsClearAndShowError(id, Notices.renderMsgs(Notices.ErrorClasses, errMsgs).asInstanceOf[Elem])
  }

  private def jsClearAndShowError(id: String, jsErrNode: => Elem): JsCmd =
    jsClearError(id) & {
      val errNode = jsErrNode % new UnprefixedAttribute("id", id, xml.Null)
      JqExpr(errNode) ~> JqAppendTo("#notices") ~> JqHighlight()
    }

  def jsPossibleError[T](box: Box[T], id: String = DefaultAjaxErrorId)(successJs: T => JsCmd): JsCmd = box match {
    case Full(v)            => jsClearError() |+| successJs(v)
    case Empty              => jsShowError(ErrorMessages.Generic, id)
    case Failure(err, _, _) => jsShowError(err, id)
  }
}

/**
 * A stateful snippet with only one rendering method.
 */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: CssSel
}