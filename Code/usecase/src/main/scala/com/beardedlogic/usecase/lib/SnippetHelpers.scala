package com.beardedlogic.usecase.lib

import scalaz.syntax.monoid._
import net.liftweb.common.{ParamFailure, Failure, Full, Box, Logger, Empty}
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{S, NotFoundResponse, RedirectResponse, StatefulSnippet, ResponseShortcutException, LiftResponse}
import net.liftweb.json.{NoTypeHints, Serialization, Serializer}
import net.liftweb.sitemap.Menu
import net.liftweb.util.Mailer.{MailTypes, From, Subject}
import net.liftweb.util.{Props, CssSel, Mailer}
import scala.xml.{Elem, Text, NodeSeq, UnprefixedAttribute}

import com.beardedlogic.usecase.app.{DI, AppConfig, AppSiteMap}
import com.beardedlogic.usecase.db.UserDescriptor
import com.beardedlogic.usecase.snippet.{AlertTypeSuccess, AlertTypeError, Notices}
import com.beardedlogic.usecase.util.HttpResponses.ShouldNeverHappenResponse
import com.beardedlogic.usecase.util.JsExt._
import com.beardedlogic.usecase.util.ErrorMessages
import AppSiteMap.Implicits._
import Types._

// TODO Needs rework between static & stateful

object SnippetHelpers extends StaticSnippetHelpers {
  final val DefaultAjaxErrorId = "ajaxErr"

  final val JqExprJsonSerializer: Serializer[JqExpr] = new Serializer[JqExpr] {
    import net.liftweb.json._
    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), JqExpr] = ???
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case expr: JqExpr => JString(expr.toJsCmd)
    }
  }

  final val DefaultJsonFormat = Serialization.formats(NoTypeHints) + JqExprJsonSerializer
}

trait StaticSnippetHelpers extends Logger {

  def redirectHome                                      : Nothing = S.redirectTo(AppSiteMap.HomeRelativeUrl)
  def redirectTo(page: Menu)                            : Nothing = S.redirectTo(page.relativeUrl)
  def redirectTo(page: Menu.Menuable)                   : Nothing = S.redirectTo(page.relativeUrl)
  def redirectTo[T](page: Menu.ParamMenuable[T])(arg: T): Nothing = S.redirectTo(page.relativeUrl(arg))

  def respondImmediately(response: LiftResponse): Nothing = throw ResponseShortcutException.shortcutResponse(response)

  def shouldNeverHappen_! = respondImmediately(ShouldNeverHappenResponse())

  def shouldNeverHappen_!(msg: String) = respondImmediately(ShouldNeverHappenResponse(msg))

  def shouldNeverHappen_swallowInProd[T](fallback: T)(msg: String): T = {
    import Props.RunModes._
    Props.mode match {
      case Production | Pilot | Staging =>
        error(msg)
        fallback
      case Test | Development | Profile =>
        shouldNeverHappen_!(msg)
    }
  }

  def requireResultO_![T](o: Option[T], fallbackErrorReaction: => Nothing = redirectHome): T = o match {
    case Some(t) => t
    case None    => fallbackErrorReaction
  }

  def requireResult_![T](box: Box[T], fallbackErrorReaction: => Nothing = redirectHome): T = box match {
    case Full(t)                                 => t
    case Empty                                   => fallbackErrorReaction
    case ParamFailure(_, _, _, r: LiftResponse)  => respondImmediately(r)
    case ParamFailure(_, _, _, m: Menu)          => redirectTo(m)
    case ParamFailure(_, _, _, m: Menu.Menuable) => redirectTo(m)
    case ParamFailure(_, _, _, NotFoundResponse) => respondImmediately(NotFoundResponse())
    case _                                       => error(s"Don't know how to react to $box"); shouldNeverHappen_!
  }

}

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends StaticSnippetHelpers with Misc with DI with Logger {
  import SnippetHelpers._

  @inline implicit final def jsExpToJsCmd(in: JsExp) = in.cmd

  @inline implicit def ConvertStringToNode(i: String) = Text(i)
  @inline implicit def ConvertSeqStringToNodes(i: Seq[String]) = i.map(Text(_))
  @inline implicit def OptionToBox[T](option: Option[T]): Box[T] = Box(option)

  protected implicit lazy val jsonFormats = DefaultJsonFormat

  def toJson[T <: AnyRef](data: T): Json[T] = Serialization.write(data).tag[IsJsonFor[T]]

  final def currentUser_!(): UserDescriptor = securityProvider.loggedInUser match {
    case Some(user) => user
    case None => respondImmediately(RedirectResponse(AppSiteMap.Login.relativeUrl))
  }

  @inline final def currentUserId_!() : UserId = currentUser_!.id

  // -------------------------------------------------------------------------------------------------------------------

  type Mail = (Subject, List[MailTypes])
  var mailer: Mailer = Mailer
  def defaultMailFrom = From(AppConfig.MailFromAddress)
  def sendMail(subject: Subject, rest: MailTypes*): Unit = mailer.sendMail(defaultMailFrom, subject, rest: _*)
  def sendMail(mail: Mail, additional: MailTypes*): Unit = sendMail(mail._1, (mail._2 ++ additional): _*)

  // -------------------------------------------------------------------------------------------------------------------
  // Error propagation and Alerts

  sealed trait AlertIdTag extends TypeTag[String]
  type AlertId = String @@ AlertIdTag

  @inline private def defaultErrAlertId = DefaultAjaxErrorId.tag[AlertIdTag]

  @inline private def appendAlert(alert: NodeSeq): JsCmd =
    JqExpr(alert) ~> JqAppendTo("#notices") ~> JqHighlight()

  private def applyAlertId(alert: NodeSeq)(implicit id: AlertId): NodeSeq =
    if (id eq null) alert
    else alert match {
      case NodeSeq.Empty => NodeSeq.Empty
      case e: Elem => e % new UnprefixedAttribute("id", id, xml.Null)
      case _ => warn("Don't know how to add id to: " + alert.getClass); alert
    }

  @inline private def jsClearAndShowError(alert: => NodeSeq)(implicit id: AlertId): JsCmd =
    jsClearError |+| appendAlert(applyAlertId(alert))

  def jsClearError(implicit id: AlertId = defaultErrAlertId): JsCmd =
    JqId(id) ~> JqRemove

  def jsShowError(errMsg: NodeSeq)(implicit id: AlertId = defaultErrAlertId): JsCmd =
    jsClearAndShowError(Notices.renderSingle(AlertTypeError, errMsg))

  def jsShowErrors(errMsgs: Seq[NodeSeq])(implicit id: AlertId = defaultErrAlertId): JsCmd = errMsgs match {
    case Nil                => jsClearError
    case singleError :: Nil => jsShowError(singleError)
    case _                  => jsClearAndShowError(Notices.renderMsgs(AlertTypeError, errMsgs))
  }

  def jsPossibleError[T](box: Box[T])(successJs: T => JsCmd, failureJs: => JsCmd = Noop)(implicit id: AlertId = defaultErrAlertId): JsCmd =
    box match {
      case Full(v)            => jsClearError |+| successJs(v)
      case Empty              => jsShowError(ErrorMessages.Generic) |+| failureJs
      case Failure(err, _, _) => jsShowError(err) |+| failureJs
    }

  def jsShowAlertSuccess(content: NodeSeq)(implicit id: AlertId = null): JsCmd =
    appendAlert(applyAlertId(Notices.renderSingle(AlertTypeSuccess, content)))
}

/**
 * A stateful snippet with only one rendering method.
 */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}
