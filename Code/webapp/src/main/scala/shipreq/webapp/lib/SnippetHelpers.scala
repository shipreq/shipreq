package shipreq.webapp.lib

import net.liftweb.common.{ParamFailure, Failure => FailBox, Full, Box, Logger, Empty}
import net.liftweb.http.js.{JsCmd, JsExp}
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{S, NotFoundResponse, RedirectResponse, StatefulSnippet, ResponseShortcutException, LiftResponse}
import net.liftweb.json.{NoTypeHints, Serialization, Serializer}
import net.liftweb.sitemap.Menu
import net.liftweb.util.Props
import scala.slick.jdbc.JdbcBackend.Session
import scala.xml.{Elem, Text, NodeSeq, UnprefixedAttribute}
import scalaz.Monoid
import shipreq.webapp.app.{DI, AppSiteMap}
import shipreq.webapp.db.{DaoS, UserDescriptor}
import shipreq.webapp.feature.validation.{ValidationResult, VFailure}
import shipreq.webapp.snippet.{AlertTypeSuccess, AlertTypeError, Notices}
import shipreq.webapp.util.HttpResponses.ShouldNeverHappenResponse
import shipreq.webapp.util.JsExt._
import shipreq.webapp.util.ErrorMessages
import AppSiteMap.Implicits._
import ScalazSubset._
import Types._

object SnippetHelpers extends StaticSnippetHelpers {
  sealed trait NoticeContainerExpTag extends TypeTag[String]
  sealed trait ErrorAlertIdTag extends TypeTag[String]

  final val DefaultNoticesContainerExp = "#notices".tag[NoticeContainerExpTag]
  final val DefaultAjaxErrorId = "x--e".tag[ErrorAlertIdTag]

  final val JqExprJsonSerializer: Serializer[JqExpr] = new Serializer[JqExpr] {
    import net.liftweb.json._
    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), JqExpr] = ???
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case expr: JqExpr => JString(expr.toJsCmd)
    }
  }

  final val NodeSeqJsonSerializer: Serializer[NodeSeq] = new Serializer[NodeSeq] {
    import net.liftweb.json._
    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), NodeSeq] = ???
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case expr: NodeSeq => JString(expr.toString)
    }
  }

  final val DefaultJsonFormat = Serialization.formats(NoTypeHints) + JqExprJsonSerializer + NodeSeqJsonSerializer
}

import SnippetHelpers.{ErrorAlertIdTag, NoticeContainerExpTag}

// =====================================================================================================================

/**
 * Snippet helpers without Misc, DI and implicit vals/defs.
 */
trait StaticSnippetHelpers extends Logger {
  final type NoticeContainerExp = String @@ NoticeContainerExpTag
  final type ErrorAlertId = String @@ ErrorAlertIdTag

  @inline implicit final def jsExpToJsCmd(in: JsExp): JsCmd = in.cmd
  @inline implicit final def str2txt(s: String): NodeSeq = Text(s)

  def redirectHome                                      : Nothing = S.redirectTo(AppSiteMap.Home.relativeUrl)
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

  // -------------------------------------------------------------------------------------------------------------------
  // Error propagation and Alerts

  @inline private def appendAlert(alert: NodeSeq)(implicit nc: NoticeContainerExp): JsCmd =
    JqExpr(alert) ~> JqAppendTo(nc) ~> JqHighlight()

  @inline private def removeAlert(id: String): JsCmd = {
    if (id eq null)
      implicitly[Monoid[JsCmd]].zero
    else
      JqId(id) ~> JqRemove
  }

  @inline private def showAlert(id: String, alert: NodeSeq)(implicit nc: NoticeContainerExp): JsCmd =
    removeAlert(id) |+| appendAlert(applyIdToAlert(id, alert))

  private def applyIdToAlert(id: String, alert: NodeSeq): NodeSeq =
    if (id eq null) alert
    else alert match {
      case NodeSeq.Empty => NodeSeq.Empty
      case e: Elem => e % new UnprefixedAttribute("id", id, xml.Null)
      case _ => warn("Don't know how to add id to: " + alert.getClass); alert
    }

  def jsClearError(implicit id: ErrorAlertId): JsCmd =
    removeAlert(id)

  def jsShowError(errMsg: NodeSeq)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    showAlert(id, Notices.renderSingle(AlertTypeError, errMsg))

  def jsShowErrors(errMsgs: Seq[NodeSeq])(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd = errMsgs match {
    case Nil                => jsClearError
    case singleError :: Nil => jsShowError(singleError)
    case _                  => showAlert(id, Notices.renderMsgs(AlertTypeError, errMsgs))
  }

  def jsPossibleError[T](box: Box[T])(successJs: T => JsCmd, failureJs: => JsCmd = Noop)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    box match {
      case Full(v)            => jsClearError                       |+| successJs(v)
      case Empty              => jsShowError(ErrorMessages.Generic) |+| failureJs
      case FailBox(err, _, _) => jsShowError(err)                   |+| failureJs
    }

  def jsShowFailure(vf: VFailure)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    jsShowError(vf.toHtml)

  def ifValid[T](v: ValidationResult[T])(f: T => JsCmd)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    v match {
      case scalaz.Failure(f) => jsShowFailure(f)
      case scalaz.Success(s) => jsClearError & f(s)
    }

  def jsShowNotice(content: NodeSeq, id: String = null)(implicit nc: NoticeContainerExp): JsCmd =
    showAlert(id, Notices.renderSingle(AlertTypeSuccess, content))
}

// =====================================================================================================================

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends StaticSnippetHelpers with Misc with DI with Logger {
  import SnippetHelpers.{DefaultNoticesContainerExp, DefaultAjaxErrorId, DefaultJsonFormat}

  protected implicit def noticesContainerExp = DefaultNoticesContainerExp
  protected implicit def errorAlertId = DefaultAjaxErrorId
  protected implicit lazy val jsonFormats = DefaultJsonFormat

  def toJson[T <: AnyRef](data: T): Json[T] = Serialization.write(data).tag[IsJsonFor[T]]

  @inline final def currentUser: Option[UserDescriptor] = securityProvider.loggedInUser
  @inline final def currentUserId_!() : UserId = currentUser_!.id
  final def currentUser_!(): UserDescriptor = currentUser match {
    case Some(user) => user
    case None => respondImmediately(RedirectResponse(AppSiteMap.Login.relativeUrl))
  }

  def taskmanD[A](dao: DaoS, f: TaskmanInterface => Session => A): A =
    f(taskman)(dao.session)
}

/**
 * A stateful snippet with only one rendering method.
 */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}
