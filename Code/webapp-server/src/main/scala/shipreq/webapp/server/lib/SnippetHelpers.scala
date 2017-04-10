package shipreq.webapp.server.lib

import net.liftweb.common.{Box, Empty, Full, ParamFailure, Failure => FailBox}
import net.liftweb.http._
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.js.{JsCmd, JsCmds, JsExp}
import net.liftweb.json.{NoTypeHints, Serialization, Serializer}
import net.liftweb.sitemap.Menu
import net.liftweb.util.Props
import scala.xml.{Elem, NodeSeq, Text, UnprefixedAttribute}
import scalaz.{-\/, Monoid, \/, \/-}
import scalaz.syntax.semigroup._
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.base.util.TaggedTypes.{JsonStr, TaggedString}
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.UserId
import shipreq.webapp.base.vali2._
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.app.{AppSiteMap, DI}
import shipreq.webapp.server.data.UserDescriptor
import shipreq.webapp.server.feature.validation.InvalidityHtml
import shipreq.webapp.server.snippet.{AlertTypeError, AlertTypeSuccess, Notices}
import shipreq.webapp.server.util.ErrorMessages
import shipreq.webapp.server.util.HttpResponses.ShouldNeverHappenResponse
import shipreq.webapp.server.util.JsExt._

object SnippetHelpers extends StaticSnippetHelpers {
  final case class NoticeContainerExp(value: String) extends TaggedString
  final case class ErrorAlertId(value: String) extends TaggedString

  final val DefaultNoticesContainerExp = NoticeContainerExp("#notices")
  final val DefaultAjaxErrorId = ErrorAlertId("x--e")

  final val JqExprJsonSerializer: Serializer[JqExpr] = new Serializer[JqExpr] {
    // TODO Change over to Argonaut. Actually acks [Jj]son
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

  implicit object JsCmdMonoid extends Monoid[JsCmd] {
    import JsCmds.{_Noop => Noop}

    override def zero = Noop
    override def append(a: JsCmd, b: => JsCmd) =
      if (a eq Noop) b
      else if (b eq Noop) a
      else a & b
  }
}

import SnippetHelpers.{ErrorAlertId, JsCmdMonoid, NoticeContainerExp}

// =====================================================================================================================

/**
 * Snippet helpers without Misc, DI and implicit vals/defs.
 */
trait StaticSnippetHelpers extends HasLogger {

  @inline implicit final def jsExpToJsCmd(in: JsExp): JsCmd = in.cmd
  @inline implicit final def str2txt(s: String): NodeSeq = Text(s)

  @inline def staticHtml(link: NodeSeq): EndoFn[NodeSeq] =
    _ => link

  def redirectHome()                                    : Nothing = S.redirectTo(AppSiteMap.Home.relativeUrl)
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
        log.error(msg)
        fallback
      case Test | Development | Profile =>
        shouldNeverHappen_!(msg)
    }
  }

  def requireResultO_![T](o: Option[T], fallbackErrorReaction: => Nothing = redirectHome()): T = o match {
    case Some(t) => t
    case None    => fallbackErrorReaction
  }

  def requireResult_![T](box: Box[T], fallbackErrorReaction: => Nothing = redirectHome()): T = box match {
    case Full(t)                                 => t
    case Empty                                   => fallbackErrorReaction
    case ParamFailure(_, _, _, r: LiftResponse)  => respondImmediately(r)
    case ParamFailure(_, _, _, m: Menu)          => redirectTo(m)
    case ParamFailure(_, _, _, m: Menu.Menuable) => redirectTo(m)
    case ParamFailure(_, _, _, NotFoundResponse) => respondImmediately(NotFoundResponse())
    case _                                       => log.error(s"Don't know how to react to $box"); shouldNeverHappen_!
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
      case _ => log.warn("Don't know how to add id to: " + alert.getClass); alert
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

  def jsShowSimpleInvalidity(f: Simple.Invalidity)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    jsShowError(InvalidityHtml.simple(f))(id, nc)

  def handleSimpleInvalidity[T](v: Simple.Invalidity \/ T)(f: T => JsCmd)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    v match {
      case \/-(s) => jsClearError & f(s)
      case -\/(e) => jsShowSimpleInvalidity(e)
    }

  def jsShowCompositeInvalidity(f: Composite.Invalidity)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    jsShowError(InvalidityHtml.composite(f))(id, nc)

  def handleCompositeInvalidity[T](v: Composite.Invalidity \/ T)(f: T => JsCmd)(implicit id: ErrorAlertId, nc: NoticeContainerExp): JsCmd =
    v match {
      case \/-(s) => jsClearError & f(s)
      case -\/(e) => jsShowCompositeInvalidity(e)
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
trait SnippetHelpers extends StaticSnippetHelpers with Misc with DI with HasLogger {
  import SnippetHelpers.{DefaultAjaxErrorId, DefaultJsonFormat, DefaultNoticesContainerExp}

  protected implicit def noticesContainerExp = DefaultNoticesContainerExp
  protected implicit def errorAlertId = DefaultAjaxErrorId
  protected implicit lazy val jsonFormats = DefaultJsonFormat

  def toJson[T <: AnyRef](data: T): JsonStr[T] = JsonStr[T](Serialization write data)

  @inline final def currentUser(): Option[UserDescriptor] = securityProvider().loggedInUser
  @inline final def currentUserId_!() : UserId = currentUser_!().id
  final def currentUser_!(): UserDescriptor = currentUser() match {
    case Some(user) => user
    case None => respondImmediately(RedirectResponse(AppSiteMap.Login.relativeUrl))
  }
}

/**
 * A stateful snippet with only one rendering method.
 */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}
