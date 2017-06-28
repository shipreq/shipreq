package shipreq.webapp.server.lib

import net.liftweb.http._
import net.liftweb.json.{NoTypeHints, Serialization, Serializer}
import net.liftweb.sitemap.Menu
import scala.xml.NodeSeq
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.UserId
import shipreq.webapp.server.app.AppSiteMap.Implicits._
import shipreq.webapp.server.app.{AppSiteMap, Global}
import shipreq.webapp.server.logic.User
import shipreq.webapp.server.util.HttpResponses.ShouldNeverHappenResponse
import shipreq.webapp.server.util.JsExt._

object SnippetHelpers extends StaticSnippetHelpers {

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

}

/**
 * Snippet helpers without Misc, Global and implicit vals/defs.
 */
trait StaticSnippetHelpers extends HasLogger {

  def respondImmediately(response: LiftResponse): Nothing =
    throw ResponseShortcutException.shortcutResponse(response)

  def redirectHome()                                    : Nothing = respondImmediately(AppSiteMap.redirectHome)
  def redirectToLogin()                                 : Nothing = respondImmediately(AppSiteMap.redirectToLogin)
  def redirectTo(page: Menu)                            : Nothing = S.redirectTo(page.relativeUrl)
  def redirectTo(page: Menu.Menuable)                   : Nothing = S.redirectTo(page.relativeUrl)
  def redirectTo[T](page: Menu.ParamMenuable[T])(arg: T): Nothing = S.redirectTo(page.relativeUrl(arg))

  def shouldNeverHappen_! : Nothing =
    respondImmediately(ShouldNeverHappenResponse) // TODO do more! notify Taskman! etc
}

// =====================================================================================================================

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends StaticSnippetHelpers with Misc with HasLogger {

  @inline final def currentUser(): Option[User] =
    Global.security.loggedInUser()

  @inline final def currentUserId_!() : UserId =
    currentUser_!().id

  final def currentUser_!(): User =
    currentUser() getOrElse redirectToLogin()
}

/** A stateful snippet with only one rendering method. */
abstract class SingleOpStatefulSnippet extends StatefulSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}

/** A stateless snippet with only one rendering method. */
abstract class SingleOpStatelessSnippet extends DispatchSnippet with SnippetHelpers {
  override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}
