package shipreq.webapp.server.lib

import net.liftweb.common.Full
import net.liftweb.http._
import net.liftweb.json.{NoTypeHints, Serialization, Serializer}
import scala.xml.NodeSeq
import shipreq.base.util.Url
import shipreq.base.util.log.HasLogger
import shipreq.webapp.base.user._
import shipreq.webapp.server.app.LiftDispatcher
import shipreq.webapp.server.util.HttpResponses.ShouldNeverHappenResponse

// TODO Move SnippetHelpers into .snippet
object SnippetHelpers extends StaticSnippetHelpers {

  lazy val NodeSeqJsonSerializer: Serializer[NodeSeq] = new Serializer[NodeSeq] {
    import net.liftweb.json._
    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), NodeSeq] = ???
    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case expr: NodeSeq => JString(expr.toString)
    }
  }

  final val DefaultJsonFormat = Serialization.formats(NoTypeHints) + NodeSeqJsonSerializer
}

/**
 * Snippet helpers without Misc, Global and implicit vals/defs.
 */
trait StaticSnippetHelpers extends HasLogger {

  /** This removes the outer-most tag that contained the data-lift directive.
    *
    * Eg. <div data-lift=X>A</div> would normally transform to <div>B</div>,
    * where as with this it will be just B.
    */
  final val removeSnippetTag: NodeSeq => NodeSeq =
    _.head.child

  def respondImmediately(response: LiftResponse): Nothing =
    throw ResponseShortcutException.shortcutResponse(response)

//  def redirectToLogin(): Nothing =
//    redirectTo(PublicUrls.login)
//
//  def redirectTo(url: Url.Relative): Nothing =
//    S.redirectTo(url.relativeUrl)

  def shouldNeverHappen_! : Nothing =
    respondImmediately(ShouldNeverHappenResponse) // TODO do more! notify Taskman! etc
}

// =====================================================================================================================

/**
 * Helpers for snippets.
 *
 * @since 11/06/2013
 */
trait SnippetHelpers extends StaticSnippetHelpers with HasLogger {

  @inline final def currentUserId_!() : UserId =
    currentUser_!().id

  final def currentUser_!(): User = {
    val user = LiftDispatcher.UserVar.is
    assert(user != null, "LiftDispatcher.UserVar isn't set!")
    user
  }

  final def currentUserOption(): Option[User] =
    Option(LiftDispatcher.UserVar.is)

  final def request_!(): Req =
    S.request.openOrThrowException("How can a snippet not have a request")

  final def requestUrl(req: Req = request_!()): Url.Absolute =
    Url.Absolute {
      var u = req.request.url
      if (req.uri == "/" && u.last == '/')
        u = u.dropRight(1)
      req.request.queryString match {
        case Full(q) => u = u + "?" + q
        case _       => ()
      }
      u
    }
}

/** A stateless snippet with only one rendering method. */
abstract class SingleOpStatelessSnippet extends DispatchSnippet with SnippetHelpers {
  final override def dispatch = { case _ => render }
  def render: NodeSeq => NodeSeq
}
