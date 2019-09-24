package shipreq.webapp.server.logic.dispatch

import japgolly.univeq.UnivEq
import shipreq.base.util.{BinaryData, Url}
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.user.User
import shipreq.webapp.base.util.ResourceHint

final case class Response(cmd: ResponseCmd, cookies: Cookie.Update)

sealed trait ResponseCmd {
  def headers: ResponseCmd.Headers = Nil
}

object ResponseCmd {

  type Header = (String, String)
  type Headers = List[Header]

  implicit class ResourceHintExt(private val rh: ResourceHint) extends AnyVal {
    def headerValue: String = {
      val g = rh.generic
      var h = s"<${g.href}>; rel=${g.rel}"
      g.as.foreach(as => h = s"$h; as=$as")
      g.`type`.foreach(t => h = h + "; type=\"" + t + "\"")
      g.crossorigin match {
        case Some("anonymous") => h = h + "; crossorigin"
        case Some(v)           => h = h + "; crossorigin=\"" + v + "\""
        case None              => ()
      }
      if (g.relativeHref) h = h + "; nopush"
      h
    }
  }

  final case class ServePublicSpa(user: Option[User]) extends ResponseCmd

  final case class ServeHomeSpa(user: User) extends ResponseCmd

  object ProjectSpa {
    final case class Serve(user: User, projectId: ProjectId) extends ResponseCmd
    case object NotOwner extends ResponseCmd
    case object InvalidId extends ResponseCmd
  }

  final case class Redirect(dest: Url.Relative) extends ResponseCmd

  /** Respond with a HTTP status only; no content */
  final case class StatusOnly(status: Int) extends ResponseCmd {
    val withoutCookieUpdate = Response(this, Cookie.Update.empty)
  }

  object StatusOnly {

    val OK = apply(200)

    /** The server cannot or will not process the request due to an apparent client error
     * (e.g., malformed request syntax, size too large). */
    val BadRequest = apply(400)

    /** The request was valid, but the server is refusing action.
     * The user might not have the necessary permissions for a resource, or may need an account of some sort. */
    val Forbidden = apply(403)

    /** The requested resource could not be found but may be available in the future. */
    val NotFound = apply(404)

    /** Indicates the HTTP method (GET, POST etc) wasn't allowed */
    val MethodNotAllowed = apply(405)
  }

  final case class Text(status: Int, body: String) extends ResponseCmd

  final case class Json(status: Int, body: String) extends ResponseCmd

  object Json {
    def apply(status: Int, body: String): Json =
      new Json(status, body)

    def apply(status: Int, body: io.circe.Json): Json =
      apply(status, body.noSpaces)
  }

  final case class Binary(status: Int, body: BinaryData) extends ResponseCmd

  implicit def univEq: UnivEq[ResponseCmd] = UnivEq.derive

  val redirectToPublicHome = Redirect(Urls.publicHome)
  val redirectToMemberHome = Redirect(Urls.memberHome)
}

object StatusCode {
  final val OK = 200

  /** The server cannot or will not process the request due to an apparent client error
   * (e.g., malformed request syntax, size too large). */
  final val BadRequest = 400

  /** The server either does not recognize the request method, or it lacks the ability to fulfil the request.
   * Usually this implies future availability (e.g., a new feature of a web-service API). */
  final val NotImplemented = 501
}

