package shipreq.webapp.server.logic.dispatch

import shipreq.base.util.{BinaryData, Url}
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.data.{ProjectId, User}

final case class Response(cmd: ResponseCmd, cookies: Cookie.Update)

sealed trait ResponseCmd {
  def headers: ResponseCmd.Headers = Nil
  def withoutCookieUpdate: Response = Response(this, Cookie.Update.empty)
}

object ResponseCmd {

  type Header = (String, String)
  type Headers = List[Header]

  final case class ServePublicSpa(user: Option[User]) extends ResponseCmd

  final case class ServeHomeSpa(user: User) extends ResponseCmd

  case object ProjectAccessRevoked extends ResponseCmd

  object ProjectSpa {
    final case class Serve(user: User, projectId: ProjectId) extends ResponseCmd
    case object AccessDenied extends ResponseCmd
  }

  final case class Redirect(dest: Url.Relative) extends ResponseCmd

  /** Respond with a HTTP status only; no content */
  final case class StatusOnly(status: Int) extends ResponseCmd {
    override val withoutCookieUpdate = Response(this, Cookie.Update.empty)
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

  /** The request contained valid data and was understood by the server, but the server is refusing action. This may be
    * due to the user not having the necessary permissions for a resource or needing an account of some sort, or
    * attempting a prohibited action (e.g. creating a duplicate record where only one is allowed). This code is also
    * typically used if the request provided authentication via the WWW-Authenticate header field, but the server did
    * not accept that authentication. The request should not be repeated.
    */
  final val Forbidden = 403

  /** The server either does not recognize the request method, or it lacks the ability to fulfil the request.
   * Usually this implies future availability (e.g., a new feature of a web-service API). */
  final val NotImplemented = 501
}

