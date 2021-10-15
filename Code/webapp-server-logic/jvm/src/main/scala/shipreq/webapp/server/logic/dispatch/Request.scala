package shipreq.webapp.server.logic.dispatch

import cats.Eval
import shipreq.base.util.{BinaryData, Url}

/** A request to the server.
 *
 * @param path Does *NOT* include query params
 */
final case class Request[+Real](method: Method,
                                path  : Url.Relative,
                                body  : Eval[Option[BinaryData]],
                                param : String => Option[String],
                                cookie: Cookie.LookupFn,
                                real  : Real)

sealed abstract class Method
object Method {
  case object Get   extends Method
  case object Post  extends Method
  case object Other extends Method
  implicit def univEq: UnivEq[Method] = UnivEq.derive
}
