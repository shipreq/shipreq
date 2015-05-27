package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import shipreq.base.util.Must

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  def failedMust[A](a: A)(e: String): A = {
    // TODO Do something more with Must failure
    org.scalajs.dom.console.error(e)
    a
  }

  def mustResolve[A](m: Must[A])(fallback: => A): A =
    m.fold(failedMust(fallback), identity)

  def mustResolveO[A](m: Must[A]): Option[A] =
    m.fold(failedMust(None: Option[A]), Some(_))

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.ui.shouldComponentUpdate[P, S, B, N]
}
