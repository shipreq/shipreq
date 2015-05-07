package shipreq.webapp.client.app.ui

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
}
