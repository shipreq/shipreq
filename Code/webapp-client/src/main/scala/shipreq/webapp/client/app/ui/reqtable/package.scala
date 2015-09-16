package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.ui.shouldComponentUpdate[P, S, B, N]
}
