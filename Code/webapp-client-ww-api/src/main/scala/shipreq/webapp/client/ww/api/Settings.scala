package shipreq.webapp.client.ww.api

import org.scalajs.dom.Transferable
import scala.scalajs.js
import Protocol._

// Inherited by both Client and Server
trait Settings {
  final protected val onError = OnError.Console
  final val codec: Codec.Binary.type = Codec.Binary

  // https://developers.google.com/web/updates/2011/12/Transferable-Objects-Lightning-Fast
  final protected def transferables(e: codec.Encoded): js.UndefOr[js.Array[Transferable]] =
    js.Array(e: Transferable)
}
