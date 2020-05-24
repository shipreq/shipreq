package shipreq.webapp.client.ww.api

import org.scalajs.dom.Transferable
import scala.scalajs.js
import Protocol._

trait Settings {
  final protected val onError = OnError.Console
  final val codec: Codec.Binary.type = Codec.Binary

  protected def transferables(e: codec.Encoded): js.UndefOr[js.Array[Transferable]] =
    js.Array(e: Transferable)
}

