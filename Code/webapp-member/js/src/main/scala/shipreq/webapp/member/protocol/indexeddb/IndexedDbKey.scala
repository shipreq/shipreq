package shipreq.webapp.member.protocol.indexeddb

import scala.scalajs.js
import scala.scalajs.js.|

final class IndexedDbKey private(val asJs: js.Any) extends AnyVal {
  @inline def value = asJs.asInstanceOf[IndexedDbKey.Typed]
}

object IndexedDbKey {

  type Typed = Int | String

  @inline def apply(t: Typed): IndexedDbKey =
    fromJs(t.asInstanceOf[js.Any])

  def fromJs(k: js.Any): IndexedDbKey =
    new IndexedDbKey(k)
}