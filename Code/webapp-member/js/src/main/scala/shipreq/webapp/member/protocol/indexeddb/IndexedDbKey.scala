package shipreq.webapp.member.protocol.indexeddb

import org.scalajs.dom.IDBKey
import scala.scalajs.js.|

final class IndexedDbKey private(val asJs: IDBKey) extends AnyVal {
  @inline def value = asJs.asInstanceOf[IndexedDbKey.Typed]
}

object IndexedDbKey {

  type Typed = Int | String

  @inline def apply(t: Typed): IndexedDbKey =
    fromJs(t)

  def fromJs(k: IDBKey): IndexedDbKey =
    new IndexedDbKey(k)
}