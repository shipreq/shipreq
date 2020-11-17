package shipreq.webapp.member.protocol.indexeddb

import scala.scalajs.js
import scala.scalajs.js.|

final case class IndexedDbKey(value: IndexedDbKey.Raw) {
  def asJs = value.asInstanceOf[js.Any]
}

object IndexedDbKey {
  type Raw = Int | String
}