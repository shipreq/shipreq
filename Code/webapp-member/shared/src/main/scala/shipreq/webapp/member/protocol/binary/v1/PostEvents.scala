package shipreq.webapp.member.protocol.binary.v1

import boopickle.DefaultBasic._
import shipreq.webapp.member.event._

object PostEvents {

  implicit val picklerEventOrd: Pickler[EventOrd] =
    transformPickler(EventOrd.apply)(_.value)

  implicit val picklerEventOrdLatest: Pickler[EventOrd.Latest] =
    transformPickler(EventOrd.Latest.apply)(_.value)
}
