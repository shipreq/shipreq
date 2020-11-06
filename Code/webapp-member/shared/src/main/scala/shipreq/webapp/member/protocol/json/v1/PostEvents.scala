package shipreq.webapp.member.protocol.json.v1

import io.circe._
import shipreq.webapp.member.project.event._

object PostEvents {

  implicit val decoderEventOrd: Decoder[EventOrd] =
    Decoder[Int].map(EventOrd.apply)

  implicit val encoderEventOrd: Encoder[EventOrd] =
    Encoder[Int].contramap(_.value)

  implicit val decoderEventOrdLatest: Decoder[EventOrd.Latest] =
    Decoder[Int].map(EventOrd.Latest.apply)

  implicit val encoderEventOrdLatest: Encoder[EventOrd.Latest] =
    Encoder[Int].contramap(_.value)
}
