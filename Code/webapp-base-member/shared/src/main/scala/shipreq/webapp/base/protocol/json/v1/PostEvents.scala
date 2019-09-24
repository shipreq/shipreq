package shipreq.webapp.base.protocol.json.v1

import io.circe._
import shipreq.webapp.base.event._

object PostEvents {
  import Events._

  implicit val decoderEventOrd: Decoder[EventOrd] =
    Decoder[Int].map(EventOrd.apply)

  implicit val encoderEventOrd: Encoder[EventOrd] =
    Encoder[Int].contramap(_.value)

  implicit val decoderEventOrdLatest: Decoder[EventOrd.Latest] =
    Decoder[Int].map(EventOrd.Latest.apply)

  implicit val encoderEventOrdLatest: Encoder[EventOrd.Latest] =
    Encoder[Int].contramap(_.value)

  implicit val decoderVerifiedEvent: Decoder[VerifiedEvent] =
    Decoder.forProduct2("#", "event")(VerifiedEvent.apply)

  implicit val encoderVerifiedEvent: Encoder[VerifiedEvent] =
    Encoder.forProduct2("#", "event")(a => (a.ord, a.event))

}
