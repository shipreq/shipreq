package shipreq.webapp.client.ww.api

import boopickle._
import shipreq.webapp.base.protocol.BoopickleMacros._
import shipreq.webapp.base.protocol.BinCodecGeneric._

final case class Svg(content: String) extends AnyVal

object Svg {
  implicit val pickle: Pickler[Svg] = pickleCaseClass
}
