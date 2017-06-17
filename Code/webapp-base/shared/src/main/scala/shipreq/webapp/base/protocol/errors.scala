package shipreq.webapp.base.protocol

import boopickle.Pickler
import BoopickleMacros._
import BinCodecGeneric._

final case class ErrorMsg(msg: String)
object ErrorMsg {
  implicit val pickleErrorMsg: Pickler[ErrorMsg] = pickleCaseClass
}
