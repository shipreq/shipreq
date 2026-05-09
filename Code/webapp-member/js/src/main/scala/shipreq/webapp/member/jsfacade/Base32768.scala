package shipreq.webapp.member.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.typedarray.Uint8Array

@JSGlobal("B32768")
@js.native
object Base32768 extends js.Object {
  def encode(bin: Uint8Array): String = js.native
  def decode(str: String): Uint8Array = js.native
}
