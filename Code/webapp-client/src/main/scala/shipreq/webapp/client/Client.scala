package shipreq.webapp.client

import scala.scalajs.js.{Any => Θ}
import scala.scalajs.js.annotation.JSExport
import shipreq.webapp.shared.rpc.{ClientAccess => C}
import shipreq.webapp.client.lib.Patches
import upickle._

@JSExport(C.client)
object Client {

  private def recv[I: Reader, O](x: C.Fn[I, O], i: Θ)(f: I => O): O =
    f(readJs[I](Patches readJs i)) // TODO errors?

  @JSExport(C.reactExamplesN)
  def reactExamples(i: Θ) = recv(C.reactExamples, i)(hahaa.ReactExamples.main)
}
