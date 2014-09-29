package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.shared.protocol._
import shipreq.webapp.lib.ServerProtocol

class WIP {
  def render = {
    val wired1 = ServerProtocol.routine(Routines.Square)(n => s"$n² = ${n*n}")
    val wired2 = ServerProtocol.routine(Routines.Half)(n => s"$n/2 = ${n/2}")
    val wired3 = ServerProtocol.routine(Routines.Grrr)(e => ExampleData(e.i + 9000))
    val pg = Routines.WIP(wired1, wired2, wired3)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
