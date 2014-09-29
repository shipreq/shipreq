package shipreq.webapp.client.lib

import org.scalajs.dom.console
import scala.scalajs.js
import upickle._
import shipreq.webapp.shared.rpc.Interface

/**
 * Client-side RPC support.
 */
object InterfaceClient {

  // TODO test all failure scenarios imaginable

  def readCluster[C <: Interface.Cluster : Reader](a: js.Any) =
    readJs[C](json.readJs(a)) // TODO unsafe readJs

  def invokeCallback[D <: Interface.Def](r: Interface.Remote[D])
                                        (input: r.d.I, callback: r.d.O => Unit)
                                        (implicit I: Writer[r.d.I], O: Reader[r.d.O]): Unit = {
    
    val i = js.encodeURIComponent(write(input))
    val s: js.Any => Unit = o => {
      console.log("invokeCallback result", o)
      val oo = readJs[r.d.O](json.readJs(o)) // TODO unsafe readJs
      callback(oo)
    }
    // needs failure
    LiftAjax.lift_ajaxHandler(s"${r.n}=$i", s, null, "json")
  }
}
