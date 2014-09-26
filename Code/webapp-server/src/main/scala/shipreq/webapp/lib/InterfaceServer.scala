package shipreq.webapp.lib

import net.liftweb.http.S
import upickle._
import shipreq.base.util.Util.quickSB
import shipreq.webapp.shared.rpc.{ClientAccess, Interface}

/**
 * Server-side RPC support.
 */
object InterfaceServer {

  def impl[D <: Interface.Def](d: D)(f: d.I => d.O)(implicit I: Reader[d.I], O: Writer[d.O]) = {
    // TODO test all failure scenarios imaginable
    val proc = S.SFuncHolder(req => {
      val i = read[d.I](req)
      val o = f(i)
      val r = write[d.O](o)
      RawJsonResponse(r)
    })
    val fnName = S.fmapFunc(proc)(n => n)
    Interface.Remote[D](fnName, d)
  }

  def invokeClientJs[I: Writer, O](f: ClientAccess.Fn[I, O])(i: I): String = {
    def runOnWindowLoad(f: StringBuilder => Unit): StringBuilder => Unit = sb => {
      sb append "window.onload = function(){"
      f(sb)
      sb append "};"
    }
    def callClient[I: Writer](n: String, i: I): StringBuilder => Unit = sb => {
      sb append ClientAccess.client
      sb append "()."
      sb append n
      sb append '('
      sb append write(i)
      sb append ')'
    }
    quickSB(runOnWindowLoad(callClient(f.name, i)))
  }

  def invokeClientHtml[I: Writer, O](f: ClientAccess.Fn[I, O])(i: I) =
    <script type="text/javascript">{invokeClientJs(f)(i)}</script>
}
