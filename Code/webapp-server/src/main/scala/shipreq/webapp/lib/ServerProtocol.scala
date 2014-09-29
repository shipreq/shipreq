package shipreq.webapp.lib

import net.liftweb.http.{BadResponse, S}
import scalaz.{-\/, \/-, \/}
import upickle._

import shipreq.base.util.Util.quickSB
import shipreq.webapp.shared.protocol.{JsEntryPoint, Routine}

object ServerProtocol {

  def parseJson[T: Reader](j: String): Throwable \/ T =
    try
      \/-(read[T](j))
    catch {
      case e: Throwable => -\/(e)
    }

  def routine[D <: Routine.Desc](d: D)(f: d.I => d.O)(implicit I: Reader[d.I], O: Writer[d.O]) = {
    val proc = S.SFuncHolder(req =>
      parseJson[d.I](req) match {
        case \/-(i) => RawJsonResponse(write[d.O](f(i)))
        case -\/(_) => BadResponse() // TODO log invalid JSON req to support?
      })
    val fnName = S.fmapFunc(proc)(n => n)
    Routine.Remote[D](fnName, d)
  }

  def invokeClientJs[I: Writer, O](f: JsEntryPoint[I, O])(i: I): String = {
    def runOnWindowLoad(f: StringBuilder => Unit): StringBuilder => Unit = sb => {
      sb append "window.onload = function(){"
      f(sb)
      sb append "};"
    }
    def callClient[I: Writer](n: String, i: I): StringBuilder => Unit = sb => {
      sb append JsEntryPoint.client
      sb append "()."
      sb append n
      sb append '('
      sb append write(i)
      sb append ')'
    }
    quickSB(runOnWindowLoad(callClient(f.name, i)))
  }

  def invokeClientHtml[I: Writer, O](f: JsEntryPoint[I, O])(i: I) =
    <script type="text/javascript">{invokeClientJs(f)(i)}</script>
}
