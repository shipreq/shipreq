package shipreq.webapp.lib

import boopickle._
import java.nio.ByteBuffer
import java.util.Base64
import net.liftweb.common.{Full, Empty, Failure => BoxFailure}
import net.liftweb.http.{BadResponse, S}
import scala.util.{Success, Failure => TryFailure}
import shipreq.base.util.Debug._
import shipreq.base.util.Util.quickSB
import shipreq.webapp.base.protocol.{JsEntryPoint, Routine}

object ServerProtocol {

  def binaryToBase64(bb: ByteBuffer): String = {
    val size = bb.limit()
    val a = new Array[Byte](size)
    bb.array().copyToArray(a, 0, size)
    Base64.getEncoder.encodeToString(a)
  }

  def routine[D <: Routine.Desc](d: D)(f: d.I => d.O): Routine.Remote[D] = {
    import d.po

    def fail = BadResponse() // TODO log invalid request to support?

    val proc = S.NFuncHolder { () =>
      val tmp =
        for {
          req  <- S.request
          body <- req.body
        } yield
          UnpickleImpl(d.pi).tryFromBytes(ByteBuffer wrap body) match {
            case Success(input) =>
              val output = f(input)
              val binary = PickleImpl.intoBytes(output)
              BinaryResponse(binary)

            case TryFailure(_) => fail
          }

      tmp match {
        case Full(resp)          => resp
        case Empty               => fail
        case BoxFailure(_, _, _) => fail
      }
    }

    val fnName = S.formFuncName
    S.addFunctionMap(fnName, proc)

    Routine.Remote[D](fnName, d)
  }

  def invokeClientJs[I, O](ep: JsEntryPoint[I, O])(i: I): String = {
    @inline def runOnWindowLoad(f: StringBuilder => Unit): StringBuilder => Unit = sb => {
      sb append "window.onload = function(){"
      f(sb)
      sb append "};"
    }
    import ep.pi
    @inline def callClient(n: String, i: I): StringBuilder => Unit = sb => {
      sb append JsEntryPoint.client
      sb append "()."
      sb append n
      sb append '('
      sb append '"'
      sb append binaryToBase64(PickleImpl.intoBytes(i))
      sb append '"'
      sb append ')'
    }
    quickSB(runOnWindowLoad(callClient(ep.name, i)))
  }

  def invokeClientHtml[I, O](f: JsEntryPoint[I, O])(i: I) =
    <script type="text/javascript">{invokeClientJs(f)(i)}</script>
}
