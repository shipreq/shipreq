package shipreq.webapp.base.protocol

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import scala.util.{Failure, Success}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ErrorMsg

trait ClientProtocol {
  def call[I, O](proc      : ServerSideProc[I, O])
                (input     : I,
                 onResponse: Throwable \/ O => Callback): Callback

  final def apply[I, O](proc: ServerSideProc[I, O]): ServerSideProcInvoker[I, ErrorMsg, O] =
    new ServerSideProcInvoker[I, Throwable, O]((i, s, f) => call(proc)(i, _.fold(f, s)))
      .mapFailure(ServerSideProcInvoker.throwableToErrorMsg)
}

object ClientProtocol {
  @inline implicit def reusability = Reusability.byRef[ClientProtocol]

  object Default extends ClientProtocol {
    import boopickle._
    import java.nio.ByteBuffer
    import org.scalajs.dom
    import org.scalajs.dom.ext.AjaxException
    import scala.concurrent.{Future, Promise}
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    import scala.scalajs.js.typedarray._
    import shipreq.webapp.base.WebappConfig

    val ajaxPath = "/" + WebappConfig.liftPath1 + "/ajax"
    val timeoutMs = 120 * 1000

    def base64ToBinary(base64: String): ByteBuffer = {
      val binstr = dom.window.atob(base64)
      val buf = new Int8Array(binstr.length)
      var i = 0
      binstr.foreach { ch =>
        buf(i) = ch.toByte
        i += 1
      }
      TypedArrayBuffer.wrap(buf)
    }

    // Initially copied from http://ochrons.github.io/scalajs-spa-tutorial/autowire-and-boopickle.html
    def postBinary(url: String, data: ByteBuffer): Future[ByteBuffer] = {
      val array = data.typedArray().subarray(0, data.limit)
      val req = new dom.XMLHttpRequest()
      val promise = Promise[dom.XMLHttpRequest]()

      req.onreadystatechange = { (e: dom.Event) =>
        if (req.readyState == 4) {
          if ((req.status >= 200 && req.status < 300) || req.status == 304)
            promise.success(req)
          else
            promise.failure(AjaxException(req))
        }
      }
      req.open("POST", url)
      req.responseType = "arraybuffer"
      req.timeout = timeoutMs
      req.setRequestHeader("Content-Type", "application/octet-stream")
      req.send(array)

      promise.future.map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
    }

    override def call[I, O](proc      : ServerSideProc[I, O])
                           (input     : I,
                            onResponse: Throwable \/ O => Callback) = Callback {
      import proc.protocol._
      val url = LiftAjax.calcAjaxUrl(ajaxPath, null) + "?" + proc.id.value
      val bin = PickleImpl.intoBytes(input)
      val res = postBinary(url, bin).map(UnpickleImpl(pickleOutput) fromBytes _)
      res.onComplete {
        case Success(o) => onResponse(\/-(o)).runNow()
        case Failure(e) => onResponse(-\/(e)).runNow()
      }
    }
  }
}
