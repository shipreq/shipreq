package shipreq.webapp.client.protocol

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.Reusability
import scalaz.{-\/, \/-, \/}
import shipreq.webapp.base.protocol.{GenericFailure, RemoteFn}
import shipreq.webapp.client.data.TCB
import shipreq.webapp.client.lib.Logger
import ClientProtocol._

trait ClientProtocol {
  def call(i: RemoteFn.Instance)(input  : i.fn.Input,
                                 success: i.fn.Output => TCB.Success,
                                 failure: Failed[i.fn.Failure] => TCB.Failure): Callback

  /**
   * Generic means of handling and consuming generic (protocol/ajax) failure.
   *
   * Eventually this should be replaced with something better.
   */
  def consumeGenericFailure(f: Failed[GenericFailure]): TCB.Failure =
    TCB.Failure(Logger(_.error(genericFailureToText(f))))

  /**
   * Generic means of handling and consuming generic (protocol/ajax) failure.
   *
   * Eventually this should be replaced with something better.
   */
  def genericFailureToText(f: Failed[GenericFailure]): String =
    f match {
      case -\/(t) => Option(t.getMessage) match {
        case Some(m) => "AJAX error occurred: " + m
        case None    => "AJAX error occurred."
      }
      case \/-(e) => "Remote error occurred: " + e.msg
    }
}

object ClientProtocol {
  type Failed[O] = Throwable \/ O

  @inline implicit def reusability = Reusability.byRef[ClientProtocol]

  object Default extends ClientProtocol {
    import boopickle._
    import java.nio.ByteBuffer
    import org.scalajs.dom
    import org.scalajs.dom.ext.AjaxException
    import scala.concurrent.{Future, Promise}
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
    import scala.scalajs.js
    import scala.scalajs.js.typedarray._
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    import shipreq.webapp.base.AppConsts

    val ajaxPath = "/" + AppConsts.ajaxPath + "/"
    val timeoutMs = 120 * 1000

    def base64ToBinary(base64: String): ByteBuffer = {
      val binstr = dom.atob(base64)
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

    override def call(i: RemoteFn.Instance)(input  : i.fn.Input,
                                            success: i.fn.Output => TCB.Success,
                                            failure: Failed[i.fn.Failure] => TCB.Failure): Callback = Callback {
      import i.fn._
      val url = LiftAjax.addPageNameAndVersion(ajaxPath, js.undefined) + "?" + i.key
      val bin = PickleImpl.intoBytes(input)
      val res = postBinary(url, bin).map(UnpickleImpl(pickleResponse) fromBytes _)
      res.onSuccess { case \/-(o) => success(o)     .runNow() }
      res.onSuccess { case -\/(f) => failure(\/-(f)).runNow() }
      res.onFailure { case t      => failure(-\/(t)).runNow() }
    }
  }
}
