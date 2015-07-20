package shipreq.webapp.client.protocol

import shipreq.webapp.base.AppConsts
import scala.scalajs.js
import scalaz.effect.IO
import shipreq.webapp.base.protocol.Routine
import shipreq.webapp.client.lib.FailureIO

trait ClientProtocol {
  // TODO Should not this accept a SuccessIO?
  def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, success: r.d.O => IO[Unit], f: FailureIO): IO[Unit]
}

/*
object JsonClientProtocol {
  import upickle._
  import upickle.Fns._

  def parseJsObject[T: Reader](a: js.Any): Throwable \/ T =
    try
      \/-(readJs[T](json.readJs(a)))
    catch {
      case e: Throwable => -\/(e)
    }

  def jsonEffect[T: Reader](f: T => IO[Unit]): js.Any => Unit =
    a => {
      val io =
        parseJsObject[T](a) match {
          case \/-(b) => f(b)
          case -\/(e) => handleJsonParsingError(a, e)
        }
      io.unsafePerformIO()
    }

  private def handleJsonParsingError(a: js.Any, e: Throwable): IO[Unit] =
    ConsoleIO(_.error(s"Parsing failure: $e\nJS: ", a))

  object Lift extends ClientProtocol {
    override def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, success: r.d.O => IO[Unit], f: FailureIO): IO[Unit] = {
      import r.d.{wi, ro}
      val i = js.URIUtils.encodeURIComponent(write(input))
      val q = s"${r.n}=$i"
      val s = jsonEffect[r.d.O](success)
      val ff = () => (ConsoleIO(_ error s"AJAX failure on ${r.n} ⇐ $input") >> f.io).unsafePerformIO()
      IO(LiftAjax.lift_ajaxHandler(q, s, ff, "json"))
    }
  }
}
*/

object ClientProtocol {

  object Default extends ClientProtocol {
    import boopickle._
    import java.nio.ByteBuffer
    import org.scalajs.dom
    import org.scalajs.dom.ext.AjaxException
    import scala.concurrent.{Future, Promise}
    import scala.scalajs.js.typedarray._
    import scala.scalajs.js.typedarray.TypedArrayBufferOps._
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

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

    // Scala.js DOM 0.8.0 does not support binary data, so we implement this here
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

    override def call[D <: Routine.Desc](r: Routine.Remote[D])(input: r.d.I, success: r.d.O => IO[Unit], failure: FailureIO): IO[Unit] = IO {
      import r.d.{pi, po}
      val url = LiftAjax.addPageNameAndVersion(ajaxPath, js.undefined) + "?" + r.n
      val inb = PickleImpl.intoBytes(input)
      val fut = postBinary(url, inb).map(UnpickleImpl(po) fromBytes _)
      fut.onSuccess { case v => success(v).unsafePerformIO() }
      fut.onFailure { case t => failure.io.unsafePerformIO() }
    }
  }
}