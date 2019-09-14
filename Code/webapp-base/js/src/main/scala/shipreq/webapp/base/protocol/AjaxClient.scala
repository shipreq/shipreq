package shipreq.webapp.base.protocol

import japgolly.scalajs.react.extra.Ajax
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import org.scalajs.dom.ext.AjaxException
import org.scalajs.dom.window.console
import scala.scalajs.js.typedarray.ArrayBuffer
import scalaz.\/
import shipreq.base.util.{BinaryData, ErrorMsg}
import shipreq.base.util.JsExt._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.DecodingFailure

trait AjaxClient[F[_]] {

  def invoker(p: Protocol.Ajax[F]): ServerSideProcInvoker[p.protocol.RequestType, ErrorMsg, p.protocol.ResponseType]
}

object AjaxClient {

  type Binary = AjaxClient[SafePickler]

  val Binary: Binary = BinaryImpl

  object BinaryImpl extends AjaxClient[SafePickler] {

    private val failedToParse = ErrorMsg("Failed to understand the response from the server.")
    private val serverIsNewer = ErrorMsg("Our servers have been upgraded to a newer version. Please reload this page and try again.")

    final case class Result[A](decodeResult: SafePickler.Result[A], clientVer: Version) {

      private def log(s: String) = console.warn(s)

      private val failureDecision = decodeResult.leftMap {

        case e: DecodingFailure.MagicNumberMismatch =>
          log(s"S:${e.actual.hex}(${e.upstreamVer.fold("?")(_.verNum)}), C:${e.expected.hex}(${clientVer.verNum})")
          (true, failedToParse)

        case e: DecodingFailure.UnsupportedMajorVer =>
          log(s"S:${e.actual.verNum}, C:${clientVer.verNum}")
          if (e.isLocalKnownToBeOutOfDate)
            (false, serverIsNewer)
          else
            (true, failedToParse)

        case e: DecodingFailure.InvalidVersion =>
          log(s"${e.major}.${e.minor}")
          (true, failedToParse)

        case e: DecodingFailure.ExceptionOccurred =>
          e.upstreamVer.foreach(v => log(s"S:${v.verStr}"))
          e.exception.printStackTrace()
          val errorMsg = ServerSideProcInvoker.throwableToErrorMsg(e.exception)
          val shouldRetry = e.isUpstreamKnownToBeOutOfDate
          (shouldRetry, errorMsg)
      }

      def shouldRetry: Boolean =
        failureDecision.fold(_._1, _ => false)

      def errMsgOrSuccess: ErrorMsg \/ A =
        failureDecision.leftMap(_._2)
    }

    def runOnce(p: Protocol.Ajax[SafePickler])(req: p.protocol.RequestType): AsyncCallback[Result[p.protocol.ResponseType]] = {
      val prep   = p.protocol.prepareSend(req)
      val reqBin = p.prepReq.codec.encode(prep.request)

      Ajax("POST", p.url.relativeUrl)
        .setRequestHeader("Content-Type", "application/octet-stream")
        .and(_.responseType = "arraybuffer")
        .send(reqBin.unsafeArrayBuffer)
        .asAsyncCallback
        .map { xhr =>
          if (xhr.status == 200) {
            // Success
            val ab       = xhr.response.asInstanceOf[ArrayBuffer]
            val resCodec = prep.response.codec
            val bin      = BinaryData.unsafeFromArrayBuffer(ab)
            val res      = resCodec.decode(bin)
            Result(res, resCodec.version)
          } else
            throw AjaxException(xhr)
        }
      }

      def runWithRetry(p: Protocol.Ajax[SafePickler])(req: p.protocol.RequestType): AsyncCallback[Result[p.protocol.ResponseType]] = {
        val once = runOnce(p)(req)
        AsyncCallback.tailrec(2) { retriesRemaining =>
          if (retriesRemaining > 0)
            once.attempt.flatMap {
              case Right(r)               if (r.shouldRetry) => AsyncCallback.pure(Left(retriesRemaining - 1))
              case Right(r)                                  => AsyncCallback.pure(Right(r))
              case Left(AjaxException(x)) if x.status == 501 => AsyncCallback.pure(Left(retriesRemaining - 1)) // server rejected due to protocol ver diff
              case Left(e)                                   => AsyncCallback.throwException(e)
            }
          else
            once.map(Right(_))
        }
      }

      override def invoker(p: Protocol.Ajax[SafePickler]): ServerSideProcInvoker[p.protocol.RequestType, ErrorMsg, p.protocol.ResponseType] = {
        ServerSideProcInvoker
          .viaAsyncCallback((req: p.protocol.RequestType) => CallbackTo(runWithRetry(p)(req).map(_.errMsgOrSuccess)))
          .mergeFailure
      }
    }

  def noop[F[_]]: AjaxClient[F] =
    new AjaxClient[F] {
      override def invoker(p: Protocol.Ajax[F]) = {
        val c = CallbackTo(AsyncCallback[p.protocol.ResponseType](_ => Callback.empty))
        ServerSideProcInvoker.viaAsyncCallback((_: p.protocol.RequestType) => c)
      }
    }
}