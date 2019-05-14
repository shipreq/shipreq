package shipreq.webapp.base.protocol

import boopickle._
import japgolly.scalajs.react.extra.Ajax
import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import org.scalajs.dom.ext.AjaxException
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.ErrorMsg
import shipreq.base.util.JsExt._

trait AjaxClient[F[_]] {

  def apply(p: Protocol.Ajax[F])(req: p.protocol.RequestType): CallbackTo[AsyncCallback[p.protocol.ResponseType]]

  final def invoker(p: Protocol.Ajax[F]): ServerSideProcInvoker[p.protocol.RequestType, ErrorMsg, p.protocol.ResponseType] =
    ServerSideProcInvoker.viaAsyncCallback(apply(p)(_))
}

object AjaxClient {

  type Binary = AjaxClient[Pickler]

  val Binary: Binary =
    new AjaxClient[Pickler] {
      override def apply(p: Protocol.Ajax[Pickler])(req: p.protocol.RequestType) =
        CallbackTo {

          val prep = p.protocol.prepareSend(req)

          val reqAB = BinaryJs.encode(p.prepReq)(prep.request).unsafeArrayBuffer

          val result: AsyncCallback[p.protocol.ResponseType] =
            Ajax("POST", p.url.relativeUrl)
              .setRequestHeader("Content-Type", "application/octet-stream")
              .and(_.responseType = "arraybuffer")
              .send(reqAB)
              .asAsyncCallback
              .map { xhr =>
                if (xhr.status == 200) {
                  val ab = xhr.response.asInstanceOf[ArrayBuffer]
                  BinaryJs.decodeUnsafeFromArrayBuffer(ab, prep.response)
                } else
                  throw AjaxException(xhr)
              }

          result
        }
    }

}