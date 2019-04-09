package shipreq.webapp.base.protocol2

import boopickle._
import japgolly.scalajs.react.AsyncCallback
import japgolly.scalajs.react.extra.Ajax
import org.scalajs.dom.ext.AjaxException

trait AjaxClient[F[_]] {
  def apply(p: Protocol.Ajax[F])
           (req: p.protocol.RequestType): AsyncCallback[p.protocol.ResponseType]
}

object AjaxClient {

  type Binary = AjaxClient[Pickler]

  val Binary: Binary =
    new AjaxClient[Pickler] {
      override def apply(p: Protocol.Ajax[Pickler])
                        (req: p.protocol.RequestType): AsyncCallback[p.protocol.ResponseType] = {

        val prep = p.protocol.prepareSend(req)

        val reqAB = BinaryJs.encodeToArrayBuffer(prep.request)(p.protocolPrepReq.codec)

        Ajax("POST", p.url.relativeUrl)
          .setRequestHeader("Content-Type", "application/octet-stream")
          .and(_.responseType = "arraybuffer")
          .send(reqAB)
          .asAsyncCallback
          .map { xhr =>
            if (xhr.status == 200)
              BinaryJs.decodeFromArrayBufferUnsafe(xhr.response)(prep.response.codec)
            else
              throw AjaxException(xhr)
          }
      }
    }

}