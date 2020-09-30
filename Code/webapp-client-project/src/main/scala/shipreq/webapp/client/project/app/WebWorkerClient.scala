package shipreq.webapp.client.project.app

import boopickle.Pickler
import japgolly.scalajs.react.AsyncCallback
import java.time.Duration
import org.scalajs.dom.webworkers.Worker
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.JsExt._
import shipreq.base.util._
import shipreq.webapp.base.lib.{LoggerJs, LruCache}
import shipreq.webapp.client.ww.api.Protocol.Codec.{default => codec}
import shipreq.webapp.client.ww.api._

object WebWorkerClient {

  type Instance = Client[WebWorkerCmd, codec.Reader, codec.Encoded]

  def apply(wwJsUrl: String, logger: LoggerJs): Instance = {
    val real = withoutCache(wwJsUrl, logger)
    cache(real, logger)
  }

  def withoutCache(wwJsUrl: String, logger: LoggerJs): Instance = {
    lazy val worker = new Worker(wwJsUrl)
    Client.default[WebWorkerCmd](worker, logger)
  }

  def cache(instance: Instance, logger: LoggerJs): Instance = {

    type Cache = LruCache.ToAny[String]

    def newCache(dur: Duration): Cache =
      LruCache.toAny[String]
        .maxAge(dur)
        .maxSize(1_000_000)
        .sizeBy((k, _) => k.length)
        .updateAgeOnGet(true)
        .withLogger(logger)
        .build()

    val cacheInIsolation =
      newCache(Duration.ofDays(365))

    val cacheByProject =
      newCache(Duration.ofHours(12))

    val clearProjectCache =
      cacheByProject.reset.asAsyncCallback

    new Instance {
      override def encode(cmd: WebWorkerCmd[_]): ArrayBuffer =
        instance.encode(cmd)

      override def postEnc[A](cmd: WebWorkerCmd[A], enc: ArrayBuffer)(implicit p: Pickler[A]): AsyncCallback[A] = {
        @inline def real = instance.postEnc(cmd, enc)

        @inline def useCache(c: Cache) = {
          val bin = BinaryData.unsafeFromArrayBuffer(enc)
          val key = bin.binaryLikeString
          c.getOrSetR(key, real)
        }

        cmd match {
          case _: WebWorkerCmd.GraphAllImplications
             | _: WebWorkerCmd.GraphReqImplications
             | _: WebWorkerCmd.GraphUseCaseFlow =>
            useCache(cacheByProject)

          case _: WebWorkerCmd.GraphInline =>
            useCache(cacheInIsolation)

          case _: WebWorkerCmd.UpdateProject =>
            clearProjectCache >> real

          case _: WebWorkerCmd.Init =>
            real
        }
      }
    }
  }
}
