package shipreq.webapp.client.project.app

import boopickle.Pickler
import japgolly.scalajs.react._
import java.time.Duration
import scala.scalajs.js.typedarray.ArrayBuffer
import shipreq.base.util.JsExt._
import shipreq.base.util._
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.api._
import shipreq.webapp.member.protocol.webworker._
import shipreq.webapp.member.util.LruCache

object WebWorkerClient {

  val protocol = WebWorkerProtocol.default

  type Instance = ManagedWebWorker.Client[WebWorkerCmd, WebWorkerPushCmd, protocol.Reader, protocol.Encoded]

  def apply(worker: AbstractWebWorker.Client,
            onPush: WebWorkerPushCmd => Callback,
            logger: LoggerJs): CallbackTo[Instance] =
    ManagedWebWorker.Client[WebWorkerCmd, WebWorkerPushCmd](
      worker,
      protocol,
      onPush,
      OnError.logToConsole, // TODO do better
      logger
    )

  def default(worker: AbstractWebWorker.Client,
              onPush: WebWorkerPushCmd => Callback,
              logger: LoggerJs): CallbackTo[Instance] =
    apply(worker, onPush, logger).map(addCaching(_, logger))

  def addCaching(instance: Instance, logger: LoggerJs): Instance = {
    type Push = WebWorkerPushCmd

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

      override def modOnPush(f: (Push => Callback) => Push => Callback): Callback =
        instance.modOnPush(f)

      override def encode(cmd: WebWorkerCmd[_]): ArrayBuffer =
        instance.encode(cmd)

      override def postEnc[A](req: WebWorkerCmd[A], enc: ArrayBuffer)(implicit p: Pickler[A]): AsyncCallback[A] = {
        @inline def real = instance.postEnc(req, enc)

        @inline def useCache(c: Cache) = {
          val bin = BinaryData.unsafeFromArrayBuffer(enc)
          val key = bin.binaryLikeString
          c.asyncGetOrSetR(key, real)
        }

        req match {
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

      override def close: Callback =
        instance.close
    }
  }
}
