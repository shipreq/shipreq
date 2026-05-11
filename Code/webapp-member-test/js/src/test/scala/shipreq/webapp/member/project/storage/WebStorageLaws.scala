package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.member.test.TestEncryption

object WebStorageLaws extends ClientSideStorageLaws {

  override protected def createInstance =
    (ctx, key) =>
      for {
        ws  <- AsyncCallback.delay(AbstractWebStorage.inMemory())
        enc <- TestEncryption.engine(key.value)
      } yield new WebStorage(ws, ctx, enc)
}
