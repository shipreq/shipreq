package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.member.test.TestEncryption

object WebStorageLaws extends ClientSideStorageLaws {

  override protected def createInstance(ctx: Context) =
    AsyncCallback.delay(AbstractWebStorage.inMemory()).flatMap(
      ClientSideStorage.ReadWrite.usingWebStorage(ctx, TestEncryption.engine, _))
}
