package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.ProjectLibrary
import shipreq.webapp.member.protocol.binary.Encryption
import shipreq.webapp.member.protocol.indexeddb.IndexedDb

object ClientSideStorage {

  trait ReadOnly {
    def getProjectLibraryOrd: AsyncCallback[Option[EventOrd.Latest]]
    def getProjectLibrary: AsyncCallback[Option[ProjectLibrary]]
  }

  trait ReadWrite extends ReadOnly {
    def saveProjectLibrary(pl: ProjectLibrary): AsyncCallback[Unit]
  }

  // ===================================================================================================================

  object ReadWrite {

    type Available = Context => AsyncCallback[ReadWrite]

    def apply(): Option[Available] =
      Encryption.Engine.global.flatMap { crypto =>

        def indexedDb: Option[Available] =
          IndexedDb.global().map(idb => usingIndexedDb(_, crypto, idb))

        def localStorage: Option[Available] =
          AbstractWebStorage.local().map(ws => usingWebStorage(_, crypto, ws))

        indexedDb orElse localStorage
      }

    def usingIndexedDb(ctx: Context, crypto: Encryption.Engine, idb: IndexedDb): AsyncCallback[ReadWrite] =
      crypto(ctx.encKey.value).flatMap(IndexedDbStorage(idb, ctx, _))

    def usingWebStorage(ctx: Context, crypto: Encryption.Engine, ws: AbstractWebStorage): AsyncCallback[ReadWrite] =
      crypto(ctx.encKey.value).map(new WebStorage(ws, ctx, _))
  }

  // -------------------------------------------------------------------------------------------------------------------

  object ReadOnly {

    type Available = Context => AsyncCallback[ReadOnly]

    def apply(): Option[Available] =
      ReadWrite().map(_.andThen(f => f))
  }

}