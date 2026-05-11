package shipreq.webapp.member.project.storage

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import shipreq.webapp.base.data.{ProjectCreator, ProjectId, UserId}
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.{CacheJs, ProjectLibrary}
import shipreq.webapp.member.protocol.binary.Encryption
import shipreq.webapp.member.protocol.indexeddb.IndexedDb

object ClientSideStorage {

  trait ReadOnly {
    protected def creator: ProjectCreator

    def isAvailable: CallbackTo[Boolean]
    def getProjectLibraryOrd: AsyncCallback[Option[EventOrd.Latest]]
    def getProjectLibrary: AsyncCallback[Option[ProjectLibrary]]

    final def getProjectLibraryOrEmpty: AsyncCallback[ProjectLibrary] =
      getProjectLibrary.map(_.getOrElse(ProjectLibrary.empty(creator, CacheJs(creator))))
  }

  trait ReadWrite extends ReadOnly {
    def saveProjectLibrary(pl: ProjectLibrary): AsyncCallback[Unit]
    def clear: AsyncCallback[Unit]

    final def disabled: ReadWrite =
      ReadWrite.AlwaysEmpty(creator)
  }

  // ===================================================================================================================

  object ReadWrite {

    type Provider = (Context, ClientSideProjectEncryptionKey) => AsyncCallback[ReadWrite]

    def apply(ctx: Context, encKey: ClientSideProjectEncryptionKey): AsyncCallback[ReadWrite] =
      get(ctx, encKey).getOrElse(AsyncCallback.pure(AlwaysEmpty(ctx.creator)))

    def get(ctx: Context, encKey: ClientSideProjectEncryptionKey): Option[AsyncCallback[ReadWrite]] =
      Encryption.Engine.global.flatMap { crypto =>

        val enc = crypto(encKey.value).memo()

        Dynamic.optionAsync(ctx.creator)(
          // highest priority
          IndexedDb.global().map(idb => enc.flatMap(IndexedDbStorage(idb, ctx, _))),
          AbstractWebStorage.local().map(ws => enc.map(new WebStorage(ws, ctx, _))),
          // lowest priority
        )
      }

    private final case class AlwaysEmpty(override protected val creator: ProjectCreator) extends ReadWrite {
      private val none                                    = AsyncCallback.pure(Option.empty[Nothing])
      override val isAvailable                            = CallbackTo.pure(false)
      override def getProjectLibraryOrd                   = none
      override def getProjectLibrary                      = none
      override def saveProjectLibrary(pl: ProjectLibrary) = AsyncCallback.unit
      override def clear                                  = AsyncCallback.unit
    }

    object Dynamic {

      def apply(creator: ProjectCreator)(highestPriorityFirst: ReadWrite*): ReadWrite =
        highestPriorityFirst.size match {
          case 0 => AlwaysEmpty(creator)
          case 1 => highestPriorityFirst.head
          case _ => new Dynamic(creator, highestPriorityFirst.toList)
        }

      def async(creator: ProjectCreator)(highestPriorityFirst: AsyncCallback[ReadWrite]*): AsyncCallback[ReadWrite] =
        AsyncCallback.sequence(highestPriorityFirst.toList)
          .map(as => apply(creator)(as: _*))

      def optionAsync(creator: ProjectCreator)(highestPriorityFirst: Option[AsyncCallback[ReadWrite]]*): Option[AsyncCallback[ReadWrite]] = {
        val as = highestPriorityFirst.iterator.filterDefined.toList
        Option.when(as.nonEmpty)(async(creator)(as: _*))
      }
    }

    private final class Dynamic(override protected val creator: ProjectCreator, highestPriorityFirst: List[ReadWrite]) extends ReadWrite {

      private val firstAvailable: CallbackTo[ReadWrite] =
        CallbackTo {
          @tailrec
          def go(rws: List[ReadWrite]): ReadWrite =
            if (rws.isEmpty)
              AlwaysEmpty(creator)
            else {
              val rw = rws.head
              if (rw.isAvailable.runNow())
                rw
              else
                go(rws.tail)
            }
          go(highestPriorityFirst)
        }

      private val firstAvailableAsync: AsyncCallback[ReadWrite] =
        firstAvailable.asAsyncCallback

      @inline private def proxy[A](f: ReadWrite => AsyncCallback[A]): AsyncCallback[A] =
        firstAvailableAsync.flatMap(f)

      override val isAvailable                            = firstAvailable.flatMap(_.isAvailable)
      override val getProjectLibraryOrd                   = proxy(_.getProjectLibraryOrd)
      override val getProjectLibrary                      = proxy(_.getProjectLibrary)
      override def saveProjectLibrary(pl: ProjectLibrary) = proxy(_.saveProjectLibrary(pl))
      override def clear                                  = proxy(_.clear)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  object ReadOnly {

    def apply(userId: UserId.Public, projectId: ProjectId.Public, creator: ProjectCreator, encKey: ClientSideProjectEncryptionKey): AsyncCallback[ReadOnly] =
      apply(Context(userId, projectId, creator), encKey)

    def apply(ctx: Context, encKey: ClientSideProjectEncryptionKey): AsyncCallback[ReadOnly] =
      ReadWrite(ctx, encKey)

    def get(ctx: Context, encKey: ClientSideProjectEncryptionKey): Option[AsyncCallback[ReadOnly]] =
      ReadWrite.get(ctx, encKey).map(f => f)
  }

  // -------------------------------------------------------------------------------------------------------------------

  final case class Context(userId: UserId.Public, projectId: ProjectId.Public, creator: ProjectCreator) {

    // Note: no need to include creator here cos it's an immutible detail per project; projectId covers it
    val namespace: String =
      s"${userId.value}:${projectId.value}"
  }

}