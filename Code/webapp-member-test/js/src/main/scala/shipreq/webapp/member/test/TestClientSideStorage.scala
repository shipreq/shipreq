package shipreq.webapp.member.test

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.{ProjectCreator, ProjectId, UserId}
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data.ClientSideProjectEncryptionKey
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.ProjectLibrary
import shipreq.webapp.member.project.storage.ClientSideStorage

final class TestClientSideStorage(override protected val creator: ProjectCreator) extends ClientSideStorage.ReadWrite {
  var available = true

  private var projectStore = Option.empty[ProjectLibrary]

  override val isAvailable: CallbackTo[Boolean] =
    CallbackTo(available)

  override def saveProjectLibrary(pl: ProjectLibrary): AsyncCallback[Unit] =
    AsyncCallback.delay {
      if (pl > projectStore.flatMap(_.ord)) {
        projectStore = Some(pl.withoutFutureEvents)
      }
    }

  override def clear: AsyncCallback[Unit] =
    AsyncCallback.delay {
      projectStore = None
    }

  override def getProjectLibrary: AsyncCallback[Option[ProjectLibrary]] =
    AsyncCallback.delay(projectStore)

  override def getProjectLibraryOrd: AsyncCallback[Option[EventOrd.Latest]] =
    getProjectLibrary.map(_.flatMap(_.ord))

  def projectLibrary(): Option[ProjectLibrary] =
    projectStore

  def ordAsInt(): Int =
    projectStore.flatMap(_.ord).fold(0)(_.value)
}

object TestClientSideStorage {

  def apply(creator: ProjectCreator = ProjectCreator(Obfuscated(""))): TestClientSideStorage =
    new TestClientSideStorage(creator)

  def provide(instance: => ClientSideStorage.ReadWrite): ClientSideStorage.ReadWrite.Provider =
    (_, _) => AsyncCallback.delay(instance)

  val provider: ClientSideStorage.ReadWrite.Provider =
    provide(apply())

  private def userId(i: Int): UserId.Public =
    Obfuscated("user-" + i)

  private def projectId(i: Int): ProjectId.Public =
    Obfuscated("project-" + i)

  private val padding = "_" * 32

  def encKey(s: String): ClientSideProjectEncryptionKey = {
    assert(s.length <= 32)
    val s2 = (s + padding).take(32)
    ClientSideProjectEncryptionKey(BinaryData.fromStringBytes(s2))
  }

  val u1p1 = ClientSideStorage.Context(userId(1), projectId(1), ProjectCreator(userId(2)))
  val u2p1 = ClientSideStorage.Context(userId(2), projectId(1), ProjectCreator(userId(2)))
  val u1p2 = ClientSideStorage.Context(userId(1), projectId(2), ProjectCreator(userId(1)))

  val key_u1p1 = encKey("u1p1")
  val key_u2p1 = encKey("u2p1")
  val key_u1p2 = encKey("u1p2")
}