package shipreq.webapp.member.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.entrypoint.ClientSideProc
import shipreq.webapp.member.project.data._

object ProjectSpaEntryPoint {

  final case class InitData(username      : Username,
                            userId        : UserId.Public,
                            projectId     : ProjectId.Public,
                            creator       : ProjectCreator,
                            projectName   : Project.Name,
                            assetManifest : AssetManifest,
                            webWorkerJsUrl: String,
                            encryptionKey : ClientSideProjectEncryptionKey) {

    def withoutEncKey: InitDataWithoutEncKey =
      InitDataWithoutEncKey(
        username       = username,
        userId         = userId,
        projectId      = projectId,
        creator        = creator,
        projectName    = projectName,
        assetManifest  = assetManifest,
        webWorkerJsUrl = webWorkerJsUrl,
      )
  }

  object InitData {
    implicit def univEq: UnivEq[InitData] = UnivEq.derive
  }

  /** Same as [[InitData]] except it excludes [[ClientSideProjectEncryptionKey]].
   * For reasons described in the [[ClientSideProjectEncryptionKey]] doc, we want to make the encryption key
   * garbage-collectable as soon as possible.
   */
  final case class InitDataWithoutEncKey(username      : Username,
                                         userId        : UserId.Public,
                                         projectId     : ProjectId.Public,
                                         creator       : ProjectCreator,
                                         projectName   : Project.Name,
                                         assetManifest : AssetManifest,
                                         webWorkerJsUrl: String)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.member.project.protocol.binary.v2.Rev0._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.userId)
        state.pickle(a.projectId)
        state.pickle(a.creator)
        state.pickle(a.projectName)
        state.pickle(a.assetManifest)
        state.pickle(a.webWorkerJsUrl)
        state.pickle(a.encryptionKey)
      }

      override def unpickle(implicit state: UnpickleState): InitData = {
        val username       = state.unpickle[Username]
        val userId         = state.unpickle[UserId.Public]
        val projectId      = state.unpickle[ProjectId.Public]
        val creator        = state.unpickle[ProjectCreator]
        val projectName    = state.unpickle[Project.Name]
        val assetManifest  = state.unpickle[AssetManifest]
        val webWorkerJsUrl = state.unpickle[String]
        val encryptionKey  = state.unpickle[ClientSideProjectEncryptionKey]
        InitData(username, userId, projectId, creator, projectName, assetManifest, webWorkerJsUrl, encryptionKey)
      }
    }

  final val Name = "P"

  val proc = ClientSideProc[InitData](Name)
}
