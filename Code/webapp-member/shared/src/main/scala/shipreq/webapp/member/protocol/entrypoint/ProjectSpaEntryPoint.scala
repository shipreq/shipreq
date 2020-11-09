package shipreq.webapp.member.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.{ProjectId, Username}
import shipreq.webapp.base.protocol.entrypoint.ClientSideProc
import shipreq.webapp.member.project.data.{ClientSideProjectEncryptionKey, Project}

object ProjectSpaEntryPoint {

  final case class InitData(username      : Username,
                            projectId     : ProjectId.Public,
                            projectName   : Project.Name,
                            assetManifest : AssetManifest,
                            webWorkerJsUrl: String,
                            encryptionKey : ClientSideProjectEncryptionKey)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.member.project.protocol.binary.v1.Rev8._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projectId)
        state.pickle(a.projectName)
        state.pickle(a.assetManifest)
        state.pickle(a.webWorkerJsUrl)
        state.pickle(a.encryptionKey)
      }

      override def unpickle(implicit state: UnpickleState): InitData = {
        val username       = state.unpickle[Username]
        val projectId      = state.unpickle[ProjectId.Public]
        val projectName    = state.unpickle[Project.Name]
        val assetManifest  = state.unpickle[AssetManifest]
        val webWorkerJsUrl = state.unpickle[String]
        val encryptionKey  = state.unpickle[ClientSideProjectEncryptionKey]
        InitData(username, projectId, projectName, assetManifest, webWorkerJsUrl, encryptionKey)
      }
    }

  final val Name = "P"

  val proc = ClientSideProc[InitData](Name)
}
