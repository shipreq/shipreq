package shipreq.webapp.base.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.user.Username

object ProjectSpaEntryPoint {

  final case class InitData(username      : Username,
                            projectId     : ProjectId.Public,
                            projectName   : Project.Name,
                            assetManifest : AssetManifest,
                            webWorkerJsUrl: String)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projectId)
        state.pickle(a.projectName)
        state.pickle(a.assetManifest)
        state.pickle(a.webWorkerJsUrl)
      }

      override def unpickle(implicit state: UnpickleState): InitData = {
        val username       = state.unpickle[Username]
        val projectId      = state.unpickle[ProjectId.Public]
        val projectName    = state.unpickle[Project.Name]
        val assetManifest  = state.unpickle[AssetManifest]
        val webWorkerJsUrl = state.unpickle[String]
        InitData(username, projectId, projectName, assetManifest, webWorkerJsUrl)
      }
    }

  final val Name = "P"

  val proc = ClientSideProc[InitData](Name)
}
