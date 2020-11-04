package shipreq.webapp.base.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.{ProjectMetaData, Username}

object HomeSpaEntryPoint {

  final case class InitData(username     : Username,
                            projects     : List[ProjectMetaData],
                            assetManifest: AssetManifest)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projects)
        state.pickle(a.assetManifest)
      }
      override def unpickle(implicit state: UnpickleState): InitData = {
        val username      = state.unpickle[Username]
        val projects      = state.unpickle[List[ProjectMetaData]]
        val assetManifest = state.unpickle[AssetManifest]
        InitData(username, projects, assetManifest)
      }
    }

  final val Name = "H"

  val proc = ClientSideProc[InitData](Name)

}
