package shipreq.webapp.ssr

import boopickle.DefaultBasic._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.{Project, Username}
import shipreq.webapp.base.protocol.binary.v1.BaseData._

/** Values herein are all names of top-level functions in [SsrJs],
  * that can be called from [SsrInterpreter] on the JVM.
  */
object SsrJsFunctionManifest {
  final val PublicLoader     = "publicLoader"
  final val HomeSpaLoader    = "homeSpaLoader"
  final val ProjectSpaLoader = "projectSpaLoader"
}

object SsrSharedData {

  type PublicInitData = shipreq.webapp.client.public.PublicSpaEntryPoint.InitData
  val  PublicInitData = shipreq.webapp.client.public.PublicSpaEntryPoint.InitData

  // -------------------------------------------------------------------------------------------------------------------

  final case class HomeSpaLoaderData(username     : Username,
                                     assetManifest: AssetManifest)

  implicit val picklerHomeSpaLoaderData: Pickler[HomeSpaLoaderData] =
    new Pickler[HomeSpaLoaderData] {
      override def pickle(a: HomeSpaLoaderData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.assetManifest)
      }
      override def unpickle(implicit state: UnpickleState): HomeSpaLoaderData = {
        val username      = state.unpickle[Username]
        val assetManifest = state.unpickle[AssetManifest]
        HomeSpaLoaderData(username, assetManifest)
      }
    }

  // -------------------------------------------------------------------------------------------------------------------

  final case class ProjectSpaLoaderData(username     : Username,
                                        projectName  : Project.Name,
                                        assetManifest: AssetManifest)

  implicit val picklerProjectSpaLoaderData: Pickler[ProjectSpaLoaderData] =
    new Pickler[ProjectSpaLoaderData] {
      override def pickle(a: ProjectSpaLoaderData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projectName)
        state.pickle(a.assetManifest)
      }
      override def unpickle(implicit state: UnpickleState): ProjectSpaLoaderData = {
        val username      = state.unpickle[Username]
        val projectName   = state.unpickle[Project.Name]
        val assetManifest = state.unpickle[AssetManifest]
        ProjectSpaLoaderData(username, projectName, assetManifest)
      }
    }

}