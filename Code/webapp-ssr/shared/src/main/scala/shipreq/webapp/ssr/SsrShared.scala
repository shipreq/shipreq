package shipreq.webapp.ssr

import boopickle.DefaultBasic._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.binary.v1.BaseData._
import shipreq.webapp.base.user.Username

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

  final case class HomeSpaLoaderData(username: Username)

  implicit val picklerHomeSpaLoaderData: Pickler[HomeSpaLoaderData] =
    transformPickler(HomeSpaLoaderData.apply)(_.username)

  // -------------------------------------------------------------------------------------------------------------------

  final case class ProjectSpaLoaderData(username: Username, projectName: Project.Name)

  implicit val picklerProjectSpaLoaderData: Pickler[ProjectSpaLoaderData] =
    new Pickler[ProjectSpaLoaderData] {
      override def pickle(a: ProjectSpaLoaderData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projectName)
      }
      override def unpickle(implicit state: UnpickleState): ProjectSpaLoaderData = {
        val username    = state.unpickle[Username]
        val projectName = state.unpickle[Project.Name]
        ProjectSpaLoaderData(username, projectName)
      }
    }

}