package shipreq.webapp.ssr

/** Values herein are all names of top-level functions in [SsrJs],
  * that can be called from [SsrInterpreter] on the JVM.
  */
object SsrJsFunctionManifest {
  final val Public           = "public"
  final val HomeSpaLoader    = "homeSpaLoader"
  final val ProjectSpaLoader = "projectSpaLoader"
}

object SsrSharedData {
  import shipreq.webapp.base.user.Username
  import shipreq.webapp.base.data.Project
  import shipreq.webapp.base.protocol.BinCodecGeneric._
  import shipreq.webapp.base.protocol.BinCodecUser._
  import shipreq.webapp.base.protocol.BoopickleMacros.pickleCaseClass

  type PublicInitData = shipreq.webapp.client.public.PublicSpaEntryPoint.InitData
  val  PublicInitData = shipreq.webapp.client.public.PublicSpaEntryPoint.InitData

  final case class HomeSpaLoaderData(username: Username)
  implicit val picklerHomeSpaLoaderData = pickleCaseClass[HomeSpaLoaderData]

  final case class ProjectSpaLoaderData(username: Username, projectName: Project.Name)
  implicit val picklerProjectSpaLoaderData = pickleCaseClass[ProjectSpaLoaderData]

}