package shipreq.webapp.ssr

import shipreq.webapp.base.user.Username
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.protocol.BinCodecGeneric._
import shipreq.webapp.base.protocol.BinCodecUser._
import shipreq.webapp.base.protocol.BoopickleMacros.pickleCaseClass

final case class ProjectSpaLoaderData(username: Username, projectName: Project.Name)

object ProjectSpaLoaderData {
  implicit val pickler = pickleCaseClass[ProjectSpaLoaderData]
}