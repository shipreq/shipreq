package shipreq.webapp.base.protocol

import boopickle.Pickler
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.user._
import shipreq.webapp.base.Urls
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecUser._
import BinCodecMemberData._

/**
  * Protocols for the Home SPA / webapp-client-home module.
  */
object HomeSpaProtocols {

  private def ajax[Req: Pickler, Res: Pickler](path: String): Protocol.Ajax.Simple[Pickler, Req, Res] =
    Protocol.Ajax.Simple(Urls.ajaxRoot / "h" / path, Protocol(implicitly), Protocol(implicitly))

  val createProject = ajax[String, ProjectMetaData]("p")

  final case class InitData(username: Username,
                            projects: List[ProjectMetaData])

  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "H"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}