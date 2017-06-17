package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._

/**
  * Protocols for the Home SPA / webapp-client-home module.
  */
object HomeSpaProtocols {

  val CreateProject = ServerSideProc.Protocol[String, ProjectCatalogue.Item]

  import CreateProject.{pickleInstance => _i1}

  final case class InitClient(username     : Username,
                              projects     : ProjectCatalogue,
                              createProject: CreateProject.Instance)

  implicit val picklerInitClient = pickleCaseClass[InitClient]

  final val EntryPointName = "H"
  val EntryPoint = ClientSideProc[InitClient](EntryPointName)
}