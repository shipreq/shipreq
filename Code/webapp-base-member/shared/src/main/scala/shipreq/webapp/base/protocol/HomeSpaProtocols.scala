package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecData._

/**
  * Protocols for the Home SPA / webapp-client-home module.
  */
object HomeSpaProtocols {

  val CreateProject = ServerSideProc.Protocol[String, ProjectMetaData]

  import CreateProject.{pickleInstance => _i1}

  final case class InitData(username     : Username,
                            projects     : List[ProjectMetaData],
                            createProject: CreateProject.Instance)

  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "H"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}