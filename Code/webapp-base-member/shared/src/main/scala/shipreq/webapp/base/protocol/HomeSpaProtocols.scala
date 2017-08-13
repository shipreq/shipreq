package shipreq.webapp.base.protocol

import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecMemberData._
import BinCodecUser._

/**
  * Protocols for the Home SPA / webapp-client-home module.
  */
object HomeSpaProtocols {

  val CreateProject = ServerSideProc.Protocol[String, ProjectMetaData]("Home.CreateProject")

  import CreateProject.{pickleInstance => _i1}

  final case class InitData(username     : Username,
                            projects     : List[ProjectMetaData],
                            createProject: CreateProject.Instance)

  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "H"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}