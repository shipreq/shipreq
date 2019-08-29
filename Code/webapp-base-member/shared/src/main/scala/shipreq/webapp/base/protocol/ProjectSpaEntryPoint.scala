package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import shipreq.webapp.base.data.{Project, ProjectId}
import shipreq.webapp.base.user.Username

object ProjectSpaEntryPoint {

  final case class InitData(username: Username,
                            projectId: ProjectId.Public,
                            projectName: Project.Name)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.CodecBaseV1._
      import shipreq.webapp.base.protocol.binary.CodecBaseMemberV1._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projectId)
        state.pickle(a.projectName)
      }
      override def unpickle(implicit state: UnpickleState): InitData = {
        val username    = state.unpickle[Username]
        val projectId   = state.unpickle[ProjectId.Public]
        val projectName = state.unpickle[Project.Name]
        InitData(username, projectId, projectName)
      }
    }

  final val Name = "P"

  val proc = ClientSideProc[InitData](Name)
}
