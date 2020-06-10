package shipreq.webapp.base.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.user.Username

object HomeSpaEntryPoint {

  final case class InitData(username: Username,
                            projects: List[ProjectMetaData])

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.base.protocol.binary.v1.BaseMemberData2._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projects)
      }
      override def unpickle(implicit state: UnpickleState): InitData = {
        val username = state.unpickle[Username]
        val projects = state.unpickle[List[ProjectMetaData]]
        InitData(username, projects)
      }
    }

  final val Name = "H"

  val proc = ClientSideProc[InitData](Name)

}
