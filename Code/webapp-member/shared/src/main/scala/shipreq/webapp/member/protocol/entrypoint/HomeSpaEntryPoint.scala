package shipreq.webapp.member.protocol.entrypoint

import boopickle.DefaultBasic._
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.protocol.entrypoint.ClientSideProc
import shipreq.webapp.member.project.data.ProjectMetaData
import shipreq.webapp.member.social.UserGroup

object HomeSpaEntryPoint {

  type UserGroupUniverse = UserGroup.Universe[Username, Unit, UserGroup.Id.Public, UserGroup[UserGroup.Id.Public]]

  final case class InitData(username     : Username,
                            projects     : List[ProjectMetaData],
                            userGroups   : UserGroupUniverse,
                            assetManifest: AssetManifest)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._
      import shipreq.webapp.member.project.protocol.binary.v1.Social._

      private[this] implicit val picklerUserGroup: Pickler[UserGroup[UserGroup.Id.Public]] = pickleUserGroup
      private[this] implicit val picklerUserGroups: Pickler[UserGroupUniverse] = pickleUserGroupUniverse

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.username)
        state.pickle(a.projects)
        state.pickle(a.userGroups)
        state.pickle(a.assetManifest)
      }
      override def unpickle(implicit state: UnpickleState): InitData = {
        val username      = state.unpickle[Username]
        val projects      = state.unpickle[List[ProjectMetaData]]
        val userGroups    = state.unpickle[UserGroupUniverse]
        val assetManifest = state.unpickle[AssetManifest]
        InitData(username, projects, userGroups, assetManifest)
      }
    }

  final val Name = "H"

  val proc = ClientSideProc[InitData](Name)

}
