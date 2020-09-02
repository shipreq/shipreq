package shipreq.webapp.client.public

import boopickle.DefaultBasic._
import shipreq.base.util.Permission
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.protocol.entrypoint.ClientSideProc
import shipreq.webapp.base.user.Username

object PublicSpaEntryPoint {

  final case class InitData(publicRegistration: Permission,
                            loggedInUser      : Option[Username],
                            assetManifest     : AssetManifest)

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.publicRegistration)
        state.pickle(a.loggedInUser)
        state.pickle(a.assetManifest)
      }

      override def unpickle(implicit state: UnpickleState): InitData = {
        val publicRegistration = state.unpickle[Permission]
        val loggedInUser       = state.unpickle[Option[Username]]
        val assetManifest      = state.unpickle[AssetManifest]
        InitData(publicRegistration, loggedInUser, assetManifest)
      }
    }

  final val Name = "A"

  val proc = ClientSideProc[InitData](Name)

}
