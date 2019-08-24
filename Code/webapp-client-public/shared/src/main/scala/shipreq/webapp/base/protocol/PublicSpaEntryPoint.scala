package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import shipreq.base.util.Permission
import shipreq.webapp.base.user.Username

object PublicSpaEntryPoint {

  final case class InitData(publicRegistration: Permission,
                            loggedInUser      : Option[Username])

  implicit val picklerInitData: Pickler[InitData] =
    new Pickler[InitData] {
      import shipreq.webapp.base.protocol.binary.CodecBaseV1._

      override def pickle(a: InitData)(implicit state: PickleState): Unit = {
        state.pickle(a.publicRegistration)
        state.pickle(a.loggedInUser)
      }

      override def unpickle(implicit state: UnpickleState): InitData = {
        val publicRegistration = state.unpickle[Permission]
        val loggedInUser       = state.unpickle[Option[Username]]
        InitData(publicRegistration, loggedInUser)
      }
    }

  final val Name = "A"

  val proc = ClientSideProc[InitData](Name)

}
