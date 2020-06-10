package shipreq.webapp.base.protocol.ajax

import boopickle.DefaultBasic._
import shipreq.webapp.base.data._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.protocol.binary._
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.protocol.Protocol

/**
  * Protocols for the Home SPA / webapp-client-home module.
  */
object HomeSpaProtocols {

  type Ajax[Req, Res] = Protocol.Ajax.Simple[SafePickler, Req, Res]

  private def defAjax[Req: SafePickler, Res: SafePickler](path: String): Ajax[Req, Res] =
    Protocol.Ajax.Simple(Urls.ajaxRoot / "hom" / path, Protocol(implicitly), Protocol(implicitly))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object CreateProject {
    type Request = String
    type Response = ProjectMetaData

    val ajax = {
      import v1.BaseMemberData2._

      val picklerRequest: Pickler[Request] = implicitly
      val picklerResponse: Pickler[Response] = implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x42A63E36, 0x0C1B2566)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0xB27B40C3, 0x004A70E7)

      defAjax[Request, Response]("cp")
    }
  }

}