package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import monocle.macros.{GenIso, Lenses}
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.protocol.binary._
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.user._
import shipreq.webapp.base.validation._

object CommonProtocols {

  type Ajax[Req, Res] = Protocol.Ajax.Simple[SafePickler, Req, Res]

  private def defAjax[Req: SafePickler, Res: SafePickler](path: String): Ajax[Req, Res] =
    Protocol.Ajax.Simple(Urls.ajaxRoot / path, Protocol(implicitly), Protocol(implicitly))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Login {

    final case class Request(user: Username \/ EmailAddr, password: PlainTextPassword) {
      def untyped: Request.Untyped =
        Request.Untyped(user.fold(_.value, _.value), password.value)
    }

    object Request {

      @Lenses
      final case class Untyped(user: String, password: String) {
        def validate: Composite.Invalidity \/ Request =
          Request.validator(this)
      }

      lazy val validator: Composite.Validator[Untyped, _, Request] =
        UserValidators.usernameOrEmail
          .tuple(UserValidators.password.named)
          .imapInput(GenIso.fields[Untyped])
          .mapValid((Request.apply _).tupled)

      implicit def univEq: UnivEq[Request] = UnivEq.derive
    }

    type Response = Permission

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.user)
            state.pickle(a.password)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val user     = state.unpickle[Username \/ EmailAddr]
            val password = state.unpickle[PlainTextPassword]
            Request(user, password)
          }
        }

      val picklerResponse: Pickler[Response] =
        implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV10.withMagicNumbers(0x8AB0DAD1, 0x38E21961)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV10.withMagicNumbers(0xBAD9BE35, 0xBCACEC71)

      defAjax("login")
    }
  }

}
