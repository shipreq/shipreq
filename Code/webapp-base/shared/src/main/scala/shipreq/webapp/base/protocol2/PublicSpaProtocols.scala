package shipreq.webapp.base.protocol2

import boopickle.Pickler
import japgolly.univeq.UnivEq
import monocle.macros.{GenIso, Lenses}
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.protocol.{ServerSideProc => _, _}
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.user._
import shipreq.webapp.base.validation._
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.Urls
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecUser._

/**
  * Protocols for the Public SPA / webapp-client-public module.
  */
object PublicSpaProtocols {

  private def newAjax[Req: Pickler, Res: Pickler](path: String): Protocol.Ajax.Simple[Pickler, Req, Res] =
    Protocol.Ajax.Simple(Urls.ajaxRoot / "pub" / path, Protocol(implicitly), Protocol(implicitly))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object LandingPage {
    final case class Request(name      : PersonName,
                             email     : EmailAddr,
                             msg       : Option[String],
                             newsletter: Boolean) {
      def untyped: Request.Untyped =
        Request.Untyped(name.value, email.value, msg getOrElse "", newsletter)
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

      def labelName  = "Your name"
      def labelEmail = "Your email address"

      def validatorName  = UserValidators.personName.unnamed
      def validatorEmail = UserValidators.emailAddr.unnamed
      def validatorMsg   = CommonValidation.optionalLargeText.mapCorrector(_ prependLive TextMod.maxTwoConsecutiveNewLines.run)

      @Lenses
      final case class Untyped(name      : String,
                               email     : String,
                               msg       : String,
                               newsletter: Boolean) {
        def validate: Composite.Invalidity \/ Request =
          Request.validator(this)
      }

      lazy val validator: Composite.Validator[Untyped, _, Request] =
        validatorName.named(labelName).named
          .tuple(validatorEmail.named(labelEmail).named)
          .tuple(validatorMsg.named("Your message").named)
          .tuple(Composite.Validator.id[Boolean])
          .imapInput(GenIso.fields[Untyped])
          .mapValid((Request.apply _).tupled)
    }

    val ajax = newAjax[Request, ErrorMsg \/ Unit]("lp")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Register {
    val ajax1 = newAjax[EmailAddr, ErrorMsg \/ Unit]("reg1")

    final case class Request(token     : SecurityToken,
                             personName: PersonName,
                             username  : Username,
                             password  : PlainTextPassword,
                             newsletter: Boolean) {

      def validate: Composite.Invalidity \/ Request =
        Request.validator((token, personName.value, username.value, password.value, newsletter))
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass

      // Only used on server-side - Request has less than the registration form
      lazy val validator: Composite.Validator[(SecurityToken, String, String, String, Boolean), _, Request] =
        Composite.Validator.id[SecurityToken]
          .tuple(UserValidators.personName.named)
          .tuple(UserValidators.username.stateless.named)
          .tuple(UserValidators.password.named)
          .tuple(Composite.Validator.id[Boolean])
          .mapValid((Request.apply _).tupled)
    }

    sealed trait Response
    object Response {
      sealed trait Terminal     extends Response
      case object Success       extends Terminal
      case object TokenInvalid  extends Terminal
      case object TokenExpired  extends Terminal
      case object UsernameTaken extends Response

      implicit val pickler: Pickler[Response] = derivePickler[Response]
      implicit def univEq: UnivEq[Response] = UnivEq.derive
    }

    /** Upon successful submission the user account is activated. */
    val ajax2 = newAjax[Request, ErrorMsg \/ Response]("reg2")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Login {
    final case class Request(user: Username \/ EmailAddr, password: PlainTextPassword) {
      def untyped: Request.Untyped =
        Request.Untyped(user.fold(_.value, _.value), password.value)
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

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
    }

    val ajax = newAjax[Request, Permission]("l")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword {
    val ajax1 = newAjax[Username \/ EmailAddr, Unit]("rp1")

    final case class Request(token: SecurityToken, newPassword: PlainTextPassword)
    implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

    sealed trait Response
    object Response {
      case object Success      extends Response
      case object TokenInvalid extends Response
      case object TokenExpired extends Response

      implicit val pickler: Pickler[Response] = derivePickler[Response]
      implicit def univEq: UnivEq[Response] = UnivEq.derive
    }

    val ajax2 = newAjax[Request, ErrorMsg \/ Response]("rp2")
  }

//  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

//                            landingPage       : LandingPage.Fn.Instance,
//                            register1         : Register.Fn1.Instance,
//                            register2         : Register.Fn2.Instance,
//                            login             : Login.Fn.Instance,
//                            resetPassword1    : ResetPassword.Fn1.Instance,
//                            resetPassword2    : ResetPassword.Fn2.Instance)
//
//  final val EntryPointName = "A"
//  val EntryPoint = ClientSideProc[InitData](EntryPointName)

}
