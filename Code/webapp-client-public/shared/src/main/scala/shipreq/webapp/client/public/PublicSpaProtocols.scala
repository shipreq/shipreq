package shipreq.webapp.client.public

import boopickle.Pickler
import japgolly.univeq.UnivEq
import monocle.macros.{GenIso, Lenses}
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.BoopickleMacros._
import shipreq.webapp.base.protocol.BinCodecGeneric._
import shipreq.webapp.base.protocol.BinCodecBaseData._
import shipreq.webapp.base.protocol.BinCodecUser._
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.user._
import shipreq.webapp.base.validation._
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.Urls

/**
  * Protocols for the Public SPA / webapp-client-public module.
  */
object PublicSpaProtocols {

  private def ajax[Req: Pickler, Res: Pickler](path: String): Protocol.Ajax.Simple[Pickler, Req, Res] =
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
  }

  val landingPage = ajax[LandingPage.Request, ErrorMsg \/ Unit]("lp")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Register {
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
  }

  val register1 = ajax[EmailAddr, ErrorMsg \/ Unit]("rg1")
  val register2 = ajax[Register.Request, ErrorMsg \/ Register.Response]("rg2")

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
  }

  val login = ajax[Login.Request, Permission]("l")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword {
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
  }

  val resetPassword1 = ajax[Username \/ EmailAddr, Unit]("rp1")
  val resetPassword2 = ajax[ResetPassword.Request, ErrorMsg \/ ResetPassword.Response]("rp2")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitData(publicRegistration: Permission,
                            loggedInUser      : Option[Username])
  object InitData {
    implicit val pickler = pickleCaseClass[InitData]
  }

  final val EntryPointName = "A"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}
