package shipreq.webapp.client.public

import boopickle.Pickler
import japgolly.univeq.UnivEq
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.CommmonUiText
import shipreq.webapp.base.data.SecurityToken
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.user._
import shipreq.webapp.base.validation._
import shipreq.webapp.base.validation.Implicits._
import BoopickleMacros._
import BinCodecGeneric._
import BinCodecBaseData._
import BinCodecUser._

/**
  * Protocols for the Public SPA / webapp-client-public module.
  */
object PublicSpaProtocols {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object LandingPage {
    final case class Request(name      : PersonName,
                             email     : EmailAddr,
                             msg       : Option[String],
                             newsletter: Boolean) {

      def validate: Composite.Invalidity \/ Request =
        Request.validator((name.value, email.value, msg getOrElse "", newsletter))
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

      def labelName  = "Your name"
      def labelEmail = "Your email address"

      def validatorName  = UserValidators.personName
      def validatorEmail = UserValidators.emailAddr.unnamed
      def validatorMsg   = CommonValidation.optionalLargeText

      lazy val validator: Composite.Validator[(String, String, String, Boolean), _, Request] =
        validatorName.named(labelName).named
          .tuple(validatorEmail.named(labelEmail).named)
          .tuple(validatorMsg.named("Your message").named)
          .tuple(Composite.Validator.id[Boolean])
          .mapValid((Request.apply _).tupled)
    }

    val Fn = ServerSideProc.Protocol[Request, Unit]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Register {
    val Fn1 = ServerSideProc.Protocol[EmailAddr, Unit]

    val Fn2A = ServerSideProc.Protocol[SecurityToken, SecurityToken.Status]

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

      lazy val validator: Composite.Validator[(SecurityToken, String, String, String, Boolean), _, Request] =
        Composite.Validator.id[SecurityToken]
          .tuple(UserValidators.personName.named(CommmonUiText.userPersonName).named)
          .tuple(UserValidators.username.named)
          .tuple(UserValidators.password.named)
          .tuple(Composite.Validator.id[Boolean])
          .mapValid((Request.apply _).tupled)
    }

    sealed trait Response
    object Response {
      case object Success       extends Response
      case object TokenInvalid  extends Response
      case object TokenExpired  extends Response
      case object UsernameTaken extends Response

      implicit val pickler: Pickler[Response] = derivePickler[Response]
      implicit def univEq: UnivEq[Response] = UnivEq.derive
    }

    /** Upon successful submission the user account is activated. */
    val Fn2B = ServerSideProc.Protocol[Request, Response]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Login {
    final case class Request(user: Username \/ EmailAddr, password: PlainTextPassword) {
      def validate: Composite.Invalidity \/ Request =
        Request.validator((user.fold(_.value, _.value), password.value))
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

      lazy val validator: Composite.Validator[(String, String), _, Request] =
        UserValidators.usernameOrEmail
          .tuple(UserValidators.password.named)
          .mapValid((Request.apply _).tupled)
    }
    val Fn = ServerSideProc.Protocol[Request, Permission]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword {
    val Fn1 = ServerSideProc.Protocol[Username \/ EmailAddr, Unit]

    val Fn2A = ServerSideProc.Protocol[SecurityToken, SecurityToken.Status]

    final case class Request(token: SecurityToken, newPassword: PlainTextPassword)
    implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

    sealed trait Response
    object Response {
      case object Success       extends Response
      case object TokenInvalid  extends Response
      case object TokenExpired  extends Response

      implicit val pickler: Pickler[Response] = derivePickler[Response]
      implicit def univEq: UnivEq[Response] = UnivEq.derive
    }

    val Fn2B = ServerSideProc.Protocol[Request, Response]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitData(landingPage    : LandingPage.Fn.Instance,
                            allowRegister  : Permission,
                            register1      : Register.Fn1.Instance,
                            register2A     : Register.Fn2A.Instance,
                            register2B     : Register.Fn2B.Instance,
                            login          : Login.Fn.Instance,
                            resetPassword1 : ResetPassword.Fn1.Instance,
                            resetPassword2A: ResetPassword.Fn2A.Instance,
                            resetPassword2B: ResetPassword.Fn2B.Instance)

  import LandingPage  .Fn  .{pickleInstance => _i1}
  import Register     .Fn1 .{pickleInstance => _i2}
  import Register     .Fn2A.{pickleInstance => _i3}
  import Register     .Fn2B.{pickleInstance => _i4}
  import Login        .Fn  .{pickleInstance => _i5}
  import ResetPassword.Fn1 .{pickleInstance => _i6}
  import ResetPassword.Fn2A.{pickleInstance => _i7}
  import ResetPassword.Fn2B.{pickleInstance => _i8}
  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "A"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}
