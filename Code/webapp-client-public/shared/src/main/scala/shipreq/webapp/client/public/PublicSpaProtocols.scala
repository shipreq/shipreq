package shipreq.webapp.client.public

import boopickle.Pickler
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data.SecurityToken
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

      def toValidatorInput: Request.ValidatorInput =
        (name.value, email.value, msg getOrElse "", newsletter)
    }

    object Request {
      implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

      def labelName  = "Your name"
      def labelEmail = "Your email address"

      def validatorName  = UserValidators.personName
      def validatorEmail = UserValidators.emailAddr.unnamed
      def validatorMsg   = CommonValidation.optionalLargeText

      type ValidatorInput = (String, String, String, Boolean)

      lazy val validator: Composite.Validator[ValidatorInput, _, Request] =
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

    final case class Request(token     : SecurityToken,
                             personName: PersonName,
                             username  : Username,
                             password  : String,
                             newsletter: Boolean)
    implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

    /** Upon successful submission the user account is activated. */
    val Fn2 = ServerSideProc.Protocol[Request, Unit]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Login {
    final case class Request(who: Username \/ EmailAddr, password: String)
    implicit val pickler: Pickler[Request] = pickleCaseClass[Request]
    val Fn = ServerSideProc.Protocol[Request, Validity]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword {
    val Fn1 = ServerSideProc.Protocol[Username \/ EmailAddr, Unit]

    // Failure responses? Expired etc
    final case class Request(token: SecurityToken, newPassword: String)
    implicit val pickler: Pickler[Request] = pickleCaseClass[Request]

    val Fn2 = ServerSideProc.Protocol[Request, Unit]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class InitData(landingPage   : LandingPage.Fn.Instance,
                            allowRegister : Permission,
                            register1     : Register.Fn1.Instance,
                            register2     : Register.Fn2.Instance,
                            login         : Login.Fn.Instance,
                            resetPassword1: ResetPassword.Fn1.Instance,
                            resetPassword2: ResetPassword.Fn2.Instance)

  import LandingPage  .Fn .{pickleInstance => _i1}
  import Register     .Fn1.{pickleInstance => _i2}
  import Register     .Fn2.{pickleInstance => _i3}
  import Login        .Fn .{pickleInstance => _i4}
  import ResetPassword.Fn1.{pickleInstance => _i5}
  import ResetPassword.Fn2.{pickleInstance => _i6}
  implicit val picklerInitData = pickleCaseClass[InitData]

  final val EntryPointName = "A"
  val EntryPoint = ClientSideProc[InitData](EntryPointName)
}
