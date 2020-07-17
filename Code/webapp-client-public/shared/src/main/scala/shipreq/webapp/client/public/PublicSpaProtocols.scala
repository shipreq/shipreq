package shipreq.webapp.client.public

import boopickle.DefaultBasic._
import monocle.macros.{GenIso, Lenses}
import shipreq.base.util._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.VerificationToken
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.protocol.binary._
import shipreq.webapp.base.user._
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.validation._

/**
  * Protocols for the Public SPA / webapp-client-public module.
  */
object PublicSpaProtocols {

  type Ajax[Req, Res] = Protocol.Ajax.Simple[SafePickler, Req, Res]

  private def defAjax[Req: SafePickler, Res: SafePickler](path: String): Ajax[Req, Res] =
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
      def labelName  = "Your name"
      def labelEmail = "Your email address"

      def validatorName  = UserValidators.personName.unnamed
      def validatorEmail = UserValidators.emailAddr.unnamed
      def validatorMsg   = CommonValidation.optionalLargeText.mapCorrector(_ prependLive TextMod.maxTwoConsecutiveNewLines.run)

      implicit def univEq: UnivEq[Request] = UnivEq.derive

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

    type Response = ErrorMsg \/ Unit

    val ajax = {
      import v1.BaseData._

      implicit val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.name)
            state.pickle(a.email)
            state.pickle(a.msg)
            state.pickle(a.newsletter)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val name       = state.unpickle[PersonName]
            val email      = state.unpickle[EmailAddr]
            val msg        = state.unpickle[Option[String]]
            val newsletter = state.unpickle[Boolean]
            Request(name, email, msg, newsletter)
          }
        }

      val picklerResponse: Pickler[Response] =
        implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0xB5AE4CF5, 0x228FA2F2)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0x7CD703D9, 0xB2C6D5E3)

      defAjax[Request, Response]("lp")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Register1 {
    type Request  = EmailAddr
    type Response = ErrorMsg \/ Unit

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      val picklerRequest: Pickler[Request] = implicitly
      val picklerResponse: Pickler[Response] = implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x89827590, 0x8858F858)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0x0FCE3232, 0x713A4224)

      defAjax("reg1")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Register2 {
    final case class Request(token     : VerificationToken,
                             personName: PersonName,
                             username  : Username,
                             password  : PlainTextPassword,
                             newsletter: Boolean) {

      def validate: Composite.Invalidity \/ Request =
        Request.validator((token, personName.value, username.value, password.value, newsletter))
    }

    object Request {
      implicit def univEq: UnivEq[Request] = UnivEq.derive

      // Only used on server-side - Request has less than the registration form
      lazy val validator: Composite.Validator[(VerificationToken, String, String, String, Boolean), _, Request] =
        Composite.Validator.id[VerificationToken]
          .tuple(UserValidators.personName.named)
          .tuple(UserValidators.username.stateless.named)
          .tuple(UserValidators.password.named)
          .tuple(Composite.Validator.id[Boolean])
          .mapValid((Request.apply _).tupled)
    }

    sealed trait Result
    object Result {
      sealed trait Terminal     extends Result
      case object Success       extends Terminal
      case object TokenInvalid  extends Terminal
      case object TokenExpired  extends Terminal
      case object UsernameTaken extends Result

      implicit def univEq: UnivEq[Result] = UnivEq.derive
    }

    type Response = ErrorMsg \/ Result

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.token)
            state.pickle(a.personName)
            state.pickle(a.username)
            state.pickle(a.password)
            state.pickle(a.newsletter)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val token      = state.unpickle[VerificationToken]
            val personName = state.unpickle[PersonName]
            val username   = state.unpickle[Username]
            val password   = state.unpickle[PlainTextPassword]
            val newsletter = state.unpickle[Boolean]
            Request(token, personName, username, password, newsletter)
          }
        }

      implicit val picklerResult: Pickler[Result] =
        new Pickler[Result] {
          override def pickle(a: Result)(implicit state: PickleState): Unit =
            a match {
              case Result.Success       => state.enc.writeByte(0)
              case Result.TokenExpired  => state.enc.writeByte(1)
              case Result.TokenInvalid  => state.enc.writeByte(2)
              case Result.UsernameTaken => state.enc.writeByte(3)
            }
          override def unpickle(implicit state: UnpickleState): Result =
            state.dec.readByte match {
              case 0 => Result.Success
              case 1 => Result.TokenExpired
              case 2 => Result.TokenInvalid
              case 3 => Result.UsernameTaken
            }
        }

      val picklerResponse: Pickler[Response] =
        pickleDisj

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x456C4A18, 0x74601B38)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0x9FE45912, 0x6FDDAE09)

      defAjax("reg2")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword1 {
    type Request  = Username \/ EmailAddr
    type Response = Unit

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      val picklerRequest: Pickler[Request] = implicitly
      val picklerResponse: Pickler[Response] = implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x1EFE85AA, 0x43067CC7)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0xFEF9FB89, 0x318614CF)

      defAjax("rp1")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ResetPassword2 {
    final case class Request(token: VerificationToken, newPassword: PlainTextPassword)

    implicit def univEqRequest: UnivEq[Request] = UnivEq.derive

    sealed trait Result
    object Result {
      case object Success      extends Result
      case object TokenInvalid extends Result
      case object TokenExpired extends Result
      implicit def univEq: UnivEq[Result] = UnivEq.derive
    }

    type Response = ErrorMsg \/ Result

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      implicit val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.token)
            state.pickle(a.newPassword)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val token       = state.unpickle[VerificationToken]
            val newPassword = state.unpickle[PlainTextPassword]
            Request(token, newPassword)
          }
        }

      implicit val picklerResult: Pickler[Result] =
        new Pickler[Result] {
          override def pickle(a: Result)(implicit state: PickleState): Unit =
            a match {
              case Result.Success      => state.enc.writeByte(0)
              case Result.TokenExpired => state.enc.writeByte(1)
              case Result.TokenInvalid => state.enc.writeByte(2)
            }
          override def unpickle(implicit state: UnpickleState): Result =
            state.dec.readByte match {
              case 0 => Result.Success
              case 1 => Result.TokenExpired
              case 2 => Result.TokenInvalid
            }
        }

      val picklerResponse: Pickler[Response] =
        pickleDisj

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x024A43EE, 0x63AE6C82)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0xB9CB8212, 0xDA4601AD)

      defAjax("rp2")
    }
  }

}
