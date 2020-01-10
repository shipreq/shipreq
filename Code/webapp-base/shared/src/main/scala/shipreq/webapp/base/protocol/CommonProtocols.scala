package shipreq.webapp.base.protocol

import boopickle.DefaultBasic._
import monocle.macros.{GenIso, Lenses}
import scalaz.\/
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.ProjectId
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

      def validate(user: Username \/ EmailAddr, password: PlainTextPassword): Composite.Invalidity \/ Request =
        Untyped(user.fold(_.value, _.value), password.value).validate

      @Lenses
      final case class Untyped(usernameOrEmail: String, password: String) {
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

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object SubmitFeedback {

    final case class UserInput(feedback: String)

    final case class ProjectMetadata(id          : ProjectId.Public,
                                     ord         : Option[Int],
                                     futureEvents: Set[Int])

    // Note: username is here rather than deriving from JWT because a user may want to submit after their session
    // expires on while on a page, in which case the browser deletes the JWT from its cookie jar.
    final case class Metadata(project  : Option[ProjectMetadata],
                              url      : String,
                              userAgent: String,
                              username : Username)

    final case class Request(input: UserInput, metadata: Metadata)

    implicit def univEqUserInput      : UnivEq[UserInput      ] = UnivEq.derive
    implicit def univEqProjectMetadata: UnivEq[ProjectMetadata] = UnivEq.derive
    implicit def univEqMetadata       : UnivEq[Metadata       ] = UnivEq.derive
    implicit def univEqRequest        : UnivEq[Request        ] = UnivEq.derive

    type Response = Unit

    val ajax: Ajax[Request, Response] = {
      import v1.BaseData._

      implicit val picklerUserInput: Pickler[UserInput] =
        new Pickler[UserInput] {
          override def pickle(a: UserInput)(implicit state: PickleState): Unit = {
            state.enc.writeInt(0) // version
            state.pickle(a.feedback)
          }
          override def unpickle(implicit state: UnpickleState): UserInput = {
            val version = state.dec.readInt
            val feedback = state.unpickle[String]
            UserInput(feedback)
          }
        }

      implicit val picklerProjectMetadata: Pickler[ProjectMetadata] =
        new Pickler[ProjectMetadata] {
          override def pickle(a: ProjectMetadata)(implicit state: PickleState): Unit = {
            state.enc.writeInt(0) // version
            state.pickle(a.id)
            state.pickle(a.ord)
            state.pickle(a.futureEvents)
          }
          override def unpickle(implicit state: UnpickleState): ProjectMetadata = {
            val version      = state.dec.readInt
            val id           = state.unpickle[ProjectId.Public]
            val ord          = state.unpickle[Option[Int]]
            val futureEvents = state.unpickle[Set[Int]]
            ProjectMetadata(id, ord, futureEvents)
          }
        }

      implicit val picklerMetadata: Pickler[Metadata] =
        new Pickler[Metadata] {
          override def pickle(a: Metadata)(implicit state: PickleState): Unit = {
            state.enc.writeInt(0) // version
            state.pickle(a.project)
            state.pickle(a.url)
            state.pickle(a.userAgent)
            state.pickle(a.username)
          }
          override def unpickle(implicit state: UnpickleState): Metadata = {
            val version   = state.dec.readInt
            val project   = state.unpickle[Option[ProjectMetadata]]
            val url       = state.unpickle[String]
            val userAgent = state.unpickle[String]
            val username  = state.unpickle[Username]
            Metadata(project, url, userAgent, username)
          }
        }

      implicit val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.input)
            state.pickle(a.metadata)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val input    = state.unpickle[UserInput]
            val metadata = state.unpickle[Metadata]
            Request(input, metadata)
          }
        }

      val picklerResponse: Pickler[Response] =
        implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV10.withMagicNumbers(0xC8EF5F9E, 0x35FCD3C3)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV10.withMagicNumbers(0x69420882, 0x48AFA035)

      defAjax("feedback")
    }
  }

}
