package shipreq.webapp.member.protocol.ajax

import boopickle.DefaultBasic._
import shipreq.base.util.SetDiff
import shipreq.webapp.base.config.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.Protocol
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import shipreq.webapp.base.protocol.binary._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.social._

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
      import shipreq.webapp.member.project.protocol.binary.v1.BaseMemberData2._

      val picklerRequest: Pickler[Request] = implicitly
      val picklerResponse: Pickler[Response] = implicitly

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x42A63E36, 0x0C1B2566)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0xB27B40C3, 0x004A70E7)

      defAjax[Request, Response]("cp")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object CreateUserGroup {
    final case class Request(name  : UserGroup.Name,
                             handle: UserGroup.Handle,
                             rels  : UserGroup.ARels[Set, UserGroup.Id.Public, Username],
                            ) {

      def usernames: Set[Username] =
        rels.users.map(_.to)
    }

    sealed trait Response
    object Response {
      final case class Success         (id: UserGroup.Id.Public)                         extends Response
      final case class SaveError       (value: UserGroup.SaveError[UserGroup.Id.Public]) extends Response
      final case class InvalidUsernames(value: NonEmptySet[Username])                    extends Response
    }

    val ajax = {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.member.project.protocol.binary.v1.Social._

      val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.name)
            state.pickle(a.handle)
            state.pickle(a.rels)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val name   = state.unpickle[UserGroup.Name]
            val handle = state.unpickle[UserGroup.Handle]
            val rels   = state.unpickle[UserGroup.ARels[Set, UserGroup.Id.Public, Username]]
            Request(name, handle, rels)
          }
        }

      val picklerResponse: Pickler[Response] = {
        implicit val picklerResponseSuccess: Pickler[Response.Success] =
          new Pickler[Response.Success] {
            override def pickle(a: Response.Success)(implicit state: PickleState): Unit = {
              state.pickle(a.id)
            }
            override def unpickle(implicit state: UnpickleState): Response.Success = {
              val id = state.unpickle[UserGroup.Id.Public]
              Response.Success(id)
            }
          }

        implicit val picklerResponseSaveError: Pickler[Response.SaveError] =
          new Pickler[Response.SaveError] {
            override def pickle(a: Response.SaveError)(implicit state: PickleState): Unit = {
              state.pickle(a.value)
            }
            override def unpickle(implicit state: UnpickleState): Response.SaveError = {
              val value = state.unpickle[UserGroup.SaveError[UserGroup.Id.Public]]
              Response.SaveError(value)
            }
          }

        implicit val picklerResponseInvalidUsernames: Pickler[Response.InvalidUsernames] =
          new Pickler[Response.InvalidUsernames] {
            override def pickle(a: Response.InvalidUsernames)(implicit state: PickleState): Unit = {
              state.pickle(a.value)
            }
            override def unpickle(implicit state: UnpickleState): Response.InvalidUsernames = {
              val value = state.unpickle[NonEmptySet[Username]]
              Response.InvalidUsernames(value)
            }
          }

        new Pickler[Response] {
          private[this] final val KeyInvalidUsernames = 0
          private[this] final val KeySaveError        = 1
          private[this] final val KeySuccess          = 2
          override def pickle(a: Response)(implicit state: PickleState): Unit =
            a match {
              case b: Response.InvalidUsernames => state.enc.writeByte(KeyInvalidUsernames); state.pickle(b)
              case b: Response.SaveError        => state.enc.writeByte(KeySaveError       ); state.pickle(b)
              case b: Response.Success          => state.enc.writeByte(KeySuccess         ); state.pickle(b)
            }
          override def unpickle(implicit state: UnpickleState): Response =
            state.dec.readByte match {
              case KeyInvalidUsernames => state.unpickle[Response.InvalidUsernames]
              case KeySaveError        => state.unpickle[Response.SaveError]
              case KeySuccess          => state.unpickle[Response.Success]
            }
        }
      }

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0xB9234A38, 0x1DAFABDE)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0xA4DC1C0F, 0x106FFE4C)

      defAjax[Request, Response]("cug")
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object UpdateUserGroup {
    final case class Request(id        : UserGroup.Id.Public,
                             name      : Option[UserGroup.Name],
                             handle    : Option[UserGroup.Handle],
                             rels      : UserGroup.ARels[SetDiff, UserGroup.Id.Public, Username],
                            ) {

      def usernames: Set[Username] =
        rels.users.allValues.map(_.to)
    }

    sealed trait Response
    object Response {
      case object      Success                                                           extends Response
      final case class SaveError       (value: UserGroup.SaveError[UserGroup.Id.Public]) extends Response
      final case class InvalidUsernames(value: NonEmptySet[Username])                    extends Response
    }

    val ajax = {
      import shipreq.webapp.base.protocol.binary.v1.BaseData._
      import shipreq.webapp.member.project.protocol.binary.v1.Social._

      val picklerRequest: Pickler[Request] =
        new Pickler[Request] {
          override def pickle(a: Request)(implicit state: PickleState): Unit = {
            state.pickle(a.id)
            state.pickle(a.name)
            state.pickle(a.handle)
          }
          override def unpickle(implicit state: UnpickleState): Request = {
            val id     = state.unpickle[UserGroup.Id.Public]
            val name   = state.unpickle[Option[UserGroup.Name]]
            val handle = state.unpickle[Option[UserGroup.Handle]]
            val rels   = state.unpickle[UserGroup.ARels[SetDiff, UserGroup.Id.Public, Username]]
            Request(id, name, handle, rels)
          }
        }

      val picklerResponse: Pickler[Response] = {
        implicit val picklerResponseSaveError: Pickler[Response.SaveError] =
          new Pickler[Response.SaveError] {
            override def pickle(a: Response.SaveError)(implicit state: PickleState): Unit = {
              state.pickle(a.value)
            }
            override def unpickle(implicit state: UnpickleState): Response.SaveError = {
              val value = state.unpickle[UserGroup.SaveError[UserGroup.Id.Public]]
              Response.SaveError(value)
            }
          }

        implicit val picklerResponseInvalidUsernames: Pickler[Response.InvalidUsernames] =
          new Pickler[Response.InvalidUsernames] {
            override def pickle(a: Response.InvalidUsernames)(implicit state: PickleState): Unit = {
              state.pickle(a.value)
            }
            override def unpickle(implicit state: UnpickleState): Response.InvalidUsernames = {
              val value = state.unpickle[NonEmptySet[Username]]
              Response.InvalidUsernames(value)
            }
          }

        new Pickler[Response] {
          private[this] final val KeyInvalidUsernames = 0
          private[this] final val KeySaveError        = 1
          private[this] final val KeySuccess          = 2
          override def pickle(a: Response)(implicit state: PickleState): Unit =
            a match {
              case b: Response.InvalidUsernames => state.enc.writeByte(KeyInvalidUsernames); state.pickle(b)
              case b: Response.SaveError        => state.enc.writeByte(KeySaveError       ); state.pickle(b)
              case Response.Success             => state.enc.writeByte(KeySuccess         )
            }
          override def unpickle(implicit state: UnpickleState): Response =
            state.dec.readByte match {
              case KeyInvalidUsernames => state.unpickle[Response.InvalidUsernames]
              case KeySaveError        => state.unpickle[Response.SaveError]
              case KeySuccess          => Response.Success
            }
        }
      }

      implicit val safePicklerRequest: SafePickler[Request] =
        picklerRequest.asV1(0).withMagicNumbers(0x7D0866A2, 0x9701DB5C)

      implicit val safePicklerResponse: SafePickler[Response] =
        picklerResponse.asV1(0).withMagicNumbers(0x90C373FC, 0x28B05C67)

      defAjax[Request, Response]("uug")
    }
  }

}