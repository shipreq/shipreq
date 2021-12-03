package shipreq.webapp.server.db

import io.circe._
import io.circe.syntax._
import shipreq.base.util.JsonUtil._
import shipreq.base.util.SetDiff
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent._
import shipreq.webapp.member.global._
import shipreq.webapp.member.social._

object GlobalEventSerialisation {
  import GlobalEventTypes._

  final case class Row(`type`: Short, data: RowData)

  final case class RowData(data: Json, ip: Option[IP], userId: Option[UserId]) {
    def needUserId: ReadResult[UserId] =
      userId match {
        case Some(u) => \/-(u)
        case None    => -\/(ReadError.UserIdRequired)
      }
  }

  object RowData {
    val noJson = apply(emptyJson, _, _)
  }

  sealed trait ReadError

  object ReadError {
    case object UserIdRequired extends ReadError
    final case class FailedToParseJson(failure: DecodingFailure) extends ReadError
  }

  type ReadResult[+A] = ReadError \/ A

  // ===================================================================================================================

  private[db] val emptyJson: Json =
    Json.Null

  private final class Codec[A](val write: A => RowData, val read: RowData => ReadResult[A])

  private object Codec {

    def apply[A](read: RowData => ReadResult[A])(write: A => RowData): Codec[A] =
      new Codec(write, read)

    def total[A](read: RowData => A)(write: (A, Json) => RowData): Codec[A] =
      apply(read.andThen(\/-(_)))(write(_, emptyJson))

      def withJson[A, J](jsonSubset: A => J)
                        (read      : (RowData, J) => ReadResult[A],
                         write     : (A, Json) => RowData)
                        (implicit d: Decoder[J], e: Encoder[J]): Codec[A] =
        Codec(r => d.decodeJson(r.data) match {
          case Right(j) => read(r, j)
          case Left(e)  => -\/(ReadError.FailedToParseJson(e))
        })(a => write(a, e(jsonSubset(a))))
  }

  private object Codecs {

    implicit val codecUserRegister1: Codec[UserRegister1] =
      Codec(r =>
        for {
          userId <- r.needUserId
        } yield UserRegister1(r.ip, userId)
      )(e => RowData.noJson(e.ip, Some(e.userId)))

    implicit val codecUserRegister2: Codec[UserRegister2] =
      Codec(r =>
        for {
          userId <- r.needUserId
        } yield UserRegister2(r.ip, userId)
      )(e => RowData.noJson(e.ip, Some(e.userId)))

    implicit val codecUserPasswordResetRequest: Codec[UserPasswordResetRequest] = {
      type J = String
      val encoder: Encoder[J] = Encoder.forProduct1("query")(identity)
      val decoder: Decoder[J] = Decoder.forProduct1("query")(identity[J])
      Codec.withJson[UserPasswordResetRequest, J](_.query)(
        (r, j) => \/-(UserPasswordResetRequest(r.ip, j, r.userId)),
        (e, j) => RowData(j, e.ip, e.userId)
      )(decoder, encoder)
    }

    implicit val codecUserPasswordReset: Codec[UserPasswordReset] =
      Codec(r =>
        for {
          userId <- r.needUserId
        } yield UserPasswordReset(r.ip, userId)
      )(e => RowData.noJson(e.ip, Some(e.userId)))

    private object UserGroupCodecs {

      implicit val decoderUserId: Decoder[UserId] =
        Decoder[Long].map(UserId.apply)

      implicit val encoderUserId: Encoder[UserId] =
        Encoder[Long].contramap(_.value)

      implicit val decoderUserGroupId: Decoder[UserGroup.Id] =
        Decoder[Long].map(UserGroup.Id.apply)

      implicit val encoderUserGroupId: Encoder[UserGroup.Id] =
        Encoder[Long].contramap(_.value)

      implicit val decoderUserGroupName: Decoder[UserGroup.Name] =
        Decoder[String].map(UserGroup.Name.apply)

      implicit val encoderUserGroupName: Encoder[UserGroup.Name] =
        Encoder[String].contramap(_.value)

      implicit val decoderUserGroupHandle: Decoder[UserGroup.Handle] =
        Decoder[String].map(UserGroup.Handle.apply)

      implicit val encoderUserGroupHandle: Encoder[UserGroup.Handle] =
        Encoder[String].contramap(_.value)

      implicit val decoderUserGroupPerm: Decoder[UserGroup.Perm] = decodeSumBySoleKey {
        case ("admin" , _) => Right(UserGroup.Perm.Admin)
        case ("member", _) => Right(UserGroup.Perm.Member)
      }

      implicit val encoderUserGroupPerm: Encoder[UserGroup.Perm] = Encoder.instance {
        case UserGroup.Perm.Admin  => Json.obj("admin"  -> ().asJson)
        case UserGroup.Perm.Member => Json.obj("member" -> ().asJson)
      }

      private def decoderARel[A: Decoder]: Decoder[UserGroup.ARel[A]] =
        Decoder.forProduct2("to", "perm")(UserGroup.ARel.apply[A])

      private def encoderARel[A: Encoder]: Encoder[UserGroup.ARel[A]] =
        Encoder.forProduct2("to", "perm")(a => (a.to, a.perm))

      implicit val decoderARelUserId: Decoder[UserGroup.ARel[UserId]] = decoderARel
      implicit val encoderARelUserId: Encoder[UserGroup.ARel[UserId]] = encoderARel

      implicit val decoderARelUserGroupId: Decoder[UserGroup.ARel[UserGroup.Id]] = decoderARel
      implicit val encoderARelUserGroupId: Encoder[UserGroup.ARel[UserGroup.Id]] = encoderARel

      implicit def encoderSetDiff[A: Encoder]: Encoder[SetDiff[A]] =
        Encoder.forProduct2("-", "+")(a => (a.removed, a.added))

      implicit def decoderSetDiff[A: Decoder : UnivEq]: Decoder[SetDiff[A]] =
        Decoder.forProduct2("-", "+")(SetDiff.apply[A])
    }

    implicit val codecUserGroupCreate: Codec[UserGroupCreate] = {
      import UserGroupCodecs._
      final case class Data(id      : UserGroup.Id,
                            name    : UserGroup.Name,
                            handle  : UserGroup.Handle,
                            parents : Set[UserGroup.ARel[UserGroup.Id]],
                            children: Set[UserGroup.ARel[UserGroup.Id]],
                            users   : Set[UserGroup.ARel[UserId]]) {
        def toEvent(userId: UserId) =
          UserGroupCreate(
            userId = userId,
            id     = id,
            name   = name,
            handle = handle,
            rels   = UserGroup.ARels(
                       parents  = parents,
                       children = children,
                       users    = users,
                     )
          )
      }
      implicit val decoder: Decoder[Data] =
        Decoder.instance { c =>
          for {
            id       <- c.get[UserGroup.Id]("id")
            name     <- c.get[UserGroup.Name]("name")
            handle   <- c.get[UserGroup.Handle]("handle")
            parents  <- c.get[Set[UserGroup.ARel[UserGroup.Id]]]("parents")
            children <- c.get[Set[UserGroup.ARel[UserGroup.Id]]]("children")
            users    <- c.get[Set[UserGroup.ARel[UserId]]]("users")
          } yield Data(id, name, handle, parents, children, users)
        }
      implicit val encoder: Encoder[Data] =
        Encoder.instance(value => Json.obj(
          "id"       -> value.id.asJson,
          "name"     -> value.name.asJson,
          "handle"   -> value.handle.asJson,
          "parents"  -> value.parents.asJson,
          "children" -> value.children.asJson,
          "users"    -> value.users.asJson,
        ))
      Codec.withJson[UserGroupCreate, Data](e =>
        Data(
          id       = e.id,
          name     = e.name,
          handle   = e.handle,
          parents  = e.rels.parents,
          children = e.rels.children,
          users    = e.rels.users,
        )
      )(
        (r, d) => \/-(d.toEvent(r.userId.get)),
        (e, d) => RowData(d, None, Some(e.userId))
      )
    }

    implicit val codecUserGroupUpdate: Codec[UserGroupUpdate] = {
      import UserGroupCodecs._
      final case class Data(id      : UserGroup.Id,
                            name    : Option[UserGroup.Name],
                            handle  : Option[UserGroup.Handle],
                            parents : SetDiff[UserGroup.ARel[UserGroup.Id]],
                            children: SetDiff[UserGroup.ARel[UserGroup.Id]],
                            users   : SetDiff[UserGroup.ARel[UserId]]) {
        def toEvent(userId: UserId) =
          UserGroupUpdate(
            userId = userId,
            id     = id,
            name   = name,
            handle = handle,
            rels   = UserGroup.ARels(
                       parents  = parents,
                       children = children,
                       users    = users,
                     )
          )
      }
      implicit val decoder: Decoder[Data] =
        Decoder.instance { c =>
          for {
            id        <- c.get[UserGroup.Id]("id")
            newName   <- c.get[Option[UserGroup.Name]]("name")
            newHandle <- c.get[Option[UserGroup.Handle]]("handle")
            parents   <- c.get[SetDiff[UserGroup.ARel[UserGroup.Id]]]("parents")
            children  <- c.get[SetDiff[UserGroup.ARel[UserGroup.Id]]]("children")
            users     <- c.get[SetDiff[UserGroup.ARel[UserId]]]("users")
          } yield Data(id, newName, newHandle, parents, children, users)
        }
      implicit val encoder: Encoder[Data] =
        Encoder.instance(value => Json.obj(
          "id"       -> value.id.asJson,
          "name"     -> value.name.asJson,
          "handle"   -> value.handle.asJson,
          "parents"  -> value.parents.asJson,
          "children" -> value.children.asJson,
          "users"    -> value.users.asJson,
        ))
      Codec.withJson[UserGroupUpdate, Data](e =>
        Data(
          id       = e.id,
          name     = e.name,
          handle   = e.handle,
          parents  = e.rels.parents,
          children = e.rels.children,
          users    = e.rels.users,
        )
      )(
        (r, d) => \/-(d.toEvent(r.userId.get)),
        (e, d) => RowData(d, None, Some(e.userId))
      )
    }

  } // Codecs

  // ===================================================================================================================

  val encode: GlobalEvent => Row =
    event => {
      import Codecs._
      event match {
        case e: UserRegister1            => Row(TypeUserRegister1           , codecUserRegister1           .write(e))
        case e: UserRegister2            => Row(TypeUserRegister2           , codecUserRegister2           .write(e))
        case e: UserPasswordResetRequest => Row(TypeUserPasswordResetRequest, codecUserPasswordResetRequest.write(e))
        case e: UserPasswordReset        => Row(TypeUserPasswordReset       , codecUserPasswordReset       .write(e))
        case e: UserGroupCreate          => Row(TypeUserGroupCreate         , codecUserGroupCreate         .write(e))
        case e: UserGroupUpdate          => Row(TypeUserGroupUpdate         , codecUserGroupUpdate         .write(e))
      }
    }

  val decode: Row => ReadResult[GlobalEvent] =
    row => {
      import Codecs._
      val codec: Codec[_ <: GlobalEvent] =
        row.`type` match {
          case TypeUserRegister1            => codecUserRegister1
          case TypeUserRegister2            => codecUserRegister2
          case TypeUserPasswordResetRequest => codecUserPasswordResetRequest
          case TypeUserPasswordReset        => codecUserPasswordReset
          case TypeUserGroupCreate          => codecUserGroupCreate
          case TypeUserGroupUpdate          => codecUserGroupUpdate
        }
      codec.read(row.data)
    }
}
