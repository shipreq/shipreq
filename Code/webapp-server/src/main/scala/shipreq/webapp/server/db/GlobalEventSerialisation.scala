package shipreq.webapp.server.db

import io.circe._
import shipreq.webapp.base.data.{IP, ProjectId, UserId}
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.global.GlobalEvent._

object GlobalEventSerialisation {
  import GlobalEventTypes._

  final case class Row(`type`: Short, data: RowData)

  final case class RowData(data: Json, ip: Option[IP], userId: Option[UserId], projectId: Option[ProjectId]) {
    def needUserId: ReadResult[UserId] =
      userId match {
        case Some(u) => \/-(u)
        case None    => -\/(ReadError.UserIdRequired)
      }

    def needProjectId: ReadResult[ProjectId] =
      projectId match {
        case Some(p) => \/-(p)
        case None    => -\/(ReadError.ProjectIdRequired)
      }
  }

  object RowData {
    val noJson = apply(emptyJson, _, _, _)
  }

  sealed trait ReadError

  object ReadError {
    case object UserIdRequired extends ReadError
    case object ProjectIdRequired extends ReadError
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
      )(e => RowData.noJson(e.ip, Some(e.userId), None))

    implicit val codecUserRegister2: Codec[UserRegister2] =
      Codec(r =>
        for {
          userId <- r.needUserId
        } yield UserRegister2(r.ip, userId)
      )(e => RowData.noJson(e.ip, Some(e.userId), None))

    implicit val codecUserPasswordResetRequest: Codec[UserPasswordResetRequest] = {
      type J = String
      val encoder: Encoder[J] = Encoder.forProduct1("query")(identity)
      val decoder: Decoder[J] = Decoder.forProduct1("query")(identity[J])
      Codec.withJson[UserPasswordResetRequest, J](_.query)(
        (r, j) => \/-(UserPasswordResetRequest(r.ip, j, r.userId)),
        (e, j) => RowData(j, e.ip, e.userId, None)
      )(decoder, encoder)
    }

    implicit val codecUserPasswordReset: Codec[UserPasswordReset] =
      Codec(r =>
        for {
          userId <- r.needUserId
        } yield UserPasswordReset(r.ip, userId)
      )(e => RowData.noJson(e.ip, Some(e.userId), None))

    implicit val codecProjectCreate: Codec[ProjectCreate] =
      Codec(r =>
        for {
          userId    <- r.needUserId
          projectId <- r.needProjectId
        } yield ProjectCreate(userId, projectId)
      )(e => RowData.noJson(None, Some(e.userId), Some(e.projectId)))
  }

  // ===================================================================================================================

  val encode: GlobalEvent => Row =
    event => {
      import Codecs._
      event match {
        case e: UserRegister1            => Row(TypeUserRegister1           , codecUserRegister1           .write(e))
        case e: UserRegister2            => Row(TypeUserRegister2           , codecUserRegister2           .write(e))
        case e: UserPasswordResetRequest => Row(TypeUserPasswordResetRequest, codecUserPasswordResetRequest.write(e))
        case e: UserPasswordReset        => Row(TypeUserPasswordReset       , codecUserPasswordReset       .write(e))
        case e: ProjectCreate            => Row(TypeProjectCreate           , codecProjectCreate           .write(e))
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
          case TypeProjectCreate            => codecProjectCreate
        }
      codec.read(row.data)
    }
}
