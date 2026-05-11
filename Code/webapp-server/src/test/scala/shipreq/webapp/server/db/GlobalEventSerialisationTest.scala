package shipreq.webapp.server.db

import io.circe._
import nyaya.gen._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.JsonTestUtil
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import sourcecode.Line
import utest._

object GlobalEventSerialisationTest extends TestSuite {
  import GlobalEvent._
  import GlobalEventSerialisation.{emptyJson => ∅, _}
  import GlobalEventTypes._

  protected implicit def univEqJson     : UnivEq[Json     ] = UnivEq.force
  protected implicit def univEqRowData  : UnivEq[RowData  ] = UnivEq.derive
  protected implicit def univEqRow      : UnivEq[Row      ] = UnivEq.derive
  protected implicit def univEqReadError: UnivEq[ReadError] = UnivEq.force

  private implicit val decoderUserId: Decoder[UserId] =
    Decoder[Long].map(UserId.apply)

  private implicit val encoderUserId: Encoder[UserId] =
    Encoder[Long].contramap(_.value)

  private implicit val decoderIP: Decoder[IP] =
    Decoder[String].map(IP.apply)

  private implicit val encoderIP: Encoder[IP] =
    Encoder[String].contramap(_.value)

  private implicit val decoderRowData: Decoder[RowData] =
    Decoder.forProduct3("data", "ip", "userId")(RowData.apply)

  private implicit val encoderRowData: Encoder[RowData] =
    Encoder.forProduct3("data", "ip", "userId")(a => (a.data, a.ip, a.userId))

  private implicit val decoderRow: Decoder[Row] =
    Decoder.forProduct2("type", "data")(Row.apply)

  private implicit val encoderRow: Encoder[Row] =
    Encoder.forProduct2("type", "data")(a => (a.`type`, a.data))

  private implicit val decoderGlobalEvent: Decoder[GlobalEvent] =
    decoderRow.map(GlobalEventSerialisation.decode(_).getOrThrow())

  private implicit val encoderGlobalEvent: Encoder[GlobalEvent] =
    encoderRow.contramap(GlobalEventSerialisation.encode)

  private def assertCodec(event: GlobalEvent, row: Row)(implicit l: Line): Unit = {
    assertEq(GlobalEventSerialisation.encode(event), row)
    assertEq(GlobalEventSerialisation.decode(row), \/-(event))
  }

  protected def propTestRoundTrip(gen: Gen[GlobalEvent])(implicit l: Line): Unit =
    JsonTestUtil.propTestRoundTrip(gen)

//  private implicit def autoSome[A](a: A): Option[A] =
//    Some(a)

  private implicit def conciseRow(a: (Int, RowData)): Row =
    Row(a._1.toShort, a._2)

  private val optionalIps: List[Option[IP]] =
    None :: Some(IP("1.2.3.4")) :: Nil

  private val userIds: List[UserId] =
    UserId(567) :: Nil

  private val optionalUserIds: List[Option[UserId]] =
    None :: userIds.map(Option(_))

  private def assertCodec_IU(event: (Option[IP], Option[UserId]) => GlobalEvent,
                             row  : (Option[IP], Option[UserId]) => Row)(implicit l: Line): Unit =
    for {
      i <- optionalIps
      u <- optionalUserIds
    } assertCodec(event(i, u), row(i, u))

  private def assertCodec_Iu(event: (Option[IP], UserId) => GlobalEvent,
                             row  : (Option[IP], Option[UserId]) => Row)(implicit l: Line): Unit =
    for {
      i <- optionalIps
      u <- userIds
    } assertCodec(event(i, u), row(i, Some(u)))

  override def tests = Tests {
    "UserRegister1"     - assertCodec_Iu(UserRegister1    , TypeUserRegister1     -> RowData(∅, _, _))
    "UserRegister2"     - assertCodec_Iu(UserRegister2    , TypeUserRegister2     -> RowData(∅, _, _))
    "UserPasswordReset" - assertCodec_Iu(UserPasswordReset, TypeUserPasswordReset -> RowData(∅, _, _))

    "UserPasswordResetRequest" - assertCodec_IU(
      UserPasswordResetRequest(_, "x", _),
      TypeUserPasswordResetRequest -> RowData(Json.obj("query" -> Json.fromString("x")), _, _))
  }
}
