package shipreq.webapp.server.db

import io.circe.Json
import shipreq.base.test.BaseTestUtil._
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

  private def assertCodec(event: GlobalEvent, row: Row)(implicit l: Line): Unit = {
    assertEq(GlobalEventSerialisation.encode(event), row)
    assertEq(GlobalEventSerialisation.decode(row), \/-(event))
  }

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
