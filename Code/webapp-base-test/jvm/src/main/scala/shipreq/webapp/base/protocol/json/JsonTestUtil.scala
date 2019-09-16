package shipreq.webapp.base.protocol.json

import io.circe._
import io.circe.syntax._
import nyaya.gen.Gen
import scalaz.Equal
import scalaz.std.either._
import sourcecode.Line
import shipreq.base.test.BaseTestUtil._

object JsonTestUtil {

  private val propTestSize = 33

  implicit def equalDecodingFailure: Equal[DecodingFailure] = Equal.equalA

  def assertDecode[A: Decoder: Equal](json: Json, expect: Decoder.Result[A])(implicit l: Line): Unit =
    assertEq(json.noSpacesSortKeys.take(180), json.as[A], expect)

  def assertDecodeOk[A: Decoder: Equal](json: Json, expect: A)(implicit l: Line): Unit =
    assertDecode(json, Right(expect))

  def assertRoundTrip[A: Decoder: Encoder: Equal](a: A)(implicit l: Line): Unit =
    assertDecodeOk(a.asJson, a)

  def assertRoundTrips[A: Decoder: Encoder: Equal](as: Traversable[A])(implicit l: Line): Unit = {
    var i = 0
    for (a <- as) {
      i += 1
      val json = a.asJson
      assertEq(s"[$i/${as.size}]", json.as[A], Right(a))
    }
  }

  def propTestRoundTrip[A: Decoder: Encoder: Equal](g: Gen[A])(implicit l: Line): Unit =
    g.samples().take(propTestSize).foreach(assertRoundTrip(_))

}
