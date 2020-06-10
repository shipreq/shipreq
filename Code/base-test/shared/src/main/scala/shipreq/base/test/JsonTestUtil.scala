package shipreq.base.test

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import nyaya.gen.Gen
import scalaz.Equal
import scalaz.std.either._
import shipreq.base.util.JsonUtil
import sourcecode.Line
import BaseTestUtil._

object JsonTestUtil {

  private val propTestSize = 33

  implicit def equalDecodingFailure: Equal[DecodingFailure] = Equal.equalA

  implicit def equalCirceError: Equal[io.circe.Error] = Equal.equalA

  implicit final class JsonUtilExtString(private val self: String) extends AnyVal {
    def toJsonOrThrow: Json =
      parse(self) match {
        case Right(j) => j
        case Left(e)  => throw new RuntimeException(JsonUtil.errorMsg(e))
      }
  }

  def decodeOrThrow[A: Decoder](s: String): A =
    decode[A](s) match {
      case Right(a) => a
      case Left(e)  => throw new RuntimeException(JsonUtil.errorMsg(e))
    }

  def assertDecode[A: Decoder: Equal](json: Json, expect: Decoder.Result[A])(implicit l: Line): Unit =
    assertEq(json.noSpacesSortKeys.take(180), json.as[A], expect)

  def assertDecodeOk[A: Decoder: Equal](json: String, expect: A)(implicit l: Line): Unit =
    assertDecodeOk(json.toJsonOrThrow, expect)

  def assertDecodeOk[A: Decoder: Equal](json: Json, expect: A)(implicit l: Line): Unit =
    assertDecode(json, Right(expect))

  def assertAllDecodeOk[A: Decoder: Equal](json: Seq[String], expect: Seq[A])(implicit l: Line): Unit =
    assertSeq(json.map(decode[A](_)), expect.map(Right(_)))

  def assertRoundTrip[A: Decoder: Encoder: Equal](a: A)(implicit l: Line): Unit =
    assertDecodeOk(a.asJson, a)

  def assertRoundTrips[A: Decoder: Encoder: Equal](as: Iterable[A])(implicit l: Line): Unit = {
    var i = 0
    for (a <- as) {
      i += 1
      val json = a.asJson
      assertEq(s"[$i/${as.size}]", json.as[A], Right(a))
    }
  }

  def propTestRoundTrip[A: Decoder: Encoder: Equal](g: Gen[A])(implicit l: Line): Unit =
    g.samples().take(propTestSize).foreach(assertRoundTrip(_))

  def decoderTester[A: Equal](d: Decoder[A]): JsonDecoderTest[A] =
    new JsonDecoderTest()(d, implicitly)
}

// =====================================================================================================================

final class JsonDecoderTest[A: Decoder: Equal] {

  def decodeOrThrow(s: String): A =
    JsonTestUtil.decodeOrThrow(s)

  def assertDecode(json: Json, expect: Decoder.Result[A])(implicit l: Line): Unit =
    JsonTestUtil.assertDecode(json, expect)

  def assertDecodeOk(json: String, expect: A)(implicit l: Line): Unit =
    JsonTestUtil.assertDecodeOk(json, expect)

  def assertDecodeOk(json: Json, expect: A)(implicit l: Line): Unit =
    JsonTestUtil.assertDecodeOk(json, expect)
}