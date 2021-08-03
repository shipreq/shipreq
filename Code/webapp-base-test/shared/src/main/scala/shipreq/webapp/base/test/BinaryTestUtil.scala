package shipreq.webapp.base.test

import boopickle.Pickler
import cats.Eq
import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.ConstructionHelperImplicits._
import sourcecode.Line

object BinaryTestUtil {

  private val propTestSize = 33

  // ===================================================================================================================
  // SafePickler testing

  // Not true but good enough for now
  implicit def equalSafePicklerDecoderFailure: Eq[SafePickler.DecodingFailure] =
    Eq.fromUniversalEquals

  def assertEqBinary(actual: BinaryData, expect: BinaryData)(implicit l: Line): Unit =
    binaryDiff(actual, expect).foreach(fail(_))

  def binaryDiff(actual    : BinaryData,
                 expect    : BinaryData,
                 descActual: String = "Actual",
                 descExpect: String = "Expect"): Option[String] =
    Option.when(actual !=* expect) {
      val limit = 100

      var failures = List.empty[String]

      if (actual.length != expect.length)
        failures ::= s"Actual length (${actual.length}) != expect ${expect.length}"

      var b1 = actual
      var b2 = expect

      var pre = ""
      var post = ""

      def tooBig() = b1.length > limit || b2.length > limit

      if (tooBig()) {
        while (tooBig() && b1.unsafeArray(0) == b2.unsafeArray(0)) {
          b1 = b1.drop(1)
          b2 = b2.drop(1)
          pre = "…"
        }

        if (tooBig()) {
          b1 = b1.take(limit)
          b2 = b2.take(limit)
          post = "…"
        }
      }

      val (s1, s2) = {
        var r1 = Console.BLACK_B
        var r2 = Console.BLACK_B
        var h1 = b1.hex
        var h2 = b2.hex
        while (h1.nonEmpty || h2.nonEmpty) {
          val b1 = h1.take(2)
          val b2 = h2.take(2)
          if (b1 ==* b2) {
            r1 += b1
            r2 += b2
          } else {
            r1 += Console.YELLOW_B + b1 + Console.BLACK_B
            r2 += Console.YELLOW_B + b2 + Console.BLACK_B
          }
          h1 = h1.drop(2)
          h2 = h2.drop(2)
        }
        (r1, r2)
      }
      failures :+=
        s"""
           |$descActual: $pre$s1$post
           |$descExpect: $pre$s2$post
           |""".stripMargin

      failures.mkString("\n")
    }

  def assertDecodeVia[A, B: Eq](p: SafePickler[A])(bin: BinaryData, expect: SafePickler.Result[B])
                                  (f: A => B, g: B => A)(implicit l: Line): Unit = {
    val actual = p.decode(bin).map(f)
    def info: Option[String] =
      expect match {
        case \/-(expectedB) =>
          val diff = binaryDiff(
            actual     = bin,
            expect     = p.encode(g(expectedB)),
            descActual = "Supplied  ",
            descExpect = "Re-encoded"
          )
          diff
        case -\/(_) =>
          None
    }

    assertEqO(info, actual, expect)
//    if (!Eq[SafePickler.Result[B]].eqv(actual, expect)) {
//      fail(info)
//    }
  }

  def assertDecode[A: Eq](p: SafePickler[A])(bin: BinaryData, expect: SafePickler.Result[A])(implicit l: Line): Unit =
    assertDecodeVia[A, A](p)(bin, expect)(identity, identity)

  def assertDecodeOk[A: Eq](p: SafePickler[A])(bin: BinaryData, expect: A)(implicit l: Line): Unit =
    assertDecode(p)(bin, \/-(expect))

  def assertRoundTrip[A: Eq](p: SafePickler[A])(a: A)(implicit l: Line): Unit =
    assertDecodeOk(p)(p.encode(a), a)

  def propTestRoundTrip[A: Eq](p: SafePickler[A])(g: Gen[A])(implicit l: Line): Unit =
    g.samples().take(propTestSize).foreach(assertRoundTrip(p)(_))

  def generateStabilityTest[A](p: SafePickler[A])(a: A)(implicit l: Line): Nothing = {
    val ver = p.version.verStr
    val hex = p.encode(a).hex
    val expect = a match {
      case s: String => "\"" + s + "\""
      case x         => x
    }
    val test =
      s"""
         |"$ver" - {
         |  val bin    = BinaryData.fromHex("$hex")
         |  val expect = $expect
         |  assertDecodeOk(codec)(bin, expect)
         |}
         |""".stripMargin.trim
    fail(s"Copy and paste this:\n\n  $test\n\n")
  }

  // ===================================================================================================================
  // Pickler testing

  def assertRoundTripP[A](a: A)(implicit p: Pickler[A], e: Eq[A], l: Line): Unit = {
    val sp = p.asV1(0)
    assertDecodeOk(sp)(sp.encode(a), a)
  }

  def propTestRoundTripP[A](g: Gen[A])(implicit p: Pickler[A], e: Eq[A], l: Line): Unit = {
    val sp = p.asV1(0)
    g.samples().take(propTestSize).foreach(assertRoundTrip(sp)(_))
  }

  def assertRoundTripsP[A](as: Iterable[A])(implicit p: Pickler[A], e: Eq[A], l: Line): Unit = {
    val sp = p.asV1(0)
    var i = 0
    for (a <- as) {
      i += 1
      val bin = sp.encode(a)
      assertEq(s"[$i/${as.size}]", sp.decode(bin), \/-(a))
    }
  }
}
