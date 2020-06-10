package shipreq.webapp.base.test

import boopickle.Pickler
import nyaya.gen.Gen
import scalaz.{-\/, Equal, \/-}
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
  implicit def equalSafePicklerDecoderFailure: Equal[SafePickler.DecodingFailure] =
    Equal.equalA

  def assertDecodeVia[A, B: Equal](p: SafePickler[A])(bin: BinaryData, expect: SafePickler.Result[B])
                                  (f: A => B, g: B => A)(implicit l: Line): Unit = {
    val actual = p.decode(bin).map(f)
    def info = {
      val limit = 200
      val descBin: BinaryData => String = _.describe(limit).filter(_ != ',')
      expect match {
        case \/-(a) =>
          val bin2 = p.encode(g(a))
          val (b1, b2) = shrinkUnequalStrings(bin.hex, bin2.hex, limit)
          val binaryMatches = b1.isEmpty && b2.isEmpty
          if (binaryMatches) {
            val (x1, x2) = shrinkUnequalStrings(actual.toString, expect.toString, limit)
            val toStringMatches = x1.isEmpty && x2.isEmpty
            if (toStringMatches)
              descBin(bin)
            else
              s"""
                 |Actual: $x1
                 |Expect: $x2
                 |""".stripMargin
          } else
            s"""
               |Subject:    ${bin.length} bytes
               |            $b1
               |Re-encoded: ${bin2.length} bytes
               |            $b2
               |""".stripMargin
        case -\/(_) => descBin(bin)
      }
    }
    // assertEq(info, actual, expect)
    if (!Equal[SafePickler.Result[B]].equal(actual, expect))
      fail(info)
  }

  def assertDecode[A: Equal](p: SafePickler[A])(bin: BinaryData, expect: SafePickler.Result[A])(implicit l: Line): Unit =
    assertDecodeVia[A, A](p)(bin, expect)(identity, identity)

  def assertDecodeOk[A: Equal](p: SafePickler[A])(bin: BinaryData, expect: A)(implicit l: Line): Unit =
    assertDecode(p)(bin, \/-(expect))

  def assertRoundTrip[A: Equal](p: SafePickler[A])(a: A)(implicit l: Line): Unit =
    assertDecodeOk(p)(p.encode(a), a)

  def propTestRoundTrip[A: Equal](p: SafePickler[A])(g: Gen[A])(implicit l: Line): Unit =
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

  def assertRoundTripP[A](a: A)(implicit p: Pickler[A], e: Equal[A], l: Line): Unit = {
    val sp = p.asV1(0)
    assertDecodeOk(sp)(sp.encode(a), a)
  }

  def propTestRoundTripP[A](g: Gen[A])(implicit p: Pickler[A], e: Equal[A], l: Line): Unit = {
    val sp = p.asV1(0)
    g.samples().take(propTestSize).foreach(assertRoundTrip(sp)(_))
  }

  def assertRoundTripsP[A](as: Iterable[A])(implicit p: Pickler[A], e: Equal[A], l: Line): Unit = {
    val sp = p.asV1(0)
    var i = 0
    for (a <- as) {
      i += 1
      val bin = sp.encode(a)
      assertEq(s"[$i/${as.size}]", sp.decode(bin), \/-(a))
    }
  }
}
