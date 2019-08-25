package shipreq.webapp.base.test

import nyaya.gen.Gen
import scalaz.{Equal, \/-}
import sourcecode.Line
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData

object BinaryTestUtil {

  // Not true but good enough for now
  implicit def univEqSafePicklerDecoderFailure: UnivEq[SafePickler.DecodingFailure] = UnivEq.force

  def assertDecode[A: Equal](p: SafePickler[A])(bin: BinaryData, expect: SafePickler.Result[A])(implicit l: Line): Unit =
    assertEq(p.decode(bin), expect)

  def assertDecodeOk[A: Equal](p: SafePickler[A])(bin: BinaryData, expect: A)(implicit l: Line): Unit =
    assertDecode(p)(bin, \/-(expect))

  def assertRoundTrip[A: Equal](p: SafePickler[A])(a: A)(implicit l: Line): Unit =
    assertDecodeOk(p)(p.encode(a), a)

  def propTestRoundTrip[A: Equal](p: SafePickler[A])(g: Gen[A])(implicit l: Line): Unit =
    g.samples().take(33).foreach(assertRoundTrip(p)(_))

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
}
