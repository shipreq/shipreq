package shipreq.webapp.member.protocol.binary

import nyaya.gen.Gen
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.BinaryData
import sourcecode.Line
import utest._
import utest.framework.TestPath

object BinaryStringTest extends TestSuite {

  private def assertRoundTrip(hex: String)(implicit l: Line): Any =
    assertRoundTrip(BinaryData.fromHex(hex))

  private def assertRoundTrip(bin: BinaryData)(implicit l: Line): Any = {
    val bs = BinaryString(bin)
    val desc = BinaryData.fromStringBytes(bs.encoded).describe()
    assertEq(desc, bs.binaryValue, bin)
    desc
  }

  override def tests = Tests {

    "roundTrip" - {
      def roundTripTest()(implicit tp: TestPath, l: Line) =
        assertRoundTrip(tp.value.last)

      "" - roundTripTest()

      "00" - roundTripTest()
      "ff" - roundTripTest()
      "ff00" - roundTripTest()
      "00ff" - roundTripTest()
      "ff00ff" - roundTripTest()
      "00ff00" - roundTripTest()
      "00ff00ff" - roundTripTest()
      "ff00ff00" - roundTripTest()

      "0011" - roundTripTest()
      "001122" - roundTripTest()
      "00112233" - roundTripTest()
      "0011223344" - roundTripTest()
      "001122334455" - roundTripTest()
      "00112233445566" - roundTripTest()
      "0011223344556677" - roundTripTest()
      "001122334455667788" - roundTripTest()
      "00112233445566778899" - roundTripTest()
      "00112233445566778899aa" - roundTripTest()
      "00112233445566778899aabb" - roundTripTest()
      "00112233445566778899aabbcc" - roundTripTest()
      "00112233445566778899aabbccdd" - roundTripTest()
      "00112233445566778899aabbccddee" - roundTripTest()
      "00112233445566778899aabbccddeeff" - roundTripTest()

      "1100" - roundTripTest()
      "112200" - roundTripTest()
      "11223300" - roundTripTest()
      "1122334400" - roundTripTest()
      "112233445500" - roundTripTest()
      "11223344556600" - roundTripTest()
      "1122334455667700" - roundTripTest()
      "112233445566778800" - roundTripTest()
      "11223344556677889900" - roundTripTest()
      "112233445566778899aa00" - roundTripTest()
      "112233445566778899aabb00" - roundTripTest()
      "112233445566778899aabbcc00" - roundTripTest()
      "112233445566778899aabbccdd00" - roundTripTest()
      "112233445566778899aabbccddee00" - roundTripTest()
      "112233445566778899aabbccddeeff00" - roundTripTest()

      "random" - {
        for (a <- Gen.byte.arraySeq(1 to 65536 * 2).samples().take(100))
          assertRoundTrip(BinaryData.fromArraySeq(a))
      }
    }
  }
}
