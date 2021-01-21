package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen.Gen
import utest._

object BinaryDataTest extends TestSuite {

  private val bytes =
    Gen.shuffle(Byte.MinValue.to(Byte.MaxValue).map(_.toByte).toList).sample()

  private def bd =
    BinaryData.fromArray(bytes.toArray)

  override def tests = Tests {

    "noOffset" - {
      def expected = bd

      "unsafeFromByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bd.unsafeByteBuffer),
        expected)

      "fromByteBuffer" - assertEq(
        BinaryData.fromByteBuffer(bd.unsafeByteBuffer),
        expected)

      "unsafeByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bd.unsafeByteBuffer),
        expected)

      "toNewByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bd.toNewByteBuffer),
        expected)

      "toNewArray" - assertEq(
        BinaryData.unsafeFromArray(bd.toNewArray),
        expected)

      "unsafeArray" - assertEq(
        BinaryData.unsafeFromArray(bd.unsafeArray),
        expected)

      "binaryLikeString" - assertEq(
        bd.binaryLikeString,
        expected.binaryLikeString)

      "hex" - {

        "manual" - {
          val hex = "DEAD0B0E"
          val bd = BinaryData.fromHex(hex)
          assertEq(bd.hex, hex)
          assertEq(bd.unsafeArray.toList, List(0xDE, 0xAD, 0x0B, 0x0E).map(_.toByte))
        }

        "range" - assertEq(BinaryData.fromHex(bd.hex), bd)
      }

    }

    // -----------------------------------------------------------------------------------------------------------------
    "offset" - {
      def bd1 = bd.drop(1)
      def expected = BinaryData.unsafeFromArray(bd.unsafeArray.drop(1))
      def bb1 = bd.unsafeByteBuffer.position(1).slice()

      "123" - assertEq(
        BinaryData.fromArray("123".getBytes).drop(1),
        BinaryData.fromArray("23".getBytes))

      "unsafeFromByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bb1),
        expected)

      "fromByteBuffer" - assertEq(
        BinaryData.fromByteBuffer(bb1),
        expected)

      "equal" - {
        assertEq(bd1, expected)
        assertNotEq(bd1, bd)
      }

      "unsafeByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bd1.unsafeByteBuffer),
        expected)

      "toNewByteBuffer" - assertEq(
        BinaryData.unsafeFromByteBuffer(bd1.toNewByteBuffer),
        expected)

      "toNewArray" - assertEq(
        BinaryData.unsafeFromArray(bd1.toNewArray),
        expected)

      "unsafeArray" - assertEq(
        BinaryData.unsafeFromArray(bd1.unsafeArray),
        expected)

      "binaryLikeString" - assertEq(
        bd1.binaryLikeString,
        expected.binaryLikeString)

      "hex" - assertEq(
        bd1.hex,
        expected.hex)

      "overDrop" - assertEq(bd.drop(99999).length, 0)
    }
  }
}