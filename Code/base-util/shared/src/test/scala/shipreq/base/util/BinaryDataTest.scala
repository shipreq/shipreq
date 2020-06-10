package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen.Gen
import utest._

object BinaryDataTest extends TestSuite {

  private val bytes =
    Gen.shuffle(Byte.MinValue.to(Byte.MaxValue).map(_.toByte).toList).sample()

  private val bd =
    BinaryData.unsafeFromArray(bytes.toArray)

  override def tests = Tests {

    "hex" - {

      "manual" - {
        val hex = "DEAD0B0E"
        val bd = BinaryData.fromHex(hex)
        assertEq(bd.hex, hex)
        assertEq(bd.unsafeArray.toList, List(0xDE, 0xAD, 0x0B, 0x0E).map(_.toByte))
      }

      "range" - assertEq(BinaryData.fromHex(bd.hex), bd)
    }

    "array" - assertEq(BinaryData.fromArray(bd.toNewArray), bd)

  }
}