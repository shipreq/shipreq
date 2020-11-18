package shipreq.base.util

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen.Gen
import scala.scalajs.js
import scala.scalajs.js.typedarray.{Int8Array, TypedArrayBuffer}
import utest._

object BinaryDataJsTest extends TestSuite {
  import JsExt._

  private val bytes =
    Gen.shuffle(Byte.MinValue.to(Byte.MaxValue).map(_.toByte).toList).sample()

  private def bd =
    BinaryData.fromArray(bytes.toArray)

  override def tests = Tests {

    "noOffset" - {
      def expected = bd

      "fromArrayBuffer" - assertEq(
        BinaryData.fromArrayBuffer(bd.toArrayBuffer),
        expected)

      "unsafeFromArrayBuffer" - assertEq(
        BinaryData.unsafeFromArrayBuffer(bd.toArrayBuffer),
        expected)

      "typedArray" - {
        val buffer = Int8Array.from(js.Array(3, 4, 5)).buffer
        val view   = new Int8Array(buffer)
        val bb     = TypedArrayBuffer.wrap(view)
        val ia     = BinaryJs.byteBufferToInt8Array(bb)
        val ab     = BinaryJs.int8ArrayToArrayBuffer(ia)
        assertEq(
          BinaryData.unsafeFromArrayBuffer(ab),
          BinaryData.unsafeFromArray(Array(3, 4, 5)))
      }
    }

    // -----------------------------------------------------------------------------------------------------------------
    "offset" - {
      def bd1 = bd.drop(1)
      def expected = BinaryData.unsafeFromArray(bd.unsafeArray.drop(1))

      "fromArrayBuffer" - assertEq(
        BinaryData.fromArrayBuffer(bd1.toArrayBuffer),
        expected)

      "unsafeFromArrayBuffer" - assertEq(
        BinaryData.unsafeFromArrayBuffer(bd1.toArrayBuffer),
        expected)

      "typedArray" - {
        val buffer = Int8Array.from(js.Array(1, 2, 3, 4, 5)).buffer
        val view   = new Int8Array(buffer, 2)
        val bb     = TypedArrayBuffer.wrap(view)
        val ia     = BinaryJs.byteBufferToInt8Array(bb)
        val ab     = BinaryJs.int8ArrayToArrayBuffer(ia)
        assertEq(
          BinaryData.unsafeFromArrayBuffer(ab),
          BinaryData.unsafeFromArray(Array(3, 4, 5)))
      }
    }
  }
}