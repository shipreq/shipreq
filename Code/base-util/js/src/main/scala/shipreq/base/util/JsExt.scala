package shipreq.base.util

import org.scalajs.dom.raw.Blob
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.ArrayBuffer

object JsExt {

  implicit class JsArrayExt[A](private val self: js.Array[A]) extends AnyVal {
    def forEachJs(f: js.Function1[A, Unit]): Unit = {
      self.asInstanceOf[js.Dynamic].forEach(f)
      ()
    }
  }

  implicit class BinaryDataJsExt(private val self: BinaryData) extends AnyVal {
    def toArrayBuffer: ArrayBuffer =
      BinaryJs.byteBufferToArrayBuffer(self.toByteBuffer)

    def toBlob: Blob =
      BinaryJs.byteBufferToBlob(self.toByteBuffer)

    def toNewJsArray: js.Array[Byte] =
      self.toNewArray.toJSArray

    def unsafeArrayBuffer: ArrayBuffer =
      BinaryJs.byteBufferToArrayBuffer(self.unsafeByteBuffer)

    def unsafeBlob: Blob =
      BinaryJs.byteBufferToBlob(self.unsafeByteBuffer)

    def unsafeJsArray: js.Array[Byte] =
      self.unsafeArray.toJSArray
  }

  implicit class BinaryDataObjectJsExt(private val self: BinaryData.type) extends AnyVal {
    def fromArrayBuffer(ab: ArrayBuffer): BinaryData =
      BinaryData.fromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))

    def unsafeFromArrayBuffer(ab: ArrayBuffer): BinaryData =
      BinaryData.unsafeFromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))
  }

}
