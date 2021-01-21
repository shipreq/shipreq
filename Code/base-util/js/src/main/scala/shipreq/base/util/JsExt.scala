package shipreq.base.util

import org.scalajs.dom.raw.Blob
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

object JsExt {

  implicit class JsAnyExt(private val self: Any) extends AnyVal {
    @inline def falsy: Boolean = {
      val a = self.asInstanceOf[js.Dynamic]
      (!a).asInstanceOf[Boolean]
    }

    @inline def truthy: Boolean =
      !falsy
  }

  implicit class JsArrayExt[A](private val self: js.Array[A]) extends AnyVal {
    def forEachJs(f: js.Function1[A, Unit]): Unit = {
      self.asInstanceOf[js.Dynamic].forEach(f)
      ()
    }
  }

  implicit class BinaryDataJsExt(private val self: BinaryData) extends AnyVal {
    def toArrayBuffer: ArrayBuffer =
      BinaryJs.byteBufferToArrayBuffer(self.unsafeByteBuffer)

    def toUint8Array: Uint8Array =
      new Uint8Array(toArrayBuffer)

    def toBlob: Blob =
      BinaryJs.byteBufferToBlob(self.unsafeByteBuffer)

    def toNewJsArray: js.Array[Byte] =
      self.toNewArray.toJSArray

    def unsafeArrayBuffer: ArrayBuffer =
      BinaryJs.byteBufferToArrayBuffer(self.unsafeByteBuffer)

    def unsafeUint8Array: Uint8Array =
      new Uint8Array(unsafeArrayBuffer)

    def unsafeBlob: Blob =
      BinaryJs.byteBufferToBlob(self.unsafeByteBuffer)

    def unsafeJsArray: js.Array[Byte] =
      self.unsafeArray.toJSArray
  }

  implicit class BinaryDataObjectJsExt(private val self: BinaryData.type) extends AnyVal {
    def fromArrayBuffer(ab: ArrayBuffer): BinaryData =
      BinaryData.fromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))

    def fromUint8Array(a: Uint8Array): BinaryData =
      fromArrayBuffer(BinaryJs.uint8ArrayToArrayBuffer(a))

    def unsafeFromArrayBuffer(ab: ArrayBuffer): BinaryData =
      BinaryData.unsafeFromByteBuffer(BinaryJs.arrayBufferToByteBuffer(ab))

    def unsafeFromUint8Array(a: Uint8Array): BinaryData =
      unsafeFromArrayBuffer(BinaryJs.uint8ArrayToArrayBuffer(a))
  }

}
