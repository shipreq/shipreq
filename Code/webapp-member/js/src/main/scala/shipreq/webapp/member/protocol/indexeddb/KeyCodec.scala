package shipreq.webapp.member.protocol.indexeddb

import japgolly.scalajs.react.CallbackTo
import scala.scalajs.js

final case class KeyCodec[A](encode: A => IndexedDbKey,
                             decode: IndexedDbKey => CallbackTo[A]) {

  def xmap[B](onDecode: A => B)(onEncode: B => A): KeyCodec[B] =
    // Delegating because decoding can fail and must be wrapped to be pure
    xmapSync(
      a => CallbackTo(onDecode(a)))(
      onEncode)

  def xmapSync[B](onDecode: A => CallbackTo[B])(onEncode: B => A): KeyCodec[B] =
    KeyCodec[B](
      encode = encode compose onEncode,
      decode = decode(_).flatMap(onDecode))
}

object KeyCodec {

  val int: KeyCodec[Int] =
    apply(IndexedDbKey(_), k => CallbackTo(
      (k.value: Any) match {
        case i: Int => i
        case _      => throw js.JavaScriptException(k.toString + " is not an int")
      }
    ))

  lazy val string: KeyCodec[String] =
    apply(IndexedDbKey(_), k => CallbackTo(
      (k.value: Any) match {
        case s: String => s
        case _         => throw js.JavaScriptException(k.toString + " is not a str")
      }
    ))
}
