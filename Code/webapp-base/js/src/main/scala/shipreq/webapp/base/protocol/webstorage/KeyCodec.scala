package shipreq.webapp.base.protocol.webstorage

import japgolly.scalajs.react.CallbackTo
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage.Key

final case class KeyCodec[A](encode: A => Key,
                             decode: Key => CallbackTo[A]) {

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

  val string: KeyCodec[String] =
    apply(Key.apply, k => CallbackTo.pure(k.value))

  val int: KeyCodec[Int] =
    string.xmap(_.toInt)(_.toString)
}
