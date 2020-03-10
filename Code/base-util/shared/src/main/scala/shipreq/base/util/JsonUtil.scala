package shipreq.base.util

import io.circe._
import japgolly.microlibs.utils.StaticLookupFn
import japgolly.univeq.UnivEq

object JsonUtil {

  implicit def univEqJsonObject: UnivEq[JsonObject] = UnivEq.force
  implicit def univEqJson: UnivEq[Json]             = UnivEq.force

  def decoderFnSumBySoleKey[A](f: PartialFunction[(String, ACursor), Decoder.Result[A]]): ACursor => Decoder.Result[A] = {
    def keyErr = "Expected a single key indicating the subtype"
    c =>
      c.keys match {
        case Some(it) =>
          it.toList match {
            case singleKey :: Nil =>
              val arg  = (singleKey, c.downField(singleKey))
              def fail = Left(DecodingFailure("Unknown subtype: " + singleKey, c.history))
              f.applyOrElse(arg, (_: (String, ACursor)) => fail)
            case Nil  => Left(DecodingFailure(keyErr, c.history))
            case keys => Left(DecodingFailure(s"$keyErr, found multiple: $keys", c.history))
          }
        case None => Left(DecodingFailure(keyErr, c.history))
      }
  }

  def decodeSumBySoleKey[A](f: PartialFunction[(String, ACursor), Decoder.Result[A]]): Decoder[A] =
    Decoder.instance(decoderFnSumBySoleKey(f))

  def decodeSumBySoleKeyOrConst[A](consts: (String, A)*)(f: PartialFunction[(String, ACursor), Decoder.Result[A]]): Decoder[A] =
    decodeSumBySoleKeyOr(consts: _*)(decoderFnSumBySoleKey(f))

  def decodeSumBySoleKeyOr[A](consts: (String, A)*)(orElse: ACursor => Decoder.Result[A]): Decoder[A] = {
    val lookup = StaticLookupFn.useMap(consts).toOption
    Decoder.instance(c =>
      c.as[String].toOption.flatMap(lookup) match {
        case Some(r) => Right(r)
        case None    => orElse(c)
      }
    )
  }

  def decodingFailureMsg(f: DecodingFailure): String =
    s"Failed to decode JSON at ${CursorOp.opsToPath(f.history)}: ${f.message}"

  def errorMsg(e: Error): String =
    e match {
      case f: ParsingFailure => s"Failed to parse JSON: ${f.message}"
      case f: DecodingFailure => decodingFailureMsg(f)
    }

}
