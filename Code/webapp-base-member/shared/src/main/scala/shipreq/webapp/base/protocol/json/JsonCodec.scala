package shipreq.webapp.base.protocol.json

import io.circe._
import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.recursion._
import japgolly.univeq.UnivEq
import scala.reflect.ClassTag
import scalaz.Traverse
import scalaz.std.either._

final case class JsonCodec[A](encoder: Encoder[A], decoder: Decoder[A]) {

  def xmap[B](f: A => B)(g: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(g), decoder.map(f))

  def narrow[B <: A: ClassTag]: JsonCodec[B] =
    xmap[B]({
      case b: B => b
      case a    => throw new IllegalArgumentException("Illegal supertype: " + a)
    })(b => b)
}

object JsonCodec {

  def summon[A](implicit encoder: Encoder[A], decoder: Decoder[A]): JsonCodec[A] =
    apply(encoder, decoder)

  def xmap[A, B](f: A => B)(g: B => A)(implicit encoder: Encoder[A], decoder: Decoder[A]): JsonCodec[B] =
    summon[A].xmap(f)(g)

  def const[A](a: A): JsonCodec[A] =
    apply(Encoder.encodeUnit.contramap(_ => ()), Decoder.const(a))

  def enumAdt[A, B: UnivEq](f: AdtMacros.AdtIsoSet[A, B])(implicit encoder: Encoder[B], decoder: Decoder[B]): JsonCodec[A] = {
    val mapBA = f._4.iterator.map(b => (b, Right(f._2(b)))).toMap
    JsonCodec(
      Encoder.instance(a => encoder(f._1(a))),
      Decoder.instance(c =>
        decoder(c).flatMap(b => mapBA.getOrElse(b, Left(DecodingFailure(s"Unrecognised value: $b", c.history))))))
  }

  def fix[F[_]: Traverse](enc: FAlgebra[F, Json],
                          dec: FCoalgebraM[Decoder.Result, F, ACursor]): JsonCodec[Fix[F]] =
    JsonCodec(
      Encoder.instance[Fix[F]](Recursion.cata(enc)(_)),
      Decoder.instance[Fix[F]](Recursion.anaM(dec)(_)))

  lazy val str: JsonCodec[String] =
    summon

  object Implicits {
    implicit def implicitJsonCodecToDecoder[A](implicit c: JsonCodec[A]): Decoder[A] = c.decoder
    implicit def implicitJsonCodecToEncoder[A](implicit c: JsonCodec[A]): Encoder[A] = c.encoder
  }
}