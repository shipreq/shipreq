package shipreq.webapp.base.protocol.json.v1

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.nonempty.NonEmpty
import nyaya.util.{MultiValues, Multimap}
import scala.reflect.ClassTag
import shipreq.base.util.JsonUtil._
import shipreq.base.util._
import shipreq.webapp.base.protocol.json.JsonCodec
import shipreq.webapp.base.util._

private[v1] object BaseData {
  import JsonCodec.Implicits._

  // ===================================================================================================================
  // Polymorphic definitions
  // (non-implicit, "encode/decode/codec" prefixes)

  def codecBoolVia[A <: IsoBool[A], B: Decoder: Encoder: UnivEq](bool: IsoBool.Object[A])
                                                                (f: A => B): JsonCodec[A] = {
    val a1 = Right(bool.positive)
    val a2 = Right(bool.negative)
    val b1 = f(a1.value)
    val b2 = f(a2.value)
    val err = Left(s"Legal values are $b1 and $b2.")
    assert(b1 !=* b2)
    JsonCodec[A](
      Encoder[B].contramap(f),
      Decoder[B].emap(b => if (b ==* b1) a1 else if (b ==* b2) a2 else err))
  }

  def codecBool[A <: IsoBool[A]](bool: IsoBool.Object[A]): JsonCodec[A] = {
    val pos = bool.positive
    codecBoolVia(bool)(_.is(pos))
  }

  def codecDigraphBiDir[A: KeyDecoder : KeyEncoder : Decoder : Encoder : UnivEq]: JsonCodec[Digraph.BiDir[A]] =
    codecDigraphUniDir[A].xmap(Digraph.BiDir.apply[A])(_.forwards)

  def codecDigraphUniDir[A: KeyDecoder : KeyEncoder: Decoder : Encoder : UnivEq]: JsonCodec[Digraph.UniDir[A]] =
    codecMultimap[A, Set, A]

  def decodeDisj[A: Decoder, B: Decoder]: Decoder[A \/ B] = decodeSumBySoleKey {
    case ("r", c) => c.as[B].map(\/-(_))
    case ("l", c) => c.as[A].map(-\/(_))
  }

  def encodeDisj[A: Encoder, B: Encoder]: Encoder[A \/ B] = Encoder.instance {
    case \/-(b) => Json.obj("r" -> b.asJson)
    case -\/(a) => Json.obj("l" -> a.asJson)
  }

  def codecDisj[A, B](implicit a: JsonCodec[A], b: JsonCodec[B]): JsonCodec[A \/ B] =
    JsonCodec(encodeDisj, decodeDisj)

  def codecIMap[K: UnivEq, V: Decoder: Encoder](empty: IMap[K, V]): JsonCodec[IMap[K, V]] =
    JsonCodec.xmap(empty ++ (_: Iterable[V]))(_.values)

  def codecIsoBoolValues[B <: IsoBool[B], A: JsonCodec]: JsonCodec[IsoBool.Values[B, A]] =
    JsonCodec.xmap[(A, A), IsoBool.Values[B, A]](x => IsoBool.Values(pos = x._1, neg = x._2))(x => (x.pos, x.neg))

  def codecLazily[A](f: => JsonCodec[A]): JsonCodec[A] = {
    lazy val g = f
    JsonCodec(encodeLazily(g.encoder), decodeLazily(g.decoder))
  }

  def decodeLazily[A](f: => Decoder[A]): Decoder[A] = {
    lazy val g = f
    Decoder.instance(c => g(c))
  }

  def encodeLazily[A](f: => Encoder[A]): Encoder[A] = {
    lazy val g = f
    Encoder.instance(c => g(c))
  }

  def codecArraySeq[A: ClassTag : Decoder : Encoder]: JsonCodec[ArraySeq[A]] = {
    import CirceHacks.{decodeArraySeq, encodeArraySeq}
    JsonCodec.summon
  }

  def codecMap[K: KeyDecoder: KeyEncoder, V: Decoder: Encoder]: JsonCodec[Map[K, V]] =
    JsonCodec.summon

  def codecMultimap[K: KeyDecoder : KeyEncoder : UnivEq, L[_], V](implicit d: Decoder[L[V]], e: Encoder[L[V]], l: MultiValues[L]): JsonCodec[Multimap[K, L, V]] =
    codecMap[K, L[V]].xmap(Multimap(_))(_.m) // TODO optimise

  def codecNES[A: UnivEq](implicit d: Decoder[Set[A]], e: Encoder[Set[A]]): JsonCodec[NonEmptySet[A]] =
    codecNonEmpty(_.whole)

  def codecNEV[A](implicit d: Decoder[Vector[A]], e: Encoder[Vector[A]]): JsonCodec[NonEmptyVector[A]] =
    codecNonEmpty(_.whole)

  def codecNEA[A](implicit d: Decoder[ArraySeq[A]], e: Encoder[ArraySeq[A]]): JsonCodec[NonEmptyArraySeq[A]] = {
    codecNonEmpty(_.whole)
  }

  def codecNonEmpty[N, E](f: N => E)(implicit d: Decoder[E], e: Encoder[E], proof: NonEmpty.Proof[E, N]): JsonCodec[N] =
    JsonCodec.xmap[E, N](NonEmpty require_! _)(f)

  def codecNonEmptyMono[A](implicit d: Decoder[A], e: Encoder[A], proof: NonEmpty.ProofMono[A]): JsonCodec[NonEmpty[A]] =
    codecNonEmpty(_.value)

  def codecObfuscated[A]: JsonCodec[Obfuscated[A]] =
    JsonCodec.xmap(Obfuscated.apply[A])(_.value)

  def codecSetDiff[A: Decoder : Encoder : UnivEq]: JsonCodec[SetDiff[A]] =
    JsonCodec(
      Encoder.forProduct2("-", "+")(a => (a.removed, a.added)),
      Decoder.forProduct2("-", "+")(SetDiff.apply[A]))

  def codecTaggedI[A <: TaggedTypes.TaggedInt](apply: Int => A): JsonCodec[A] =
    JsonCodec.xmap(apply)(_.value)

  def codecTaggedL[A <: TaggedTypes.TaggedLong](apply: Long => A): JsonCodec[A] =
    JsonCodec.xmap(apply)(_.value)

  def codecTaggedS[A <: TaggedTypes.TaggedString](apply: String => A): JsonCodec[A] =
    JsonCodec.xmap(apply)(_.value)

  // ===================================================================================================================
  // Concrete picklers for base data type
  // (implicit lazy vals, "decoder/encoder/codec" prefix)

  implicit val codecDirection: JsonCodec[Direction] =
    codecBoolVia(Direction) {
      case Forwards  => "->"
      case Backwards => "<-"
    }

  implicit val codecNonEmptyVectorInt: JsonCodec[NonEmptyVector[Int]] =
    codecNEV

  implicit val codecVectorTreeParentLocation: JsonCodec[VectorTree.ParentLocation] = {
    import VectorTree._

    implicit val decoderParentLocationAt: Decoder[ParentLocation.At] =
      Decoder[Location].map(ParentLocation.At.apply)

    implicit val encoderParentLocationAt: Encoder[ParentLocation.At] =
      Encoder[Location].contramap(_.loc)

    implicit val decoderParentLocation: Decoder[ParentLocation] =
      decodeSumBySoleKeyOrConst[ParentLocation](
        "empty" -> ParentLocation.Empty
      ) {
        case ("at", c) => c.as[ParentLocation.At]
      }

    implicit val encoderParentLocation: Encoder[ParentLocation] = Encoder.instance {
      case a: ParentLocation.At => Json.obj("at" -> a.asJson)
      case ParentLocation.Empty => Json.fromString("empty")
    }

    JsonCodec.summon
  }

}
