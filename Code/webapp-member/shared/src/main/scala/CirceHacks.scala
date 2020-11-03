package io.circe

import scala.reflect.ClassTag

object CirceHacks extends MidPriorityEncoders {

  implicit final def decodeArraySeq[A: ClassTag : Decoder]: Decoder[ArraySeq[A]] =
    new SeqDecoder[A, ArraySeq](implicitly) {
      final protected def createBuilder() = ArraySeq.newBuilder[A]
    }

  implicit final def encodeArraySeq[A: Encoder]: Encoder[ArraySeq[A]] =
    new IterableAsArrayEncoder[A, ArraySeq](implicitly) {
      final protected def toIterator(a: ArraySeq[A]) = a.iterator
    }
}