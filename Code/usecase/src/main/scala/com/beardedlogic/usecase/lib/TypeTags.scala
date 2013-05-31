package com.beardedlogic.usecase
package lib

import model._
import java.lang.{Long => JLong}

/**
 * @since 30/05/2013
 */
object TypeTags {
  // TODO doc TypeTags

  sealed trait TypeTag
  type Tagged[T <: TypeTag] = {type Tag = T}
  type @@[O, T <: TypeTag] = O with T
  @inline final def tag[T <: TypeTag](long: JLong) = long.asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag](long: scala.Long) = JLong.valueOf(long).asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag](long: Int) = JLong.valueOf(long).asInstanceOf[JLong @@ T]

  implicit class StringTypeExt(val s: String) extends AnyVal {
    def hasNormalisedRefs = s.asInstanceOf[String @@ NormalisedRefs]
    def asLocalStepId = s.asInstanceOf[String @@ LocalStepId]
  }

  implicit class LongTypeExt(val x: Long) extends AnyVal {
    def tag[T <: TypeTag] = JLong.valueOf(x).asInstanceOf[JLong @@ T]
  }

  trait NormalisedRefs extends TypeTag
  trait LocalStepId extends TypeTag

  trait FieldKeyId extends TypeTag
  type Long_FieldKeyId = JLong @@ FieldKeyId
  @inline final implicit def FieldKeyToId(v: Value[DataType.FieldKey]) = tag[FieldKeyId](v.valueId)

  trait StepValueId extends TypeTag
  type Long_StepValueId = JLong @@ StepValueId
  @inline final implicit def StepValueIdExtractor(v: PlainValue[DataType.Step]) = v.valueId.tag[StepValueId]

  trait StepDataId extends TypeTag
  type Long_StepDataId = JLong @@ StepDataId
  @inline final implicit def StepDataIdExtractor(v: PlainValue[DataType.Step]) = v.dataId.tag[StepDataId]

  implicit class StepValueExt(val v: PlainValue[DataType.Step]) extends AnyVal {
    def taggedDataId: Long_StepDataId = StepDataIdExtractor(v)
    def taggedValueId: Long_StepValueId = StepValueIdExtractor(v)
  }
}
