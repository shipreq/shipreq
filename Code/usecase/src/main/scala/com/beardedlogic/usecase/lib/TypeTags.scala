package com.beardedlogic.usecase
package lib

import model._
import java.lang.{Long => JLong}

/**
 * @since 30/05/2013
 */
object TypeTags {

  sealed trait TypeTag[B]
  type Tagged[T <: TypeTag[_]] = {type Tag = T}
  type @@[O, T <: TypeTag[_]] = O with T

  @inline final def tag[T <: TypeTag[Long]](long: JLong) = long.asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag[Long]](long: scala.Long) = JLong.valueOf(long).asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag[Long]](long: Int) = JLong.valueOf(long).asInstanceOf[JLong @@ T]

  implicit def taggedStringOrdering[T <: TypeTag[String]] = implicitly[Ordering[String]].asInstanceOf[Ordering[String @@ T]]

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  /** Indicates that references to steps are in normalised form. Eg. [D.112] instead of [3.0.1] */
  trait NormalisedRefs extends TypeTag[String]

  /** A transient ID used as glue between internal components and/or the client. */
  trait LocalId extends TypeTag[String]

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  trait Label extends TypeTag[String]

  implicit class StringTypeExt(val s: String) extends AnyVal {
    def hasNormalisedRefs = s.asInstanceOf[String @@ NormalisedRefs]
    def asLocalId = s.asInstanceOf[String @@ LocalId]
    def asLabel = s.asInstanceOf[String @@ Label]
  }

  implicit class StringTypeExt2[M[_]](val s: M[String]) extends AnyVal {
    def asLocalIds = s.asInstanceOf[M[String @@ LocalId]]
    def asLabels = s.asInstanceOf[M[String @@ Label]]
  }

  implicit class StringTypeExt3[M[_]](val s: M[List[String]]) extends AnyVal {
    def asLocalIds = s.asInstanceOf[M[List[String @@ LocalId]]]
    def asLabels = s.asInstanceOf[M[List[String @@ Label]]]
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  /** Marks a Long value as corresponding to `field_key.id`. */
  trait FieldKeyId extends TypeTag[Long]
  type Long_FieldKeyId = JLong @@ FieldKeyId
  @inline final implicit def FieldKeyToId(v: Value[DataType.FieldKey]) = tag[FieldKeyId](v.valueId)

  /** Marks a Long value as corresponding to `step.id`. */
  trait StepValueId extends TypeTag[Long]
  type Long_StepValueId = JLong @@ StepValueId
  @inline final implicit def StepValueIdExtractor(v: PlainValue[DataType.Step]) = v.valueId.tag[StepValueId]

  /** Marks a Long value as corresponding to `data.id` of a Step. */
  trait StepDataId extends TypeTag[Long]
  type Long_StepDataId = JLong @@ StepDataId
  @inline final implicit def StepDataIdExtractor(v: PlainValue[DataType.Step]) = v.dataId.tag[StepDataId]

  implicit class StepValueExt(val v: PlainValue[DataType.Step]) extends AnyVal {
    def taggedDataId: Long_StepDataId = StepDataIdExtractor(v)
    def taggedValueId: Long_StepValueId = StepValueIdExtractor(v)
  }

  implicit class LongTypeExt(val x: Long) extends AnyVal {
    def tag[T <: TypeTag[Long]] = JLong.valueOf(x).asInstanceOf[JLong @@ T]
  }
}
