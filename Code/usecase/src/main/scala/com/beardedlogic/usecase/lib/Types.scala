package com.beardedlogic.usecase
package lib

import java.lang.{Long => JLong}
import scalaz.LensFamily
import change.{Change, ChangeResultF}
import field.Field
import model._
import util.{AppliedLens, LazyVal, BiMap}
import net.liftweb.common.Box

/**
 * @since 30/05/2013
 */
object Types {

  // -------------------------------------------------------------------------------------------------------------------
  // Typedefs

  type FieldKeyRecData = Option[String]
  type FieldValueRecData = Option[String]

  type SavedSteps = BiMap[Long_StepDataId, LocalIdStr]
  def EmptySavedSteps: SavedSteps = BiMap.empty

  type StepAndLabelBiMap = LazyVal[BiMap[LocalIdStr, LabelStr]]
  final val EmptyStepAndLabelBiMap: StepAndLabelBiMap = LazyVal <~ BiMap.empty

  type FieldStates = Map[Field, Field#State]
  type FieldValues = Map[Field, Field#Value]

  type UcUpdateResult = ChangeResultF[UseCase, (UcChangeDomain, Change)]

  // Due to http://youtrack.jetbrains.com/issue/SCL-5900
  @inline final def alens[A1, A2, B](l: LensFamily[A1, A2, B, B], key: A1) = AppliedLens(l, key)

  // -------------------------------------------------------------------------------------------------------------------
  // Type tags

  // TODO Use Scalaz TypeTags
  sealed trait TypeTag[B]
  type Tagged[T <: TypeTag[_]] = {type Tag = T}
  type @@[O, T <: TypeTag[_]] = O with T

  @inline final def tag[T <: TypeTag[Long]](long: JLong) = long.asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag[Long]](long: scala.Long) = JLong.valueOf(long).asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag[Long]](long: Int) = JLong.valueOf(long).asInstanceOf[JLong @@ T]
  @inline final def tag[T <: TypeTag[Long]](m: Box[scala.Long]) = m.map(_.tag[T])
  @inline final def tag[T <: TypeTag[Long]](m: Option[scala.Long]) = m.map(_.tag[T])

  implicit def taggedStringOrdering[T <: TypeTag[String]] = implicitly[Ordering[String]].asInstanceOf[Ordering[String @@ T]]

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  /** Indicates that references to steps are in normalised form. Eg. [D.112] instead of [3.0.1] */
  trait NormalisedRefs extends TypeTag[String]
  type TextWithNormalisedRefs = String @@ NormalisedRefs

  /** A transient ID used as glue between internal components and/or the client. */
  trait LocalId extends TypeTag[String]
  type LocalIdStr = String @@ LocalId

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  trait Label extends TypeTag[String]
  type LabelStr = String @@ Label

  implicit class StringTypeExt(val s: String) extends AnyVal {
    def hasNormalisedRefs = s.asInstanceOf[TextWithNormalisedRefs]
    def asLocalId = s.asInstanceOf[LocalIdStr]
    def asLabel = s.asInstanceOf[LabelStr]
  }

  implicit class StringTypeExt2[M[_]](val s: M[String]) extends AnyVal {
    def asLocalIds = s.asInstanceOf[M[LocalIdStr]]
    def asLabels = s.asInstanceOf[M[LabelStr]]
  }

  implicit class StringTypeExt3[M[_]](val s: M[List[String]]) extends AnyVal {
    def asLocalIds = s.asInstanceOf[M[List[LocalIdStr]]]
    def asLabels = s.asInstanceOf[M[List[LabelStr]]]
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  /** Marks a Long value as corresponding to `field_key.id`. */
  trait FieldKeyIdTag extends TypeTag[Long]
  type FieldKeyId = JLong @@ FieldKeyIdTag
  @inline final implicit def FieldKeyToId(r: FieldKeyRec) = tag[FieldKeyIdTag](r.id)

  /** Marks a Long value as corresponding to `usecase.id` and `usecase_rev.ident_id`. */
  trait UseCaseIdentIdTag extends TypeTag[Long]
  type UseCaseIdentId = JLong @@ UseCaseIdentIdTag
  @inline final implicit def UseCaseRevToIdentId(r: UseCaseRev) = tag[UseCaseIdentIdTag](r.identId)

  /** Marks a Long value as corresponding to `usecase_rev.id`. */
  trait UseCaseRevIdTag extends TypeTag[Long]
  type UseCaseRevId = JLong @@ UseCaseRevIdTag
  @inline final implicit def UseCaseRevToId(r: UseCaseRev) = tag[UseCaseRevIdTag](r.id)

  implicit class LongTypeExt(val x: Long) extends AnyVal {
    def tag[T <: TypeTag[Long]] = JLong.valueOf(x).asInstanceOf[JLong @@ T]
  }
}
