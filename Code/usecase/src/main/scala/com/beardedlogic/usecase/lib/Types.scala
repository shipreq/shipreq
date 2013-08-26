package com.beardedlogic.usecase
package lib

import java.lang.{Long => JLong}
import net.liftweb.common.Box
import net.liftweb.http.js.{JsCmd, JsCmds}
import scalaz.{Monoid, LensFamily}
import change.{Change, ChangeResultF}
import field.Field
import model.{FieldKeyRec, TextRev, UseCaseRev}
import util.{AppliedLens, LazyVal, BiMap}

/**
 * @since 30/05/2013
 */
object Types {

  // -------------------------------------------------------------------------------------------------------------------
  // Type tags

  sealed trait TypeTag[B]
  type Tagged[T <: TypeTag[_]] = {type Tag = T}
  type @@[O, T <: TypeTag[_]] = O with T

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  implicit def taggedStringOrdering[T <: TypeTag[String]] = implicitly[Ordering[String]].asInstanceOf[Ordering[String @@ T]]

  /** Indicates that references to steps are in normalised form. Eg. [D.112] instead of [3.0.1] */
  trait NormalisedRefs extends TypeTag[String]
  type TextWithNormalisedRefs = String @@ NormalisedRefs

  trait LocalIdTag extends TypeTag[String]
  type AnyLocalId = String @@ LocalIdTag

  /** A transient ID used to identify a TextField's . */
  trait LocalTextFieldIdTag extends LocalIdTag
  type LocalTextFieldId = String @@ LocalTextFieldIdTag

  /** A transient ID used to identify a single step node in memory. */
  trait LocalStepIdTag extends LocalIdTag
  type LocalStepId = String @@ LocalStepIdTag

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  trait Label extends TypeTag[String]
  type LabelStr = String @@ Label

  implicit class StringTypeExt(val s: String) extends AnyVal {
    def hasNormalisedRefs = s.asInstanceOf[TextWithNormalisedRefs]
    def asLocalStepId = s.asInstanceOf[LocalStepId]
    def asLabel = s.asInstanceOf[LabelStr]
    def tag[T <: TypeTag[String]] = s.asInstanceOf[String @@ T]
  }

  implicit class StringTypeExt2[M[_]](val s: M[String]) extends AnyVal {
    def asLocalStepIds = s.asInstanceOf[M[LocalStepId]]
    def asLabels = s.asInstanceOf[M[LabelStr]]
  }

  implicit class StringTypeExt3[M[_]](val s: M[List[String]]) extends AnyVal {
    def asLocalStepIds = s.asInstanceOf[M[List[LocalStepId]]]
    def asLabels = s.asInstanceOf[M[List[LabelStr]]]
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Long tags

  implicit class JLongTypeExt(val x: JLong) extends AnyVal {
    def tag[T <: TypeTag[Long]] = x.asInstanceOf[JLong @@ T]
  }
  implicit class LongTypeExt(val x: scala.Long) extends AnyVal {
    def tag[T <: TypeTag[Long]] = JLong.valueOf(x).tag[T]
  }
  implicit class LongOptionExt(val x: Option[Long]) extends AnyVal {
    def tag[T <: TypeTag[Long]] = x.map(_.tag[T])
  }
  implicit class LongBoxExt(val x: Box[scala.Long]) extends AnyVal {
    def tag[T <: TypeTag[Long]] = x.map(_.tag[T])
  }

  /** Marks a Long value as corresponding to `field_key.id`. */
  trait FieldKeyIdTag extends TypeTag[Long]
  type FieldKeyId = JLong @@ FieldKeyIdTag
  @inline final implicit def FieldKeyToId(r: FieldKeyRec) = r.id.tag[FieldKeyIdTag]

  /** Marks a Long value as corresponding to `usecase.id` and `usecase_rev.ident_id`. */
  trait UseCaseIdentIdTag extends TypeTag[Long]
  type UseCaseIdentId = JLong @@ UseCaseIdentIdTag
  @inline final implicit def UseCaseRevToIdentId(r: UseCaseRev) = r.identId.tag[UseCaseIdentIdTag]

  /** Marks a Long value as corresponding to `usecase_rev.id`. */
  trait UseCaseRevIdTag extends TypeTag[Long]
  type UseCaseRevId = JLong @@ UseCaseRevIdTag
  @inline final implicit def UseCaseRevToId(r: UseCaseRev) = r.id.tag[UseCaseRevIdTag]

  /** Marks a Long value as corresponding to `text.id` and `text_rev.ident_id`. */
  trait TextIdentIdTag extends TypeTag[Long]
  type TextIdentId = JLong @@ TextIdentIdTag
  @inline final implicit def TextRevToIdentId(r: TextRev) = r.identId.tag[TextIdentIdTag]

  /** Marks a Long value as corresponding to `text_rev.id`. */
  trait TextRevIdTag extends TypeTag[Long]
  type TextRevId = JLong @@ TextRevIdTag
  @inline final implicit def TextRevToId(r: TextRev) = r.id.tag[TextRevIdTag]

  // -------------------------------------------------------------------------------------------------------------------
  // Typedefs

  type FieldKeyRecData = Option[String]
  type FieldValueRecData = Option[String]

  type SavedSteps = BiMap[TextIdentId, LocalStepId]
  def EmptySavedSteps: SavedSteps = BiMap.empty

  type StepAndLabelBiMap = LazyVal[BiMap[LocalStepId, LabelStr]]
  final val EmptyStepAndLabelBiMap: StepAndLabelBiMap = LazyVal <~ BiMap.empty

  type FieldValues = Map[Field, Field#Value]

  type UcUpdateResult = ChangeResultF[UseCase, (UcChangeDomain, Change)]

  // Due to http://youtrack.jetbrains.com/issue/SCL-5900
  @inline final def alens[A1, A2, B](l: LensFamily[A1, A2, B, B], key: A1) = AppliedLens(l, key)

  // -------------------------------------------------------------------------------------------------------------------
  // Type class instances

  implicit object JsCmdMonoid extends Monoid[JsCmd] {
    import JsCmds.{_Noop => Noop}
    override def zero = Noop
    override def append(a: JsCmd, b: => JsCmd) =
      if (a eq Noop) b
      else if (b eq Noop) a
      else a & b
  }
}
