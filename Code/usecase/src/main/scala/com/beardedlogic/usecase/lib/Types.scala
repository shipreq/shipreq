package com.beardedlogic.usecase
package lib

import java.lang.{Long => JLong, Short => JShort}
import net.liftweb.common.Box
import net.liftweb.http.js.{JsCmd, JsCmds}
import scalaz.{LensFamily, Monoid, Name, Value}
import change.{Change, ChangeResultF}
import field.Field
import db.{UseCaseIdent, UserRegistrationInfo, UserDescriptor, FieldKeyRec, TextRev, UseCaseRev}
import util.{AppliedLens, BiMap}

/**
 * @since 30/05/2013
 */
object Types {

  // -------------------------------------------------------------------------------------------------------------------
  // Type tags

  trait TypeTag[B]
  // type Tagged[T <: TypeTag[_]] = {type Tag = T}
  type @@[O, T <: TypeTag[_]] = O with T

  // -------------------------------------------------------------------------------------------------------------------
  // String tags

  implicit def taggedStringOrdering[T <: TypeTag[String]] = implicitly[Ordering[String]].asInstanceOf[Ordering[String @@ T]]

  sealed trait JsonTag[T] extends TypeTag[String]
  type Json[T] = String @@ JsonTag[T]

  /** Indicates that references to steps are in normalised form. Eg. [D.112] instead of [3.0.1] */
  sealed trait TextWithNormalisedRefsTag extends TypeTag[String]
  type TextWithNormalisedRefs = String @@ TextWithNormalisedRefsTag

  sealed trait LocalIdTag extends TypeTag[String]
  type AnyLocalId = String @@ LocalIdTag

  /** A transient ID used to identify a TextField's . */
  sealed trait LocalTextFieldIdTag extends LocalIdTag
  type LocalTextFieldId = String @@ LocalTextFieldIdTag

  /** A transient ID used to identify a single step node in memory. */
  sealed trait LocalStepIdTag extends LocalIdTag
  type LocalStepId = String @@ LocalStepIdTag

  /** A textual label for a tree node. Eg. "1.0.2.a" */
  sealed trait LabelTag extends TypeTag[String]
  type LabelStr = String @@ LabelTag

  implicit class StringTypeExt(val s: String) extends AnyVal {
    def hasNormalisedRefs = s.asInstanceOf[TextWithNormalisedRefs]
    def asLocalStepId = s.asInstanceOf[LocalStepId]
    def asLabel = s.asInstanceOf[LabelStr]
    def tag[T <: TypeTag[String]] = s.asInstanceOf[String @@ T]
  }

  implicit class StringTypeExt2[F[_]](val s: F[String]) extends AnyVal {
    def asLocalStepIds = s.asInstanceOf[F[LocalStepId]]
    def asLabels = s.asInstanceOf[F[LabelStr]]
  }

  implicit class StringTypeExt3[G[_], F[G]](val s: F[G[String]]) extends AnyVal {
    def asLocalStepIds = s.asInstanceOf[F[G[LocalStepId]]]
    def asLabels = s.asInstanceOf[F[G[LabelStr]]]
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Short tags

  implicit class JShortTypeExt(val x: JShort) extends AnyVal {
    def tag[T <: TypeTag[Short]] = x.asInstanceOf[JShort @@ T]
  }
  implicit class ShortTypeExt(val x: scala.Short) extends AnyVal {
    def tag[T <: TypeTag[Short]] = JShort.valueOf(x).tag[T]
  }
  implicit class ShortOptionExt(val x: Option[Short]) extends AnyVal {
    def tag[T <: TypeTag[Short]] = x.map(_.tag[T])
  }
  implicit class ShortBoxExt(val x: Box[scala.Short]) extends AnyVal {
    def tag[T <: TypeTag[Short]] = x.map(_.tag[T])
  }

  /** Marks a Short value as corresponding to `usecase.number`. */
  sealed trait UseCaseNumberTag extends TypeTag[Short]
  type UseCaseNumber = JShort @@ UseCaseNumberTag
  @inline final implicit def UcIdentToUcN(u: UseCaseIdent): UseCaseNumber = u.number

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
  sealed trait FieldKeyIdTag extends TypeTag[Long]
  type FieldKeyId = JLong @@ FieldKeyIdTag
  @inline final implicit def FieldKeyToId(r: FieldKeyRec): FieldKeyId = r.id

  /** Marks a Long value as corresponding to `usecase_rev.id`. */
  sealed trait UseCaseRevIdTag extends TypeTag[Long]
  type UseCaseRevId = JLong @@ UseCaseRevIdTag
  @inline final implicit def UseCaseRevToId(r: UseCaseRev): UseCaseRevId = r.id

  /** Marks a Long value as corresponding to `text.id` and `text_rev.ident_id`. */
  sealed trait TextIdentIdTag extends TypeTag[Long]
  type TextIdentId = JLong @@ TextIdentIdTag
  @inline final implicit def TextRevToIdentId(r: TextRev): TextIdentId = r.identId

  /** Marks a Long value as corresponding to `usr.id`. */
  sealed trait UserIdTag extends TypeTag[Long]
  type UserId = JLong @@ UserIdTag
  @inline final implicit def UserToId1(a: UserDescriptor): UserId = a.id
  @inline final implicit def UserToId2(a: UserRegistrationInfo): UserId = a.id

  // -------------------------------------------------------------------------------------------------------------------
  // Externalisable ID tags

  sealed trait ExteralisableIdTag extends TypeTag[Long] {
    type EITag <: TypeTag[String]
    type EI = String @@ EITag
  }

  /** Marks a Long value as corresponding to `usecase.id` and `usecase_rev.ident_id`. */
  sealed trait UseCaseIdentIdTag extends ExteralisableIdTag {
    override type EITag = UseCaseIdentEITag
  }
  sealed trait UseCaseIdentEITag extends TypeTag[String]
  type UseCaseIdentId = JLong @@ UseCaseIdentIdTag
  type UseCaseIdentEI = String @@ UseCaseIdentEITag
  @inline final implicit def UseCaseRevToIdentId(r: UseCaseRev): UseCaseIdentId = r.identId
  @inline final implicit def UseCaseIdentToIdentId(i: UseCaseIdent): UseCaseIdentId = i.identId

  /** Marks a Long value as corresponding to `text_rev.id`. */
  sealed trait TextRevIdTag extends ExteralisableIdTag {
    override type EITag = TextRevEITag
  }
  sealed trait TextRevEITag extends TypeTag[String]
  type TextRevId = JLong @@ TextRevIdTag
  type TextRevEI = String @@ TextRevEITag
  @inline final implicit def TextRevToId(r: TextRev): TextRevId = r.id

  /** Marks a Long value as corresponding to `project.id`. */
  trait ProjectIdTag extends ExteralisableIdTag {
    override type EITag = ProjectEITag
  }
  sealed trait ProjectEITag extends TypeTag[String]
  type ProjectId = JLong @@ ProjectIdTag
  type ProjectEI = String @@ ProjectEITag

  object AutoExternaliseIds {
    implicit def autoExternaliseId_P(id: ProjectId): ProjectEI = ExternalId.Project(id)
    implicit def autoExternaliseId_UC(id: UseCaseIdentId): UseCaseIdentEI = ExternalId.UseCase(id)
    implicit def autoExternaliseId_TR(id: TextRevId): TextRevEI = ExternalId.TextRev(id)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Typedefs

  type FieldKeyRecData = Option[String]
  type FieldValueRecData = Option[String]

  type SavedSteps = BiMap[TextIdentId, LocalStepId]
  def EmptySavedSteps: SavedSteps = BiMap.empty

  type StepAndLabelBiMap = Name[BiMap[LocalStepId, LabelStr]]
  final val EmptyStepAndLabelBiMap: StepAndLabelBiMap = Value(BiMap.empty)

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
