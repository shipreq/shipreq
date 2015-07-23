package shipreq.webapp.base.data

import monocle._
import monocle.macros.Lenses
import scalaz.{Traverse, OneAnd, Equal}
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr}
import shipreq.base.util._
import shipreq.base.util.TaggedTypes.{TaggedString, TaggedInt}
import shipreq.webapp.base.util.TypeclassDerivation._
import Must.Auto._

// =====================================================================================================================
// Types

sealed abstract class FieldType(val name: String)
sealed abstract class StaticFieldType(name: String) extends FieldType(name)
sealed abstract class CustomFieldType(name: String) extends FieldType(name)

object StaticFieldType {
  case object StepTree  extends StaticFieldType("Step Tree")
  case object StepGraph extends StaticFieldType("Step Graph")

  val values = NonEmptyVector[StaticFieldType](
    StepTree,
    StepGraph)


  implicit val equality: UnivEq[StaticFieldType] = { import AutoDerive._; deriveUnivEq }
}

object CustomFieldType {
  case object Implication extends CustomFieldType("Implication")
  case object Tag         extends CustomFieldType("Tag")
  case object Text        extends CustomFieldType("Text")

  val values = NonEmptyVector[CustomFieldType](
    Implication,
    Tag,
    Text)

  implicit val equality: UnivEq[CustomFieldType] = { import AutoDerive._; deriveUnivEq }
}

object FieldType {
  val values: NonEmptyVector[FieldType] =
    StaticFieldType.values ++ CustomFieldType.values

  implicit val equality: UnivEq[FieldType] = { import AutoDerive._; deriveUnivEq }
}

// =====================================================================================================================
// Instances

/**
 * A key by which users can refer to a field.
 */
final case class FieldRefKey(value: String) extends TaggedString

sealed trait Mandatory
case object Mandatory extends Mandatory with IsoBool.Obj[Mandatory] {
  override protected def neg = Not
  case object Not extends Mandatory
}

sealed trait Deletable
case object Deletable extends Deletable with IsoBool.Obj[Deletable] {
  override protected def neg = Not
  case object Not extends Deletable
}

/** type [[FieldId]] = [[StaticField]] | [[CustomFieldId]] */
sealed trait FieldId {
  def foldId[A](s: StaticField => A, c: CustomFieldId => A): A
}

object FieldId {
  implicit object IdGeneric extends Generic[FieldId] {
    override type Repr = StaticField :+: CustomFieldId :+: CNil
    override def to  (id: FieldId): Repr = id.foldId(Coproduct[Repr](_), Coproduct[Repr](_))
    override def from(co: Repr): FieldId = co match {
      case Inl(s)      => s
      case Inr(Inl(c)) => c
      case _           => ???
    }
  }

  implicit val idEquality: UnivEq[FieldId] = deriveUnivEq
}

sealed trait Field {
  def fieldType: FieldType
  def reqTypes : Field.ApplicableReqTypes
  def keyO     : Option[FieldRefKey]
  def mandatory: Mandatory
  def live     : Live

  /** Independent as opposed to the name being derived from some external state. */
  def independentName: Option[String]

  def fold[A](s: StaticField => A, c: CustomField => A): A

  final def fieldId: FieldId =
    fold(s => s, _.id)
}

object Field {
  type ApplicableReqTypes = ISubset[ReqTypeId]

  implicit lazy val applicableReqTypesEquality: UnivEq[ApplicableReqTypes] = implicitly

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree) = {
    val cn: CustomField => Must[String] = CustomField.name(customReqTypes, tags)
    val fn: Field       => Must[String] = {
      case f: StaticField => f.name
      case f: CustomField => cn(f)
    }
    fn
  }

  def nameP(p: Project) = name(p.config.customReqTypes.data, p.config.tags.data)
}

import Field.ApplicableReqTypes

sealed abstract class StaticField(         val name     : String,
                                  override val fieldType: StaticFieldType,
                                  override val reqTypes : Field.ApplicableReqTypes,
                                  override val mandatory: Mandatory,
                                           val deletable: Deletable,
                                  override val keyO     : Option[FieldRefKey]) extends Field with FieldId {

  override final def live = Live

  override final def independentName = Some(name)

  override final def fold  [A](s: StaticField => A, c: CustomField   => A): A = s(this)
  override final def foldId[A](s: StaticField => A, c: CustomFieldId => A): A = s(this)
}

object StaticField {
  val useCaseOnly: ApplicableReqTypes =
    ISubset.Only(NonEmptySet one StaticReqType.UseCase)

  @inline final private[this] def T = StaticFieldType

  case object NormalAltStepTree extends StaticField(
    "Normal and Alternate Courses", T.StepTree, useCaseOnly, Mandatory.Not, Deletable.Not, None)

  case object ExceptionStepTree extends StaticField(
    "Exception Courses", T.StepTree, useCaseOnly, Mandatory.Not, Deletable.Not, None)

  case object StepGraph extends StaticField(
    "Step Graph", T.StepGraph, useCaseOnly, Mandatory.Not, Deletable, None)

  // Non lazy causes utest to crash
  lazy val values: NonEmptyVector[StaticField] =
    NonEmptyVector(NormalAltStepTree, ExceptionStepTree, StepGraph)

  lazy val (deletable, notDeletable) =
    values.whole.partition(_.deletable ≟ Deletable)

  lazy val names: Set[String] =
    values.toStream.map(_.name).toSet

  implicit val equality: UnivEq[StaticField] = { import AutoDerive._; deriveUnivEq }
}

sealed abstract class CustomFieldId extends TaggedInt with FieldId {
  final def foldId[A](s: StaticField => A, c: CustomFieldId => A): A = c(this)
}

/** Custom here just distinguishes user-defined fields from static fields. */
sealed abstract class CustomField(override final val fieldType: CustomFieldType) extends Field {
  def id: CustomFieldId

  override final def fold[A](s: StaticField => A, c: CustomField => A): A = c(this)
}

object CustomField {
  object IdAccess extends ObjDataId[CustomField.type, CustomField, CustomFieldId] {
    override def id(d: CustomField) = d.id
    override val unapplyData: AnyRef => Option[CustomField] = {case r: CustomField => Some(r); case _ => None}
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Text(id       : Text.Id,
                  name     : String,
                  key      : FieldRefKey,
                  mandatory: Mandatory,
                  reqTypes : ApplicableReqTypes,
                  live     : Live) extends CustomField(CustomFieldType.Text) {
    override def independentName = Some(name)
    override def keyO = Some(key)
  }
  object Text {
    final case class Id(value: Int) extends CustomFieldId {
      override def toString = s"CustomField.Text.Id($value)"
    }
    object IdAccess extends ObjDataId[Text.type, Text, Id] {
      override def id(d: Text) = d.id
      override val unapplyData: AnyRef => Option[Text] = {case r: Text => Some(r); case _ => None}
    }
    implicit val equality: UnivEq[Text] = deriveUnivEq
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Tag(id       : Tag.Id,
                 tagId    : TagId,
                 mandatory: Mandatory,
                 reqTypes : ApplicableReqTypes,
                 live     : Live) extends CustomField(CustomFieldType.Tag) {
    override def independentName = None
    override def keyO = None

    def name(tags: TagTree): Must[String] =
      tags(tagId).map(_.tag.name)
  }
  object Tag {
    final case class Id(value: Int) extends CustomFieldId  {
      override def toString = s"CustomField.Tag.Id($value)"
    }
    object IdAccess extends ObjDataId[Tag.type, Tag, Id] {
      override def id(d: Tag) = d.id
      override val unapplyData: AnyRef => Option[Tag] = {case r: Tag => Some(r); case _ => None}
    }
    implicit val equality: UnivEq[Tag] = deriveUnivEq
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Implication(id       : Implication.Id,
                         reqTypeId: ReqTypeId,
                         mandatory: Mandatory,
                         reqTypes : ApplicableReqTypes,
                         live     : Live) extends CustomField(CustomFieldType.Implication) {
    override def independentName = None
    override def keyO = None

    def name(customReqTypes: CustomReqTypeIMap): Must[String] =
      ReqType.name(customReqTypes)(reqTypeId)
  }
  object Implication {
    final case class Id(value: Int) extends CustomFieldId {
      override def toString = s"CustomField.Implication.Id($value)"
    }
    object IdAccess extends ObjDataId[Implication.type, Implication, Id] {
      override def id(d: Implication) = d.id
      override val unapplyData: AnyRef => Option[Implication] = {case r: Implication => Some(r); case _ => None}
    }
    implicit val equality: UnivEq[Implication] = deriveUnivEq
  }

  // -------------------------------------------------------------------------------------------------------------------

  val independentName = Optional[CustomField, String](_.independentName)(n => {
    case Text(a, _, b, c, d, e) => Text(a, n, b, c, d, e)
    case f: Tag                 => f
    case f: Implication         => f
  })

  val key = Optional[CustomField, FieldRefKey](_.keyO)(n => {
    case Text(a, b, _, c, d, e) => Text(a, b, n, c, d, e)
    case f: Tag                 => f
    case f: Implication         => f
  })

  def mandatory = Lens[CustomField, Mandatory](_.mandatory)(n => {
    case f: Text        => f.copy(mandatory = n)
    case f: Tag         => f.copy(mandatory = n)
    case f: Implication => f.copy(mandatory = n)
  })

  def live = Lens[CustomField, Live](_.live)(n => {
    case f: Text        => f.copy(live = n)
    case f: Tag         => f.copy(live = n)
    case f: Implication => f.copy(live = n)
  })

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree): CustomField => Must[String] = {
    case f: Text        => f.name
    case f: Tag         => f.name(tags)
    case f: Implication => f.name(customReqTypes)
  }

  def nameP(p: Project) = name(p.config.customReqTypes.data, p.config.tags.data)

  implicit val equalImplication: UnivEq[Implication] = deriveUnivEq
  implicit val equalTag        : UnivEq[Tag]         = deriveUnivEq
  implicit val equalText       : UnivEq[Text]        = deriveUnivEq
  implicit val equality        : UnivEq[CustomField] = deriveUnivEq
}

// =====================================================================================================================
// Set

/**
 * @param order Can include dead custom-fields.
 */
@Lenses
case class FieldSet(customFields: FieldSet.CustomFields,
                    order       : FieldSet.Order) {

  lazy val fields: Must[Vector[Field]] =
    Traverse[Vector].traverseImpl(order) {
      case  f: StaticField   => f
      case id: CustomFieldId => customFields(id)
    }
}

object FieldSet {
  // TODO FieldSet.Order should be NonEmptyVector.
  type Order = Vector[FieldId]
  type CustomFields = IMap[CustomFieldId, CustomField]
  def emptyCustomFields: CustomFields = IMap.empty(_.id)

  implicit val equality: Equal[FieldSet] = deriveEqual
}