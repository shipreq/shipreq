package shipreq.webapp.base.data

import monocle._
import monocle.macros.Lenser
import scalaz.{NonEmptyList, OneAnd, Equal}
import scalaz.Maybe.optionMaybeIso
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.{TaggedString, TaggedLong}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.delta.Partition

// =====================================================================================================================
// Types

sealed abstract class FieldType(val name: String)
sealed abstract class StaticFieldType(name: String) extends FieldType(name)
sealed abstract class CustomFieldType(name: String) extends FieldType(name)

object StaticFieldType {
  case object StepTree  extends StaticFieldType("Step Tree")
  case object StepGraph extends StaticFieldType("Step Graph")

  val values = NonEmptyList[StaticFieldType](
    StepTree,
    StepGraph)

  implicit val equality = Equal.equalA[StaticFieldType]
}

object CustomFieldType {
  case object Implication extends CustomFieldType("Implication")
  case object Tag         extends CustomFieldType("Tag")
  case object Text        extends CustomFieldType("Text")

  val values = NonEmptyList[CustomFieldType](
    Implication,
    Tag,
    Text)

  implicit val equality = Equal.equalA[CustomFieldType]
}

object FieldType {
  val values: NonEmptyList[FieldType] =
    StaticFieldType.values append CustomFieldType.values

  implicit val equality = Equal.equalA[FieldType]
}

// =====================================================================================================================
// Instances

/**
 * A key by which users can refer to a field.
 */
final case class FieldRefKey(value: String) extends TaggedString

sealed trait Mandatory
case object Mandatory extends Mandatory with (Boolean <=> Mandatory) {
  implicit val equality = Equal.equalA[Mandatory]
  override val from     = equality.equal(Mandatory, _: Mandatory)
  override val to       = if (_: Boolean) Mandatory else Not
  case object Not extends Mandatory
}

sealed trait Deletable
case object Deletable extends Deletable with (Boolean <=> Deletable) {
  implicit val equality = Equal.equalA[Deletable]
  override val from     = equality.equal(Deletable, _: Deletable)
  override val to       = if (_: Boolean) Deletable else Not
  case object Not extends Deletable
}

sealed trait Field {
  def fieldType: FieldType
  def reqTypes : Field.ApplicableReqTypes
  def keyO     : Option[FieldRefKey]
  def mandatory: Mandatory

  /** Independent as opposed to the name being derived from some external state. */
  def independentName: Option[String]

  def fold[A](s: StaticField => A, c: CustomField => A): A

  final def fieldId: Field.Id =
    fold(s => s, _.id)
}

object Field {
  type ApplicableReqTypes = ISubset[Set, ReqType.Id]

  // type Id = Static \/ CustomField.Id
  sealed trait Id {
    def foldId[A](s: StaticField => A, c: CustomField.Id => A): A
  }

  implicit lazy val applicableReqTypesEquality: Equal[ApplicableReqTypes] = implicitly

  implicit val idEquality = Equal.equalA[Id]

  val filterAlive: Field => Boolean =
    _.fold(_ => true, _.alive ≟ Alive)

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree) = {
    val cn: CustomField => String = CustomField.name(customReqTypes, tags)
    val fn: Field       => String = {
      case f: StaticField => f.name
      case f: CustomField => cn(f)
    }
    fn
  }

  def nameP(p: Project) = name(p.customReqTypes.data, p.tags.data)

  def nameAffectingPartitions: NonEmptyList[Partition] =
    NonEmptyList(Partition.CustomReqTypes, Partition.Tags)
}

import Field.ApplicableReqTypes

sealed abstract class StaticField(         val name     : String,
                                  override val fieldType: StaticFieldType,
                                  override val reqTypes : Field.ApplicableReqTypes,
                                  override val mandatory: Mandatory,
                                           val deletable: Deletable,
                                  override val keyO     : Option[FieldRefKey]) extends Field with Field.Id {

  override final def independentName = Some(name)

  override final def fold  [A](s: StaticField => A, c: CustomField    => A): A = s(this)
  override final def foldId[A](s: StaticField => A, c: CustomField.Id => A): A = s(this)
}

object StaticField {
  val useCaseOnly: ApplicableReqTypes = ISubset.Only(OneAnd(StaticReqType.UseCase, Set.empty))

  @inline final private[this] def T = StaticFieldType

  case object NormalAltStepTree extends StaticField(
    "Normal and Alternate Courses", T.StepTree, useCaseOnly, Mandatory.Not, Deletable.Not, None)

  case object ExceptionStepTree extends StaticField(
    "Exception Courses", T.StepTree, useCaseOnly, Mandatory.Not, Deletable.Not, None)

  case object StepGraph extends StaticField(
    "Step Graph", T.StepGraph, useCaseOnly, Mandatory.Not, Deletable, None)

  // Non lazy causes utest to crash
  lazy val values: NonEmptyList[StaticField] =
    NonEmptyList(NormalAltStepTree, ExceptionStepTree, StepGraph)

  lazy val (deletable, notDeletable) =
    values.list.partition(_.deletable ≟ Deletable)

  lazy val names: Set[String] =
    values.list.map(_.name).toSet

  implicit val equality = Equal.equalA[StaticField]
}

/** Custom here just distinguishes user-defined fields from static fields. */
sealed abstract class CustomField(override final val fieldType: CustomFieldType) extends Field {
  def id: CustomField.Id
  def alive: Alive

  override final def fold[A](s: StaticField => A, c: CustomField => A): A = c(this)
}

object CustomField {
  final case class Id(value: Long) extends TaggedLong with Field.Id {
    def foldId[A](s: StaticField => A, c: CustomField.Id => A): A = c(this)
  }

  object IdAccess extends ObjDataIdM[CustomField.type, CustomField, Id] {
    override def id(d: CustomField) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(cf: CustomField, i: Id) = cf match {
      case f: Text        => f.copy(id = i)
      case f: Tag         => f.copy(id = i)
      case f: Implication => f.copy(id = i)
    }
  }

  case class Text(id       : Id,
                  name     : String,
                  key      : FieldRefKey,
                  mandatory: Mandatory,
                  reqTypes : ApplicableReqTypes,
                  alive    : Alive) extends CustomField(CustomFieldType.Text) {
    override def independentName = Some(name)
    override def keyO = Some(key)
  }

  import shipreq.webapp.base.data.Tag.{Id => TagId}
  case class Tag(id       : Id,
                 tagId    : TagId,
                 mandatory: Mandatory,
                 reqTypes : ApplicableReqTypes,
                 alive    : Alive) extends CustomField(CustomFieldType.Tag) {
    override def independentName = None
    override def keyO = None

    def name(tags: TagTree): String =
      tags.get(tagId).fold(UiText.entityNameNotFound)(_.tag.name)
  }

  case class Implication(id       : Id,
                         reqTypeId: ReqType.Id,
                         mandatory: Mandatory,
                         reqTypes : ApplicableReqTypes,
                         alive    : Alive) extends CustomField(CustomFieldType.Implication) {
    override def independentName = None
    override def keyO = None

    def name(customReqTypes: CustomReqTypeIMap): String =
      ReqType.name(customReqTypes)(reqTypeId)
  }

  val _independentName = Optional[CustomField, String](optionMaybeIso to _.independentName)(n => {
    case Text(a, _, b, c, d, e) => Text(a, n, b, c, d, e)
    case f: Tag                 => f
    case f: Implication         => f
  })

  val _key = Optional[CustomField, FieldRefKey](optionMaybeIso to _.keyO)(n => {
    case Text(a, b, _, c, d, e) => Text(a, b, n, c, d, e)
    case f: Tag                 => f
    case f: Implication         => f
  })

  val _mandatory = Lens[CustomField, Mandatory](_.mandatory)(n => {
    case f: Text        => f.copy(mandatory = n)
    case f: Tag         => f.copy(mandatory = n)
    case f: Implication => f.copy(mandatory = n)
  })

  val _alive = Lens[CustomField, Alive](_.alive)(n => {
    case f: Text        => f.copy(alive = n)
    case f: Tag         => f.copy(alive = n)
    case f: Implication => f.copy(alive = n)
  })

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree): CustomField => String = {
    case f: Text        => f.name
    case f: Tag         => f.name(tags)
    case f: Implication => f.name(customReqTypes)
  }

  def nameP(p: Project) = name(p.customReqTypes.data, p.tags.data)

  implicit val equalImplication = deriveEqual[Implication]
  implicit val equalTag         = deriveEqual[Tag]
  implicit val equalText        = deriveEqual[Text]

  implicit object Equality extends Equal[CustomField] {
    override def equal(a: CustomField, b: CustomField) = a match {
      case x: Text        => b match {case y: Text        => x ≟ y; case _ => false}
      case x: Tag         => b match {case y: Tag         => x ≟ y; case _ => false}
      case x: Implication => b match {case y: Implication => x ≟ y; case _ => false}
    }
  }
}

// =====================================================================================================================
// Set

case class FieldSet(customFields: IMap[CustomField.Id, CustomField],
                    order       : Vector[Field.Id]) {

  def fields: Vector[Field] =
    order.map {
      case  f: StaticField    => f
      case id: CustomField.Id => customFields.get(id).get
    }
}

object FieldSet {
  implicit val equality = deriveEqual[FieldSet]
}