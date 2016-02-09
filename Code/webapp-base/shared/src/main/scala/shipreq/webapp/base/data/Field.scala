package shipreq.webapp.base.data

import monocle._
import monocle.macros.Lenses
import scala.collection.immutable.ListSet
import scalaz.Equal
import shipreq.base.util._
import ScalaExt._
import TaggedTypes.{TaggedString, TaggedInt}
import UnivEq.Implicits._

// =====================================================================================================================
// Types

sealed abstract class FieldType(val name: String)
sealed abstract class StaticFieldType(name: String) extends FieldType(name)
sealed abstract class CustomFieldType(name: String) extends FieldType(name)

object StaticFieldType {
  case object StepTree  extends StaticFieldType("Step Tree")
  case object StepGraph extends StaticFieldType("Step Graph")

  val values: NonEmptyVector[StaticFieldType] =
    UtilMacros.adtValues[StaticFieldType]

  implicit def equality: UnivEq[StaticFieldType] = UnivEq.derive
}

object CustomFieldType {
  case object Implication extends CustomFieldType("Implication")
  case object Tag         extends CustomFieldType("Tag")
  case object Text        extends CustomFieldType("Text")

  val values: NonEmptyVector[CustomFieldType] =
    UtilMacros.adtValues[CustomFieldType]

  implicit def equality: UnivEq[CustomFieldType] = UnivEq.derive
}

object FieldType {
  val values: NonEmptyVector[FieldType] =
    StaticFieldType.values ++ CustomFieldType.values

  implicit def equality: UnivEq[FieldType] = UnivEq.derive
}

// =====================================================================================================================
// Instances

/**
 * A key by which users can refer to a field.
 */
final case class FieldRefKey(value: String) extends TaggedString

sealed trait Mandatory extends IsoBool[Mandatory] {
  override final def companion = Mandatory
}
case object Mandatory extends Mandatory with IsoBool.Object[Mandatory] {
  override def positive = this
  override def negative = Not
  case object Not extends Mandatory
}

sealed trait Deletable extends IsoBool[Deletable] {
  override final def companion = Deletable
}
case object Deletable extends Deletable with IsoBool.Object[Deletable] {
  override def positive = this
  override def negative = Not
  case object Not extends Deletable
}

/** type [[FieldId]] = [[StaticField]] | [[CustomFieldId]] */
sealed trait FieldId {
  def foldId[A](s: StaticField => A, c: CustomFieldId => A): A
}

sealed trait Field {
  def fieldType: FieldType
  def reqTypes : Field.ApplicableReqTypes
  def keyO     : Option[FieldRefKey]
  def mandatory: Mandatory

  def live(cfg: ProjectConfig): Live

  /** Independent as opposed to the name being derived from some external state. */
  def independentName: Option[String]

  def fold[A](s: StaticField => A, c: CustomField => A): A

  final def fieldId: FieldId =
    fold(s => s, _.id)
}

object Field {
  type ApplicableReqTypes = ISubset[ReqTypeId]

  implicit lazy val applicableReqTypesEquality: UnivEq[ApplicableReqTypes] = implicitly

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree): Field => String = {
    val cn: CustomField => String = CustomField.name(customReqTypes, tags)
    val fn: Field       => String = {
      case f: StaticField => f.name
      case f: CustomField => cn(f)
    }
    fn
  }

  def nameP(p: Project): Field => String =
    name(p.config.customReqTypes, p.config.tags)
}

import Field.ApplicableReqTypes

sealed abstract class StaticField(         val name     : String,
                                  override val fieldType: StaticFieldType,
                                  override val reqTypes : Field.ApplicableReqTypes,
                                  override val mandatory: Mandatory,
                                           val deletable: Deletable,
                                  override val keyO     : Option[FieldRefKey]) extends Field with FieldId {

  override final def live(cfg: ProjectConfig) = Live

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
  // ORDER MATTERS as this is the default order of fields use in new projects
  lazy val values: NonEmptyVector[StaticField] =
    UtilMacros.adtValuesManual[StaticField](NormalAltStepTree, ExceptionStepTree, StepGraph)

  lazy val (deletable, notDeletable) =
    values.whole.partition(_.deletable :: Deletable)

  lazy val names: Set[String] =
    values.toStream.map(_.name).toSet

  implicit def equality: UnivEq[StaticField] = UnivEq.derive
}

sealed abstract class CustomFieldId extends TaggedInt with FieldId {
  final def foldId[A](s: StaticField => A, c: CustomFieldId => A): A = c(this)
}

/** Custom here just distinguishes user-defined fields from static fields. */
sealed abstract class CustomField(override final val fieldType: CustomFieldType) extends Field {
  def id: CustomFieldId

  /** Whether the user has explicitly marked this field as deleted or not. */
  val liveExplicitly: Live

  /**
   * If [[liveExplicitly]] was [[Live]], would the final live value be [[Live]] too.
   */
  final def recoverable(cfg: ProjectConfig): Boolean =
    CustomField.liveExplicitly.set(Live)(this).live(cfg) :: Live

  override final def fold[A](s: StaticField => A, c: CustomField => A): A = c(this)
}

object CustomField {
  object IdAccess extends ObjDataId[CustomField.type, CustomField, CustomFieldId] {
    override def id(d: CustomField) = d.id
    override val unapplyData: AnyRef => Option[CustomField] = {case r: CustomField => Some(r); case _ => None}
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Text(id            : Text.Id,
                  name          : String,
                  key           : FieldRefKey,
                  mandatory     : Mandatory,
                  reqTypes      : ApplicableReqTypes,
                  liveExplicitly: Live) extends CustomField(CustomFieldType.Text) {
    override def independentName = Some(name)
    override def keyO = Some(key)
    override def live(cfg: ProjectConfig) = liveExplicitly
  }
  object Text {
    final case class Id(value: Int) extends CustomFieldId {
      override def toString = s"CustomField.Text.Id($value)"
    }
    object IdAccess extends ObjDataId[Text.type, Text, Id] {
      override def id(d: Text) = d.id
      override val unapplyData: AnyRef => Option[Text] = {case r: Text => Some(r); case _ => None}
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Tag(id            : Tag.Id,
                 tagId         : TagId,
                 mandatory     : Mandatory,
                 reqTypes      : ApplicableReqTypes,
                 liveExplicitly: Live) extends CustomField(CustomFieldType.Tag) {
    override def independentName = None
    override def keyO = None

    def name(tags: TagTree): String =
      tags.need(tagId).tag.name

    override def live(cfg: ProjectConfig) =
      liveExplicitly && cfg.live(tagId)
  }
  object Tag {
    final case class Id(value: Int) extends CustomFieldId  {
      override def toString = s"CustomField.Tag.Id($value)"
    }
    object IdAccess extends ObjDataId[Tag.type, Tag, Id] {
      override def id(d: Tag) = d.id
      override val unapplyData: AnyRef => Option[Tag] = {case r: Tag => Some(r); case _ => None}
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  case class Implication(id            : Implication.Id,
                         reqTypeId     : ReqTypeId,
                         mandatory     : Mandatory,
                         reqTypes      : ApplicableReqTypes,
                         liveExplicitly: Live) extends CustomField(CustomFieldType.Implication) {
    override def independentName = None
    override def keyO = None

    def name(customReqTypes: CustomReqTypeIMap): String =
      ReqType.name(customReqTypes)(reqTypeId)

    override def live(cfg: ProjectConfig) =
      liveExplicitly && cfg.live(reqTypeId)
  }
  object Implication {
    final case class Id(value: Int) extends CustomFieldId {
      override def toString = s"CustomField.Implication.Id($value)"
    }
    object IdAccess extends ObjDataId[Implication.type, Implication, Id] {
      override def id(d: Implication) = d.id
      override val unapplyData: AnyRef => Option[Implication] = {case r: Implication => Some(r); case _ => None}
    }
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

  def liveExplicitly = Lens[CustomField, Live](_.liveExplicitly)(n => {
    case f: Text        => f.copy(liveExplicitly = n)
    case f: Tag         => f.copy(liveExplicitly = n)
    case f: Implication => f.copy(liveExplicitly = n)
  })

  def name(customReqTypes: CustomReqTypeIMap, tags: TagTree): CustomField => String = {
    case f: Text        => f.name
    case f: Tag         => f.name(tags)
    case f: Implication => f.name(customReqTypes)
  }

  def nameP(p: Project) = name(p.config.customReqTypes, p.config.tags)

  implicit def equalImplication: UnivEq[Implication] = UnivEq.derive
  implicit def equalTag        : UnivEq[Tag]         = UnivEq.derive
  implicit def equalText       : UnivEq[Text]        = UnivEq.derive
  implicit def equality        : UnivEq[CustomField] = UnivEq.derive
}

object FieldId {
  implicit def idEquality: UnivEq[FieldId] = UnivEq.derive
}

// =====================================================================================================================
// Set

/**
 * @param order Can include dead custom-fields.
 */
@Lenses
case class FieldSet(customFields: FieldSet.CustomFields,
                    order       : FieldSet.Order) {

  lazy val fields: Vector[Field] =
    order map {
      case f : StaticField   => f
      case id: CustomFieldId => customFields need id
    }

  def fieldsForReqType(reqTypeId: ReqTypeId): Vector[Field] =
    fields.filter(_.reqTypes.filter(reqTypeId))

  def staticFieldIterator: Iterator[StaticField] =
    order.filterTI[StaticField]

  def staticFieldSet: ListSet[StaticField] =
    staticFieldIterator.to
}

object FieldSet {
  // TODO FieldSet.Order should be NonEmptyVector.
  type Order = Vector[FieldId]
  type CustomFields = IMap[CustomFieldId, CustomField]
  def emptyCustomFields: CustomFields = IMap.empty(_.id)

  implicit val equality: Equal[FieldSet] = UtilMacros.deriveEqual
}