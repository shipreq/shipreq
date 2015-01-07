package shipreq.webapp.base.data

import monocle._
import monocle.macros.Lenser
import scalaz.{NonEmptyList, OneAnd, Equal, Maybe}
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.{TaggedString, TaggedLong}

// =====================================================================================================================
// Types

sealed trait FieldType
object FieldType {
  case object Text      extends FieldType
  case object StepTree  extends FieldType
  case object StepGraph extends FieldType

  implicit val equality = Equal.equalA[FieldType]

  val name: FieldType => String = {
    case Text      => "Text"
    case StepTree  => "Step Tree"
    case StepGraph => "Step Graph"
  }

  // allowNew: FieldType => FieldSet => Boolean
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
  def name     : String
  def fieldType: FieldType
  def reqTypes : Field.ApplicableReqTypes
  def keyO     : Option[FieldRefKey]
}

object Field {
  type ApplicableReqTypes = ISubset[Set, ReqType.Id]

  // type Id = Static \/ CustomField.Id
  sealed trait Id

  implicit val applicableReqTypesEquality: Equal[ApplicableReqTypes] = implicitly

  implicit val idEquality = Equal.equalA[Id]
}

import Field.ApplicableReqTypes

sealed abstract class StaticField(override val name     : String,
                                  override val fieldType: FieldType,
                                  override val reqTypes : Field.ApplicableReqTypes,
                                  override val keyO     : Option[FieldRefKey],
                                           val deletable: Deletable) extends Field with Field.Id

object StaticField {
  val useCaseOnly: ApplicableReqTypes = ISubset.Only(OneAnd(ReqType.UseCase, Set.empty))

  case object NormalAltStepTree extends StaticField("Normal and Alternate Courses", FieldType.StepTree,  useCaseOnly, None, Deletable.Not)
  case object ExceptionStepTree extends StaticField("Exception Courses",            FieldType.StepTree,  useCaseOnly, None, Deletable.Not)
  case object StepGraph         extends StaticField("Step Graph",                   FieldType.StepGraph, useCaseOnly, None, Deletable)

  // Non lazy causes utest to crash
  lazy val values: NonEmptyList[StaticField] =
    NonEmptyList(NormalAltStepTree, ExceptionStepTree, StepGraph)

  // Non lazy causes utest to crash
  lazy val (deletable, notDeletable) =
    values.list.partition(_.deletable ≟ Deletable)

  implicit val equality = Equal.equalA[StaticField]
}

/** Custom here just distinguishes user-defined fields from static fields. */
sealed abstract class CustomField(override final val fieldType: FieldType) extends Field {
  def id: CustomField.Id
  def alive: Alive
}

object CustomField {
  final case class Id(value: Long) extends TaggedLong with Field.Id

  object IdAccess extends ObjDataIdM[CustomField.type, CustomField, Id] {
    override def id(d: CustomField) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(cf: CustomField, i: Id) = cf match {
      case f: Text => f.copy(id = i)
    }
  }

  case class Text(id       : Id,
                  name     : String,
                  key      : FieldRefKey,
                  mandatory: Mandatory,
                  reqTypes : ApplicableReqTypes,
                  alive    : Alive) extends CustomField(FieldType.Text) {
    override def keyO = Some(key)
  }

  object Text {
    implicit val equality = deriveEqual[Text]
  }

  val _name = Lens[CustomField, String](_.name)(n => {
    case Text(a, _, b, c, d, e) => Text(a, n, b, c, d, e)
  })

  val _key = Optional[CustomField, FieldRefKey](f => Maybe.optionMaybeIso.to(f.keyO))(n => {
    case Text(a, b, _, c, d, e) => Text(a, b, n, c, d, e)
  })

  val _alive = Lens[CustomField, Alive](_.alive)(n => {
    case t: Text => t.copy(alive = n)
  })

  implicit object Equality extends Equal[CustomField] {
    override def equal(a: CustomField, b: CustomField) = a match {
      case x: Text => b match {case y: Text => x ≟ y; case _ => false}
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