package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenser
import scalaz.{OneAnd, Equal}
import scalaz.Isomorphism._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import shapeless.TypeClass.deriveConstructors
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.IMap
import shipreq.base.util.TaggedTypes.TaggedLong

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
}
object Field {
  type ApplicableReqTypes = ISubset[Set, ReqType.Id]

  val useCaseOnly: ApplicableReqTypes = ISubset.Only(OneAnd(ReqType.UseCase, Set.empty))

  // type Id = Static \/ CustomField.Id
  sealed trait Id

  sealed abstract class Static(override val name     : String,
                               override val fieldType: FieldType,
                               override val reqTypes : Field.ApplicableReqTypes,
                                        val deletable: Deletable) extends Field with Id

  case object NormalAltStepTree extends Static("Normal and Alternate Courses", FieldType.StepTree,  useCaseOnly, Deletable.Not)
  case object ExceptionStepTree extends Static("Exception Courses",            FieldType.StepTree,  useCaseOnly, Deletable.Not)
  case object StepGraph         extends Static("Step Graph",                   FieldType.StepGraph, useCaseOnly, Deletable)

  // Not lazy causes crash in DataProp
  lazy val static: List[Static] =
    List(NormalAltStepTree, ExceptionStepTree, StepGraph)

  // Not lazy causes crash in tests
  implicit lazy val applicableReqTypesEquality: Equal[ApplicableReqTypes] = implicitly

  implicit val staticEquality = Equal.equalA[Static]
  implicit val idEquality     = Equal.equalA[Id]
}

import Field.ApplicableReqTypes

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

  val _alive = Lens((_: CustomField).alive)(n => {
    case t: Text => t.copy(alive = n)
  })

  case class Text(id       : Id,
                  name     : String,
                  key      : RefKey,
                  mandatory: Mandatory,
                  reqTypes : ApplicableReqTypes,
                  alive    : Alive) extends CustomField(FieldType.Text)

  implicit val textFieldEquality = deriveEqual[Text]

  implicit object Equality extends Equal[CustomField] {
    override def equal(a: CustomField, b: CustomField) = a match {
      case x: Text => b match {case y: Text => x ≟ y; case _ => false}
    }
  }
}

// =====================================================================================================================
// Set

case class FieldSet(customFields: IMap[CustomField.Id, CustomField],
                    order       : Vector[Field.Id])

object FieldSet {
  implicit val equality = deriveEqual[FieldSet]
}