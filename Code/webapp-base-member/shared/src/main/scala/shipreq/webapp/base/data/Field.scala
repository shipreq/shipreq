package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty._
import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle._
import monocle.macros.{GenLens, Lenses}
import scala.collection.immutable.ListSet
import scalaz.{-\/, Equal, \/}
import scalaz.std.option.toRight
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.util.Must._
import shipreq.webapp.base.WebappConfig
import IndexLabel._
import TaggedTypes.TaggedInt

// =====================================================================================================================
// Types

sealed abstract class FieldType(val name: String)
sealed abstract class StaticFieldType(name: String) extends FieldType(name)
sealed abstract class CustomFieldType(name: String) extends FieldType(name)

object StaticFieldType {
  case object UseCaseSteps     extends StaticFieldType("Use Case Steps")
  case object UseCaseStepGraph extends StaticFieldType("Use Case Step Graph")
  case object ImplicationGraph extends StaticFieldType("Implication Graph")

  val values: NonEmptyVector[StaticFieldType] =
    AdtMacros.adtValues[StaticFieldType]

  implicit def equality: UnivEq[StaticFieldType] = UnivEq.derive
}

object CustomFieldType {
  case object Implication extends CustomFieldType("Implication")
  case object Tag         extends CustomFieldType("Tag")
  case object Text        extends CustomFieldType("Text")

  val values: NonEmptyVector[CustomFieldType] =
    AdtMacros.adtValues[CustomFieldType]

  implicit def equality: UnivEq[CustomFieldType] = UnivEq.derive
}

object FieldType {
  val values: NonEmptyVector[FieldType] =
    StaticFieldType.values ++ CustomFieldType.values

  implicit def equality: UnivEq[FieldType] = UnivEq.derive
}

// =====================================================================================================================
// Instances

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
  def fieldType        : FieldType
  def fieldReqTypeRules: FieldReqTypeRules[Any]

  def live(cfg: ProjectConfig): Live

  /** Independent as opposed to the name being derived from some external state. */
  def independentName: Option[String]

  def fold[A](s: StaticField => A, c: CustomField => A): A

  final def fieldId: FieldId =
    fold(s => s, _.id)
}

object Field {
  implicit lazy val applicableReqTypesEquality: UnivEq[ApplicableReqTypes] = implicitly
}

sealed abstract class StaticField(val name                      : String,
                                  override val fieldType        : StaticFieldType,
                                  override val fieldReqTypeRules: FieldReqTypeRules[Nothing],
                                  val deletable                 : Deletable) extends Field with FieldId {

  override final def live(cfg: ProjectConfig) = Live

  override final def independentName = Some(name)

  override final def fold  [A](s: StaticField => A, c: CustomField   => A): A = s(this)
  override final def foldId[A](s: StaticField => A, c: CustomFieldId => A): A = s(this)
}

sealed trait UseCaseStepLabelFmt
object UseCaseStepLabelFmt {
  case object `UC-N.m` extends UseCaseStepLabelFmt
  case object    `N.m` extends UseCaseStepLabelFmt
  case object     `.m` extends UseCaseStepLabelFmt
}

object StaticField {
  val useCaseOptionalOnly: FieldReqTypeRules[Nothing] =
    FieldReqTypeRules.only(StaticReqType.UseCase, FieldReqTypeRules.Resolution.Optional)

  @inline final private[this] def T = StaticFieldType

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class UseCaseStepTree(_name     : String,
                                        _fieldType: StaticFieldType,
                                        _reqTypes : FieldReqTypeRules[Nothing],
                                        _deletable: Deletable)
      extends StaticField(_name, _fieldType, _reqTypes, _deletable) {

    val treeFilterAll: UseCaseSteps.Tree => Range
    val useCaseSteps: Lens[UseCase, UseCaseSteps]
    val useCaseStepTree: Lens[UseCase, UseCaseSteps.Tree] // Has to be lazy to be implemented here. No.

    final def stepLabel(ucNumber: ReqTypePos,
                        loc     : VectorTree.PartialLocation,
                        fmt     : UseCaseStepLabelFmt): String =
      Util.quickJSB { sb =>
        import UseCaseStepLabelFmt._
        @inline def sep = '.'

        fmt match {
          case `UC-N.m` =>
            sb append StaticReqType.UseCase.mnemonic.value
            sb append '-'
            sb append ucNumber.value
          case `N.m` =>
            sb append ucNumber.value
          case `.m` =>
        }

        for (p <- stepLabelPrefix) {
          sb append sep
          sb append p
        }
        var level = 0
        loc.value.foreach { index =>
          sb append sep
          if (index < 0)
            sb append WebappConfig.useCaseStepsDeadNode
          else {
            sb append stepLabelsPerLevel(level).label(index)
            level += 1
          }
        }
      }

    def stepLabelPrefix: Option[String]

    val stepLabelsPerLevel: Vector[IndexLabel]

    /**
      * Maximum number of levels (inclusive) where the root (no steps) is 0.
      */
    final def maxDepth = stepLabelsPerLevel.length

    @inline final def canShiftLeft(loc: VectorTree.Location): Permission =
      VectorTree.canShiftLeft(loc)

    final def canShiftRight(loc         : VectorTree.Location,
                            f           : VectorTree.Location => Validity,
                            maxDepthTree: VectorTree[Int]): Permission =
      VectorTree.canShiftRightV(loc, f) && maxDepthTree.getAtLocation(loc).exists(_ + loc.length < maxDepth)

    def canDelete(loc: VectorTree.Location): Permission

    final def canInsertAfter(loc: VectorTree.Location): Permission =
      // TODO Add a real implementation and make tests generate tree at maxLength
      Allow
  }

  // UC-8.0.1.a.i.1
  // UC-8.E.1.a.i.1
  // ______|↑_↑_↑_↑
  private val sharedUseCaseStepLabels: Vector[IndexLabel] =
    Vector(NumericFrom1, Alpha, Roman, NumericFrom1)

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object NormalAltStepTree extends UseCaseStepTree(
      "Normal and Alternate Courses", T.UseCaseSteps, useCaseOptionalOnly, Deletable.Not) {

    override val useCaseSteps = GenLens[UseCase](_.stepsNA)
    override val useCaseStepTree = useCaseSteps ^|-> UseCaseSteps.tree

    // UC-8.0.1.a.i.1
    // ____|_________
    override def stepLabelPrefix: Option[String] =
      None

    // UC-8.0.1.a.i.1
    // ____|↑_+_+_+_+
    override val stepLabelsPerLevel =
      NumericFrom0 +: sharedUseCaseStepLabels

    override def canDelete(loc: VectorTree.Location) =
      Deny when (loc ==* VectorTree.root)

    val treeFilterN: UseCaseSteps.Tree => Range = Function.const(0 to 0)
    val treeFilterA: UseCaseSteps.Tree => Range = 1 until _.children.length

    override val treeFilterAll: UseCaseSteps.Tree => Range = _.children.indices
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object ExceptionStepTree extends UseCaseStepTree(
      "Exception Courses", T.UseCaseSteps, useCaseOptionalOnly, Deletable.Not) {

    override val useCaseSteps = GenLens[UseCase](_.stepsE)
    override val useCaseStepTree = useCaseSteps ^|-> UseCaseSteps.tree

    // UC-8.E.1.a.i.1
    // ____|↑|_______
    override val stepLabelPrefix: Option[String] =
      Some("E")

    // UC-8.E.1.a.i.1
    // ______|+_+_+_+
    override val stepLabelsPerLevel =
      sharedUseCaseStepLabels

    override def canDelete(loc: VectorTree.Location) =
      Allow

    val treeFilter: UseCaseSteps.Tree => Range = _.children.indices

    override val treeFilterAll = treeFilter
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object StepGraph extends StaticField(
    T.UseCaseStepGraph.name, T.UseCaseStepGraph, useCaseOptionalOnly, Deletable)

  case object ImplicationGraph extends StaticField(
    T.ImplicationGraph.name, T.ImplicationGraph, FieldReqTypeRules.optional, Deletable)

  // Non lazy causes utest to crash
  // ORDER MATTERS as this is the default order of fields use in new projects
  lazy val values: NonEmptyVector[StaticField] =
    AdtMacros.adtValuesManually[StaticField](
      ImplicationGraph,
      NormalAltStepTree,
      ExceptionStepTree,
      StepGraph)

  lazy val useCaseStepTrees: NonEmptyVector[UseCaseStepTree] =
    AdtMacros.adtValuesManually[UseCaseStepTree](NormalAltStepTree, ExceptionStepTree)

  lazy val (deletable, notDeletable) =
    values.whole.partition(_.deletable is Deletable)

  lazy val byName: Map[String, StaticField] =
    values.iterator.map(f => f.name -> f).toMap

  def names: Set[String] =
    byName.keySet

  lazy val namesLowercase: Set[String] =
    names.iterator.map(_.toLowerCase).toSet

  implicit def equality: UnivEq[StaticField] = UnivEq.derive

  implicit def useCaseStepTreeEquality: UnivEq[UseCaseStepTree] = UnivEq.derive
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
    CustomField.liveExplicitly.set(Live)(this).live(cfg) is Live

  override final def fold[A](s: StaticField => A, c: CustomField => A): A = c(this)
}

object CustomField {
  object IdAccess extends ObjDataId[CustomField.type, CustomField, CustomFieldId] {
    override def id(d: CustomField) = d.id
    override val unapplyData: AnyRef => Option[CustomField] = {case r: CustomField => Some(r); case _ => None}
  }

  def referencesCustomReqType(id: CustomReqTypeId): CustomField => Boolean = {
    case _: CustomField.Tag
       | _: CustomField.Text        => false
    case f: CustomField.Implication => f.reqTypeId.foldId(_ => false, _ ==* id)
  }

  // -------------------------------------------------------------------------------------------------------------------
  @Lenses
  final case class Text(id               : Text.Id,
                        name             : String,
                        fieldReqTypeRules: FieldReqTypeRules.ForTextField,
                        liveExplicitly   : Live) extends CustomField(CustomFieldType.Text) {
    override def toString = s"CustomField.Text($id, $name, $fieldReqTypeRules, $liveExplicitly)"
    override def independentName = Some(name)
    override def live(cfg: ProjectConfig) = liveExplicitly
  }

  object Text {

    def v1(id                : Text.Id,
           name              : String,
           key               : String,
           mandatory         : Mandatory,
           applicableReqTypes: ApplicableReqTypes,
           liveExplicitly    : Live): Text = {
      val _ = key
      apply(
        id                = id,
        name              = name,
        fieldReqTypeRules = FieldReqTypeRules.v1(mandatory, applicableReqTypes),
        liveExplicitly    = liveExplicitly,
      )
    }

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
  final case class Tag(id               : Tag.Id,
                       tagId            : TagId,
                       fieldReqTypeRules: FieldReqTypeRules.ForTagField,
                       liveExplicitly   : Live) extends CustomField(CustomFieldType.Tag) {

    override def toString = s"CustomField.Tag($id, $tagId, $fieldReqTypeRules, $liveExplicitly)"
    override def independentName = None

    def name(tags: TagTree): String =
      tags.need(tagId).tag.name

    override def live(cfg: ProjectConfig) =
      liveExplicitly & cfg.tags.live(tagId)
  }

  object Tag {

    def v1(id                : Tag.Id,
           tagId             : TagId,
           mandatory         : Mandatory,
           applicableReqTypes: ApplicableReqTypes,
           liveExplicitly    : Live): Tag = {
      apply(
        id                = id,
        tagId             = tagId,
        fieldReqTypeRules = FieldReqTypeRules.v1(mandatory, applicableReqTypes),
        liveExplicitly    = liveExplicitly,
      )
    }

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
  final case class Implication(id               : Implication.Id,
                               reqTypeId        : ReqTypeId,
                               fieldReqTypeRules: FieldReqTypeRules.ForImpField,
                               liveExplicitly   : Live) extends CustomField(CustomFieldType.Implication) {
    override def toString = s"CustomField.Implication($id, $reqTypeId, $fieldReqTypeRules, $liveExplicitly)"
    override def independentName = None

    def name(reqTypes: ReqTypes): String =
      ReqType.name(reqTypes)(reqTypeId)

    override def live(cfg: ProjectConfig) =
      liveExplicitly & cfg.live(reqTypeId)
  }

  object Implication {

    def v1(id                : Implication.Id,
           reqTypeId         : ReqTypeId,
           mandatory         : Mandatory,
           applicableReqTypes: ApplicableReqTypes,
           liveExplicitly    : Live): Implication = {
      apply(
        id                = id,
        reqTypeId         = reqTypeId,
        fieldReqTypeRules = FieldReqTypeRules.v1(mandatory, applicableReqTypes),
        liveExplicitly    = liveExplicitly,
      )
    }

    final case class Id(value: Int) extends CustomFieldId {
      override def toString = s"CustomField.Implication.Id($value)"
    }
    object IdAccess extends ObjDataId[Implication.type, Implication, Id] {
      override def id(d: Implication) = d.id
      override val unapplyData: AnyRef => Option[Implication] = {case r: Implication => Some(r); case _ => None}
    }

    // Implication fields always look backwards (i.e. refer to implication sources)
    def dir = Backwards
  }

  // ===================================================================================================================

  val independentName = Optional[CustomField, String](_.independentName)(n => {
    case Text(a, _, b, c) => Text(a, n, b, c)
    case f: Tag           => f
    case f: Implication   => f
  })

  /** HACK: Default type set to "Any" which is a lie. Safe unless you try to change defaults. */
  def fieldReqTypeRulesHack = Lens[CustomField, FieldReqTypeRules[Any]](_.fieldReqTypeRules)(n => {
    case f: Tag         => f.copy(fieldReqTypeRules = n.asInstanceOf[FieldReqTypeRules.ForTagField])
    case f: Text        => f.copy(fieldReqTypeRules = n.asInstanceOf[FieldReqTypeRules.ForTextField])
    case f: Implication => f.copy(fieldReqTypeRules = n.asInstanceOf[FieldReqTypeRules.ForImpField])
  })

  def liveExplicitly = Lens[CustomField, Live](_.liveExplicitly)(n => {
    case f: Text        => f.copy(liveExplicitly = n)
    case f: Tag         => f.copy(liveExplicitly = n)
    case f: Implication => f.copy(liveExplicitly = n)
  })

  implicit def equalImplication: UnivEq[Implication] = UnivEq.derive
  implicit def equalTag        : UnivEq[Tag]         = UnivEq.derive
  implicit def equalText       : UnivEq[Text]        = UnivEq.derive
  implicit def equality        : UnivEq[CustomField] = UnivEq.derive

  final class MutableLists {
    var imps = List.empty[CustomField.Implication]
    var tags = List.empty[CustomField.Tag]
    var text = List.empty[CustomField.Text]

    def isEmpty() = imps.isEmpty && tags.isEmpty && text.isEmpty

    def +=(cf: CustomField): Unit =
      cf match {
        case f: CustomField.Text        => text ::= f
        case f: CustomField.Tag         => tags ::= f
        case f: CustomField.Implication => imps ::= f
      }

    def result(): Lists =
      Lists(imps, tags, text)
  }

  final case class Lists(
    imps: List[CustomField.Implication],
    tags: List[CustomField.Tag],
    text: List[CustomField.Text],
  ) {
    def isEmpty = text.isEmpty && imps.isEmpty && tags.isEmpty
    def contains(id: CustomField.Text       .Id): Boolean = text.exists(_.id ==* id)
    def contains(id: CustomField.Tag        .Id): Boolean = tags.exists(_.id ==* id)
    def contains(id: CustomField.Implication.Id): Boolean = imps.exists(_.id ==* id)
  }
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
final case class FieldSet(customFields: FieldSet.CustomFields,
                          order       : FieldSet.Order) {

  def get(id: FieldId): Option[Field] =
    id match {
      case f : StaticField   => if (order contains f) Some(f) else None
      case id: CustomFieldId => customFields get id
    }

  def need(id: FieldId): Field =
    id match {
      case f : StaticField   => f
      case id: CustomFieldId => customFields need id
    }

  lazy val fields: Vector[Field] =
    order map {
      case f : StaticField   => f
      case id: CustomFieldId => customFields need id
    }

  def staticFieldIterator: Iterator[StaticField] =
    order.toIterator.filterSubType[StaticField]

  def staticFieldSet: ListSet[StaticField] =
    staticFieldIterator.to

  def custom[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): D = {
    val f = customFields.need(id)
    d.unapplyData(f) mustExistElse s"$id associated with wrong type: $f"
  }

  def customAttempt[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): String \/ D =
    customFields.get(id) match {
      case Some(f) =>
        toRight(d unapplyData f)(s"$id associated with wrong type: $f")
      case None =>
        -\/(s"$id not found.")
    }

  private lazy val splitFields = {
    val f = new CustomField.MutableLists
    customFields.valuesIterator.foreach(f.+=)
    f
  }

  def customImpFields: List[CustomField.Implication] =
    splitFields.imps

  def customTagFields: List[CustomField.Tag] =
    splitFields.tags

  def customTextFields: List[CustomField.Text] =
    splitFields.text
}

object FieldSet {
  val empty: FieldSet =
    FieldSet(emptyCustomFields, StaticField.values.whole)

  // TODO FieldSet.Order should be NonEmptyVector.
  type Order = Vector[FieldId]
  type CustomFields = IMap[CustomFieldId, CustomField]
  def emptyCustomFields: CustomFields = IMap.empty(_.id)

  def customFieldsTraversal: Traversal[CustomFields, CustomField] =
    IMap.traversal[CustomFieldId, CustomField]

  implicit def univEq: UnivEq[FieldSet] = UnivEq.derive
}