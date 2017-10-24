package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import scalaz.{Equal, Order}
import shipreq.base.util.{SetDiff, Util}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.util._

// DO NOT EDIT THIS
// This file is generated in its entirety by bin/gen-generic_data

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ApplicableTagGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Children extends Attr {
    override type Data = TagInTree.Children
    override def apply(data: Data) = ValueForChildren(data)
    val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Children]]
  }
  final case class ValueForChildren(value: Children.Data) extends Value {
    override val attr: Children.type = Children
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForChildren => Children.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Desc extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDesc(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
  }
  final case class ValueForDesc(value: Desc.Data) extends Value {
    override val attr: Desc.type = Desc
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForDesc => Desc.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Key extends Attr {
    override type Data = HashRefKey
    override def apply(data: Data) = ValueForKey(data)
    val dataEquality: Equal[Data] = implicitly[Equal[HashRefKey]]
  }
  final case class ValueForKey(value: Key.Data) extends Value {
    override val attr: Key.type = Key
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Parents extends Attr {
    override type Data = TagInTree.Parents
    override def apply(data: Data) = ValueForParents(data)
    val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Parents]]
  }
  final case class ValueForParents(value: Parents.Data) extends Value {
    override val attr: Parents.type = Parents
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForParents => Parents.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Children, Desc, Key, Name, Parents))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Children, Desc, Key, Name, Parents)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CodeGroupGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Code extends Attr {
    override type Data = ReqCode.Value
    override def apply(data: Data) = ValueForCode(data)
    val dataEquality: Equal[Data] = implicitly[Equal[ReqCode.Value]]
  }
  final case class ValueForCode(value: Code.Data) extends Value {
    override val attr: Code.type = Code
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForCode => Code.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Title extends Attr {
    override type Data = Text.CodeGroupTitle.OptionalText
    override def apply(data: Data) = ValueForTitle(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Text.CodeGroupTitle.OptionalText]]
  }
  final case class ValueForTitle(value: Title.Data) extends Value {
    override val attr: Title.type = Title
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTitle => Title.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Code, Title))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Code, Title)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomImpFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Mandatory extends Attr {
    override type Data = Mandatory
    override def apply(data: Data) = ValueForMandatory(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
  }
  final case class ValueForMandatory(value: Mandatory.Data) extends Value {
    override val attr: Mandatory.type = Mandatory
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ReqTypeId extends Attr {
    override type Data = ReqTypeId
    override def apply(data: Data) = ValueForReqTypeId(data)
    val dataEquality: Equal[Data] = implicitly[Equal[ReqTypeId]]
  }
  final case class ValueForReqTypeId(value: ReqTypeId.Data) extends Value {
    override val attr: ReqTypeId.type = ReqTypeId
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForReqTypeId => ReqTypeId.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ReqTypes extends Attr {
    override type Data = Field.ApplicableReqTypes
    override def apply(data: Data) = ValueForReqTypes(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Field.ApplicableReqTypes]]
  }
  final case class ValueForReqTypes(value: ReqTypes.Data) extends Value {
    override val attr: ReqTypes.type = ReqTypes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForReqTypes => ReqTypes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Mandatory, ReqTypeId, ReqTypes))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Mandatory, ReqTypeId, ReqTypes)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomIssueTypeGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Desc extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDesc(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
  }
  final case class ValueForDesc(value: Desc.Data) extends Value {
    override val attr: Desc.type = Desc
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForDesc => Desc.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Key extends Attr {
    override type Data = HashRefKey
    override def apply(data: Data) = ValueForKey(data)
    val dataEquality: Equal[Data] = implicitly[Equal[HashRefKey]]
  }
  final case class ValueForKey(value: Key.Data) extends Value {
    override val attr: Key.type = Key
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Desc, Key))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Desc, Key)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomReqTypeGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Imp extends Attr {
    override type Data = ImplicationRequired
    override def apply(data: Data) = ValueForImp(data)
    val dataEquality: Equal[Data] = implicitly[Equal[ImplicationRequired]]
  }
  final case class ValueForImp(value: Imp.Data) extends Value {
    override val attr: Imp.type = Imp
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImp => Imp.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Mnemonic extends Attr {
    override type Data = ReqType.Mnemonic
    override def apply(data: Data) = ValueForMnemonic(data)
    val dataEquality: Equal[Data] = implicitly[Equal[ReqType.Mnemonic]]
  }
  final case class ValueForMnemonic(value: Mnemonic.Data) extends Value {
    override val attr: Mnemonic.type = Mnemonic
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForMnemonic => Mnemonic.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Imp, Mnemonic, Name))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Imp, Mnemonic, Name)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomTagFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Mandatory extends Attr {
    override type Data = Mandatory
    override def apply(data: Data) = ValueForMandatory(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
  }
  final case class ValueForMandatory(value: Mandatory.Data) extends Value {
    override val attr: Mandatory.type = Mandatory
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ReqTypes extends Attr {
    override type Data = Field.ApplicableReqTypes
    override def apply(data: Data) = ValueForReqTypes(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Field.ApplicableReqTypes]]
  }
  final case class ValueForReqTypes(value: ReqTypes.Data) extends Value {
    override val attr: ReqTypes.type = ReqTypes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForReqTypes => ReqTypes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object TagId extends Attr {
    override type Data = TagId
    override def apply(data: Data) = ValueForTagId(data)
    val dataEquality: Equal[Data] = implicitly[Equal[TagId]]
  }
  final case class ValueForTagId(value: TagId.Data) extends Value {
    override val attr: TagId.type = TagId
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTagId => TagId.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Mandatory, ReqTypes, TagId))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Mandatory, ReqTypes, TagId)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomTextFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Key extends Attr {
    override type Data = FieldRefKey
    override def apply(data: Data) = ValueForKey(data)
    val dataEquality: Equal[Data] = implicitly[Equal[FieldRefKey]]
  }
  final case class ValueForKey(value: Key.Data) extends Value {
    override val attr: Key.type = Key
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Mandatory extends Attr {
    override type Data = Mandatory
    override def apply(data: Data) = ValueForMandatory(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
  }
  final case class ValueForMandatory(value: Mandatory.Data) extends Value {
    override val attr: Mandatory.type = Mandatory
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ReqTypes extends Attr {
    override type Data = Field.ApplicableReqTypes
    override def apply(data: Data) = ValueForReqTypes(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Field.ApplicableReqTypes]]
  }
  final case class ValueForReqTypes(value: ReqTypes.Data) extends Value {
    override val attr: ReqTypes.type = ReqTypes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForReqTypes => ReqTypes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Key, Mandatory, Name, ReqTypes))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Key, Mandatory, Name, ReqTypes)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/**
 * This is only used in [[GenericReqCreate]].
 * All fields are optional; any mandatory data is required in the [[GenericReqCreate]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
object GenericReqGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Codes extends Attr {
    override type Data = NonEmptySet[ReqCode.IdAndValue]
    override def apply(data: Data) = ValueForCodes(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqCode.IdAndValue]]]
  }
  final case class ValueForCodes(value: Codes.Data) extends Value {
    override val attr: Codes.type = Codes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForCodes => Codes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object CustomText extends Attr {
    override type Data = Event.NonEmptyCustomTextMap
    override def apply(data: Data) = ValueForCustomText(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Event.NonEmptyCustomTextMap]]
  }
  final case class ValueForCustomText(value: CustomText.Data) extends Value {
    override val attr: CustomText.type = CustomText
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForCustomText => CustomText.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ImpSrcs extends Attr {
    override type Data = NonEmptySet[ReqId]
    override def apply(data: Data) = ValueForImpSrcs(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
  }
  final case class ValueForImpSrcs(value: ImpSrcs.Data) extends Value {
    override val attr: ImpSrcs.type = ImpSrcs
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImpSrcs => ImpSrcs.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ImpTgts extends Attr {
    override type Data = NonEmptySet[ReqId]
    override def apply(data: Data) = ValueForImpTgts(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
  }
  final case class ValueForImpTgts(value: ImpTgts.Data) extends Value {
    override val attr: ImpTgts.type = ImpTgts
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImpTgts => ImpTgts.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Tags extends Attr {
    override type Data = NonEmptySet[ApplicableTagId]
    override def apply(data: Data) = ValueForTags(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApplicableTagId]]]
  }
  final case class ValueForTags(value: Tags.Data) extends Value {
    override val attr: Tags.type = Tags
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTags => Tags.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Title extends Attr {
    override type Data = Text.GenericReqTitle.NonEmptyText
    override def apply(data: Data) = ValueForTitle(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Text.GenericReqTitle.NonEmptyText]]
  }
  final case class ValueForTitle(value: Title.Data) extends Value {
    override val attr: Title.type = Title
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTitle => Title.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Codes, CustomText, ImpSrcs, ImpTgts, Tags, Title))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Codes, CustomText, ImpSrcs, ImpTgts, Tags, Title)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object SavedViewGD extends GenericData {
  import shipreq.webapp.base.filter.Filter.{Valid => ValidFilter}
  import reqtable._

  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Columns extends Attr {
    override type Data = NonEmptyVector[Column]
    override def apply(data: Data) = ValueForColumns(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptyVector[Column]]]
  }
  final case class ValueForColumns(value: Columns.Data) extends Value {
    override val attr: Columns.type = Columns
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForColumns => Columns.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Filter extends Attr {
    override type Data = Option[ValidFilter]
    override def apply(data: Data) = ValueForFilter(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Option[ValidFilter]]]
  }
  final case class ValueForFilter(value: Filter.Data) extends Value {
    override val attr: Filter.type = Filter
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFilter => Filter.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object FilterDead extends Attr {
    override type Data = FilterDead
    override def apply(data: Data) = ValueForFilterDead(data)
    val dataEquality: Equal[Data] = implicitly[Equal[FilterDead]]
  }
  final case class ValueForFilterDead(value: FilterDead.Data) extends Value {
    override val attr: FilterDead.type = FilterDead
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFilterDead => FilterDead.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = SavedView.Name
    override def apply(data: Data) = ValueForName(data)
    val dataEquality: Equal[Data] = implicitly[Equal[SavedView.Name]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object SortCriteria extends Attr {
    override type Data = SortCriteria
    override def apply(data: Data) = ValueForSortCriteria(data)
    val dataEquality: Equal[Data] = implicitly[Equal[SortCriteria]]
  }
  final case class ValueForSortCriteria(value: SortCriteria.Data) extends Value {
    override val attr: SortCriteria.type = SortCriteria
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForSortCriteria => SortCriteria.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Columns, Filter, FilterDead, Name, SortCriteria))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Columns, Filter, FilterDead, Name, SortCriteria)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object TagGroupGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Children extends Attr {
    override type Data = TagInTree.Children
    override def apply(data: Data) = ValueForChildren(data)
    val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Children]]
  }
  final case class ValueForChildren(value: Children.Data) extends Value {
    override val attr: Children.type = Children
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForChildren => Children.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Desc extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDesc(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
  }
  final case class ValueForDesc(value: Desc.Data) extends Value {
    override val attr: Desc.type = Desc
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForDesc => Desc.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object MutexChildren extends Attr {
    override type Data = MutexChildren
    override def apply(data: Data) = ValueForMutexChildren(data)
    val dataEquality: Equal[Data] = implicitly[Equal[MutexChildren]]
  }
  final case class ValueForMutexChildren(value: MutexChildren.Data) extends Value {
    override val attr: MutexChildren.type = MutexChildren
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForMutexChildren => MutexChildren.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Parents extends Attr {
    override type Data = TagInTree.Parents
    override def apply(data: Data) = ValueForParents(data)
    val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Parents]]
  }
  final case class ValueForParents(value: Parents.Data) extends Value {
    override val attr: Parents.type = Parents
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForParents => Parents.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Children, Desc, MutexChildren, Name, Parents))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Children, Desc, MutexChildren, Name, Parents)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/**
 * This is only used in [[UseCaseCreate]].
 * All fields are optional; any mandatory data is required in the [[UseCaseCreate]] constructor directly. Thus,
 * when a field is provided, its value must be non-empty (with emptiness representable by omitting the field).
 */
object UseCaseGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Codes extends Attr {
    override type Data = NonEmptySet[ReqCode.IdAndValue]
    override def apply(data: Data) = ValueForCodes(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqCode.IdAndValue]]]
  }
  final case class ValueForCodes(value: Codes.Data) extends Value {
    override val attr: Codes.type = Codes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForCodes => Codes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object CustomText extends Attr {
    override type Data = Event.NonEmptyCustomTextMap
    override def apply(data: Data) = ValueForCustomText(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Event.NonEmptyCustomTextMap]]
  }
  final case class ValueForCustomText(value: CustomText.Data) extends Value {
    override val attr: CustomText.type = CustomText
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForCustomText => CustomText.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ImpSrcs extends Attr {
    override type Data = NonEmptySet[ReqId]
    override def apply(data: Data) = ValueForImpSrcs(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
  }
  final case class ValueForImpSrcs(value: ImpSrcs.Data) extends Value {
    override val attr: ImpSrcs.type = ImpSrcs
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImpSrcs => ImpSrcs.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object ImpTgts extends Attr {
    override type Data = NonEmptySet[ReqId]
    override def apply(data: Data) = ValueForImpTgts(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
  }
  final case class ValueForImpTgts(value: ImpTgts.Data) extends Value {
    override val attr: ImpTgts.type = ImpTgts
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImpTgts => ImpTgts.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Tags extends Attr {
    override type Data = NonEmptySet[ApplicableTagId]
    override def apply(data: Data) = ValueForTags(data)
    val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApplicableTagId]]]
  }
  final case class ValueForTags(value: Tags.Data) extends Value {
    override val attr: Tags.type = Tags
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTags => Tags.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Title extends Attr {
    override type Data = Text.UseCaseTitle.NonEmptyText
    override def apply(data: Data) = ValueForTitle(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Text.UseCaseTitle.NonEmptyText]]
  }
  final case class ValueForTitle(value: Title.Data) extends Value {
    override val attr: Title.type = Title
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTitle => Title.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Codes, CustomText, ImpSrcs, ImpTgts, Tags, Title))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Codes, CustomText, ImpSrcs, ImpTgts, Tags, Title)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object UseCaseStepGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object FlowIn extends Attr {
    override type Data = SetDiff.NE[UseCaseStepId]
    override def apply(data: Data) = ValueForFlowIn(data)
    val dataEquality: Equal[Data] = implicitly[Equal[SetDiff.NE[UseCaseStepId]]]
  }
  final case class ValueForFlowIn(value: FlowIn.Data) extends Value {
    override val attr: FlowIn.type = FlowIn
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFlowIn => FlowIn.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object FlowOut extends Attr {
    override type Data = SetDiff.NE[UseCaseStepId]
    override def apply(data: Data) = ValueForFlowOut(data)
    val dataEquality: Equal[Data] = implicitly[Equal[SetDiff.NE[UseCaseStepId]]]
  }
  final case class ValueForFlowOut(value: FlowOut.Data) extends Value {
    override val attr: FlowOut.type = FlowOut
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFlowOut => FlowOut.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Title extends Attr {
    override type Data = Text.UseCaseStep.OptionalText
    override def apply(data: Data) = ValueForTitle(data)
    val dataEquality: Equal[Data] = implicitly[Equal[Text.UseCaseStep.OptionalText]]
  }
  final case class ValueForTitle(value: Title.Data) extends Value {
    override val attr: Title.type = Title
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForTitle => Title.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(FlowIn, FlowOut, Title))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](FlowIn, FlowOut, Title)
}