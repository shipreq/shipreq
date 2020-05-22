package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import scalaz.{Equal, Order}
import shipreq.base.util._
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

  case object ApplicableReqTypes extends Attr {
    override type Data = ApplicableReqTypes
    override def apply(data: Data) = ValueForApplicableReqTypes(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[ApplicableReqTypes]]
  }
  final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
    override val attr: ApplicableReqTypes.type = ApplicableReqTypes
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Children extends Attr {
    override type Data = TagInTree.Children
    override def apply(data: Data) = ValueForChildren(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Children]]
  }
  final case class ValueForChildren(value: Children.Data) extends Value {
    override val attr: Children.type = Children
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForChildren => Children.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Colour extends Attr {
    override type Data = Option[Colour]
    override def apply(data: Data) = ValueForColour(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[Colour]]]
  }
  final case class ValueForColour(value: Colour.Data) extends Value {
    override val attr: Colour.type = Colour
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForColour => Colour.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Desc extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDesc(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[HashRefKey]]
  }
  final case class ValueForKey(value: Key.Data) extends Value {
    override val attr: Key.type = Key
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Parents extends Attr {
    override type Data = TagInTree.Parents
    override def apply(data: Data) = ValueForParents(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Parents]]
  }
  final case class ValueForParents(value: Parents.Data) extends Value {
    override val attr: Parents.type = Parents
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForParents => Parents.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Children, Colour, Desc, Key, Parents))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Children, Colour, Desc, Key, Parents)

  def apply(key: HashRefKey, desc: Option[String], colour: Option[Colour], applicableReqTypes: ApplicableReqTypes, parents: TagInTree.Parents, children: TagInTree.Children): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForKey(key) + ValueForDesc(desc) + ValueForColour(colour) + ValueForApplicableReqTypes(applicableReqTypes) + ValueForParents(parents) + ValueForChildren(children))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CodeGroupGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Code extends Attr {
    override type Data = ReqCode.Value
    override def apply(data: Data) = ValueForCode(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[ReqCode.Value]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Text.CodeGroupTitle.OptionalText]]
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

  def apply(code: ReqCode.Value, title: Text.CodeGroupTitle.OptionalText): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForCode(code) + ValueForTitle(title))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomImpFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object FieldReqTypeRules extends Attr {
    override type Data = FieldReqTypeRules[Impossible]
    override def apply(data: Data) = ValueForFieldReqTypeRules(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[FieldReqTypeRules[Impossible]]]
  }
  final case class ValueForFieldReqTypeRules(value: FieldReqTypeRules.Data) extends Value {
    override val attr: FieldReqTypeRules.type = FieldReqTypeRules
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFieldReqTypeRules => FieldReqTypeRules.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(FieldReqTypeRules))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](FieldReqTypeRules)

  def apply(fieldReqTypeRules: FieldReqTypeRules[Impossible]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForFieldReqTypeRules(fieldReqTypeRules))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomIssueTypeGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Desc extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDesc(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[HashRefKey]]
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

  def apply(key: HashRefKey, desc: Option[String]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForKey(key) + ValueForDesc(desc))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomReqTypeGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Description extends Attr {
    override type Data = Option[String]
    override def apply(data: Data) = ValueForDescription(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
  }
  final case class ValueForDescription(value: Description.Data) extends Value {
    override val attr: Description.type = Description
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForDescription => Description.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Implication extends Attr {
    override type Data = Mandatory
    override def apply(data: Data) = ValueForImplication(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
  }
  final case class ValueForImplication(value: Implication.Data) extends Value {
    override val attr: Implication.type = Implication
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForImplication => Implication.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Mnemonic extends Attr {
    override type Data = ReqType.Mnemonic
    override def apply(data: Data) = ValueForMnemonic(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[ReqType.Mnemonic]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Description, Implication, Mnemonic, Name))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Description, Implication, Mnemonic, Name)

  def apply(mnemonic: ReqType.Mnemonic, name: String, implication: Mandatory, description: Option[String]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForMnemonic(mnemonic) + ValueForName(name) + ValueForImplication(implication) + ValueForDescription(description))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomTagFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object FieldReqTypeRules extends Attr {
    override type Data = FieldReqTypeRules[ApplicableTagId]
    override def apply(data: Data) = ValueForFieldReqTypeRules(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[FieldReqTypeRules[ApplicableTagId]]]
  }
  final case class ValueForFieldReqTypeRules(value: FieldReqTypeRules.Data) extends Value {
    override val attr: FieldReqTypeRules.type = FieldReqTypeRules
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFieldReqTypeRules => FieldReqTypeRules.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(FieldReqTypeRules))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](FieldReqTypeRules)

  def apply(fieldReqTypeRules: FieldReqTypeRules[ApplicableTagId]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForFieldReqTypeRules(fieldReqTypeRules))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object CustomTextFieldGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object FieldReqTypeRules extends Attr {
    override type Data = FieldReqTypeRules[Impossible]
    override def apply(data: Data) = ValueForFieldReqTypeRules(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[FieldReqTypeRules[Impossible]]]
  }
  final case class ValueForFieldReqTypeRules(value: FieldReqTypeRules.Data) extends Value {
    override val attr: FieldReqTypeRules.type = FieldReqTypeRules
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForFieldReqTypeRules => FieldReqTypeRules.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[String]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(FieldReqTypeRules, Name))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](FieldReqTypeRules, Name)

  def apply(name: String, fieldReqTypeRules: FieldReqTypeRules[Impossible]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForName(name) + ValueForFieldReqTypeRules(fieldReqTypeRules))
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
    override type Data = NonEmptySet[ApReqCodeId.AndValue]
    override def apply(data: Data) = ValueForCodes(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApReqCodeId.AndValue]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Event.NonEmptyCustomTextMap]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApplicableTagId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Text.GenericReqTitle.NonEmptyText]]
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

  def apply(codes: NonEmptySet[ApReqCodeId.AndValue], customText: Event.NonEmptyCustomTextMap, impSrcs: NonEmptySet[ReqId], impTgts: NonEmptySet[ReqId], tags: NonEmptySet[ApplicableTagId], title: Text.GenericReqTitle.NonEmptyText): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForCodes(codes) + ValueForCustomText(customText) + ValueForImpSrcs(impSrcs) + ValueForImpTgts(impTgts) + ValueForTags(tags) + ValueForTitle(title))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object SavedViewGD extends GenericData {
  import shipreq.webapp.base.filter.Filter.{Valid => ValidFilter}
  import shipreq.webapp.base.data.savedview._

  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Columns extends Attr {
    override type Data = NonEmptyVector[Column]
    override def apply(data: Data) = ValueForColumns(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptyVector[Column]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[ValidFilter]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[FilterDead]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[SavedView.Name]]
  }
  final case class ValueForName(value: Name.Data) extends Value {
    override val attr: Name.type = Name
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Order extends Attr {
    override type Data = SortCriteria
    override def apply(data: Data) = ValueForOrder(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[SortCriteria]]
  }
  final case class ValueForOrder(value: Order.Data) extends Value {
    override val attr: Order.type = Order
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForOrder => Order.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Columns, Filter, FilterDead, Name, Order))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Columns, Filter, FilterDead, Name, Order)

  def apply(name: SavedView.Name, filterDead: FilterDead, columns: NonEmptyVector[Column], order: SortCriteria, filter: Option[ValidFilter]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForName(name) + ValueForFilterDead(filterDead) + ValueForColumns(columns) + ValueForOrder(order) + ValueForFilter(filter))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object TagGroupGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object Children extends Attr {
    override type Data = TagInTree.Children
    override def apply(data: Data) = ValueForChildren(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Children]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
  }
  final case class ValueForDesc(value: Desc.Data) extends Value {
    override val attr: Desc.type = Desc
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForDesc => Desc.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Exclusivity extends Attr {
    override type Data = Exclusivity
    override def apply(data: Data) = ValueForExclusivity(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[Exclusivity]]
  }
  final case class ValueForExclusivity(value: Exclusivity.Data) extends Value {
    override val attr: Exclusivity.type = Exclusivity
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForExclusivity => Exclusivity.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  case object Name extends Attr {
    override type Data = String
    override def apply(data: Data) = ValueForName(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[String]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Parents]]
  }
  final case class ValueForParents(value: Parents.Data) extends Value {
    override val attr: Parents.type = Parents
    override def equals(o: Any): Boolean = o match {
      case v2: ValueForParents => Parents.dataEquality.equal(value, v2.value)
      case _ => false
    }
  }

  override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
    Util.univEqAndArbitraryOrder(Vector(Children, Desc, Exclusivity, Name, Parents))

  @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

  override val attrs = NonEmptySet[Attr](Children, Desc, Exclusivity, Name, Parents)

  def apply(name: String, desc: Option[String], exclusivity: Exclusivity, parents: TagInTree.Parents, children: TagInTree.Children): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForName(name) + ValueForDesc(desc) + ValueForExclusivity(exclusivity) + ValueForParents(parents) + ValueForChildren(children))
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
    override type Data = NonEmptySet[ApReqCodeId.AndValue]
    override def apply(data: Data) = ValueForCodes(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApReqCodeId.AndValue]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Event.NonEmptyCustomTextMap]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ReqId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[NonEmptySet[ApplicableTagId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Text.UseCaseTitle.NonEmptyText]]
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

  def apply(codes: NonEmptySet[ApReqCodeId.AndValue], customText: Event.NonEmptyCustomTextMap, impSrcs: NonEmptySet[ReqId], impTgts: NonEmptySet[ReqId], tags: NonEmptySet[ApplicableTagId], title: Text.UseCaseTitle.NonEmptyText): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForCodes(codes) + ValueForCustomText(customText) + ValueForImpSrcs(impSrcs) + ValueForImpTgts(impTgts) + ValueForTags(tags) + ValueForTitle(title))
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object UseCaseStepGD extends GenericData {
  sealed abstract class Attr extends AttrBase
  sealed abstract class Value extends ValueBase

  case object FlowIn extends Attr {
    override type Data = SetDiff.NE[UseCaseStepId]
    override def apply(data: Data) = ValueForFlowIn(data)
    override val dataEquality: Equal[Data] = implicitly[Equal[SetDiff.NE[UseCaseStepId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[SetDiff.NE[UseCaseStepId]]]
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
    override val dataEquality: Equal[Data] = implicitly[Equal[Text.UseCaseStep.OptionalText]]
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

  def apply(title: Text.UseCaseStep.OptionalText, flowIn: SetDiff.NE[UseCaseStepId], flowOut: SetDiff.NE[UseCaseStepId]): NonEmptyValues =
    NonEmpty.force(emptyValues + ValueForTitle(title) + ValueForFlowIn(flowIn) + ValueForFlowOut(flowOut))
}